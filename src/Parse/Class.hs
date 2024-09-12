module Parse.Class
  ( mkClassInstanceR
  , classOInstance
  , collectClassInstances
  ) where

import Types
import Language.Haskell.Exts
import Parse.Utils
import Parse.Variable (mkVar)
import Data.HashMap.Strict ((!), (!?))
import Control.Lens ((^.), (&), (.~), (?~))
import Data.Maybe ( mapMaybe, isJust, isNothing )
import qualified Data.HashMap.Strict as HM
import Data.Foldable (foldr')
import qualified Data.HashSet as HS

collectClassInstances :: Map String ModuleT -> [ModuleT] -> [ModuleT]
collectClassInstances modulesMap modules =
  let conn = foldr' (scanModule modulesMap) (HM.empty, HM.empty) modules
  in map (connectClassInstances conn) modules

connectClassInstances :: ConnMap -> ModuleT -> ModuleT
connectClassInstances conn moduleT =
  let classes = map (updateClass conn (moduleT ^. #name)) $ moduleT ^. #classes
      instances = filter (isJust . (^. #_module)) . map (updateInstance conn (moduleT ^. #name)) $ moduleT ^. #instances
  in moduleT & #classes .~ classes & #instances .~ instances

updateClass :: ConnMap -> String -> ClassDesc -> ClassDesc
updateClass (classToIns, _) modName classI =
  let key = EntityDef modName (classI ^. #name)
  in maybe classI (\ list -> classI & #instances .~ list) $ classToIns !? key

updateInstance :: ConnMap -> String -> InstanceDesc -> InstanceDesc
updateInstance (_, insToClass) modName instanceI =
  let key = InstanceDef modName (instanceI ^. #head)
  in maybe (instanceI & #_module .~ Nothing) (\ EntityDef { _module } -> instanceI & #_module ?~ _module) $ insToClass !? key

scanModule :: Map String ModuleT -> ModuleT -> ConnMap -> ConnMap
scanModule modulesMap moduleT conn =
  let payload = Payload
        { modulesMap = modulesMap
        , imports = moduleT ^. #imports
        , modName = moduleT ^. #name
        , prefix = []
        , localBindings = HS.empty
        }
  in foldr' (scanInstance payload) conn $ moduleT ^. #instances

scanInstance :: Payload -> InstanceDesc -> ConnMap -> ConnMap
scanInstance payload InstanceDesc {head = head', ..} conn@(classToIns, insToClass) =
  case findEntityDefForClass payload (_module, _class) of
    Just classEntity ->
      let instanceDef = InstanceDef (payload ^. #modName) head'
          classToInsList = maybe [instanceDef] (instanceDef:) $ HM.lookup classEntity classToIns
      in (HM.insert classEntity classToInsList classToIns, HM.insert instanceDef classEntity insToClass)
    Nothing -> conn

classOInstance :: [ClassOInstance] -> ([ClassDesc], [InstanceDesc])
classOInstance = foldr classOInstanceAux ([], [])
  where
  classOInstanceAux :: ClassOInstance -> ([ClassDesc], [InstanceDesc]) -> ([ClassDesc], [InstanceDesc])
  classOInstanceAux (ClassE classE) (cls, ins) = (classE : cls, ins)
  classOInstanceAux (InstanceE instanceE) (cls, ins) = (cls, instanceE : ins)

mkClassInstanceR :: Decl Src -> [ClassOInstance] -> [ClassOInstance]
mkClassInstanceR decl res = maybe res (:res) $ mkClassInstance decl

mkClassInstance :: Decl SrcSpanInfo -> Maybe ClassOInstance
mkClassInstance decl@(ClassDecl srcInfo _ declHead _ mClassDecls) =
  let name' = searchNameDecl declHead
      classDesc = ClassDesc
        { name = name'
        , location = mkRange srcInfo
        , stringified = prettyPrint decl
        , methods = maybe [] (foldr mkVarClassDescR []) mClassDecls
        , instances = []
        }
  in Just . ClassE $ classDesc
mkClassInstance decl@(InstDecl srcInfo _ instRule mInstDecl) =
  let (qualifier, className) = getClassInstRule instRule
      instanceDesc = InstanceDesc
        { _class = className
        , _module = qualifier
        , head = prettyPrint instRule
        , location = mkRange srcInfo
        , stringified = prettyPrint decl
        , methods = maybe [] (foldr mkVarInstanceDescR []) mInstDecl
        }
  in Just . InstanceE $ instanceDesc
mkClassInstance _ = Nothing

mkVarClassDescR :: ClassDecl Src -> [VarDesc] -> [VarDesc]
mkVarClassDescR decl res = maybe res (<:> res) $ mkVarClassDesc decl

mkVarClassDesc :: ClassDecl Src -> Maybe VarDesc
mkVarClassDesc (ClsDecl _ decl) = mkVar [] decl
mkVarClassDesc classDecl@(ClsDefSig srcInfo name' _) =
  let desc = VarDesc
        { name = getName name'
        , location = mkRange srcInfo
        , code = prettyPrint classDecl
        , dependencies = []
        }
  in Just desc
mkVarClassDesc _ = Nothing

getClassInstRule :: InstRule Src -> (Maybe String, String)
getClassInstRule (IRule _ _ _ instHead) = getClassInstHead instHead
getClassInstRule (IParen _ instRule) = getClassInstRule instRule

getClassInstHead :: InstHead Src -> (Maybe String, String)
getClassInstHead (IHCon _ qName) = openQName qName
getClassInstHead (IHInfix _ _ qName) = openQName qName
getClassInstHead (IHParen _ instHead) = getClassInstHead instHead
getClassInstHead (IHApp _ instHead _) = getClassInstHead instHead

mkVarInstanceDescR :: InstDecl Src -> [VarDesc] -> [VarDesc]
mkVarInstanceDescR decl res = maybe res (<:> res) $ mkVarInstanceDesc decl

mkVarInstanceDesc :: InstDecl Src -> Maybe VarDesc
mkVarInstanceDesc (InsDecl _ decl) = mkVar [] decl
mkVarInstanceDesc _ = Nothing

findEntityDefForClass :: Payload -> (Maybe String, String) -> Maybe EntityDef
findEntityDefForClass payload@Payload {..} (Just alias', cName) =
  let moduleM = headMaybe . mapMaybe (lookForClass payload cName . (modulesMap !) . (^. #_module)) . filter (checkImportForClass (Just alias') cName) $ imports
  in (\ ModuleT {name = name'} -> EntityDef name' cName) <$> moduleM
findEntityDefForClass payload@Payload {..} (Nothing, cName) =
  let moduleM = headMaybe . mapMaybe (lookForClass payload cName) . ((modulesMap ! modName):) . map ((modulesMap !) . (^. #_module)) . filter (checkImportForClass Nothing cName) $ imports
  in (\ ModuleT {name = name'} -> EntityDef name' cName) <$> moduleM

checkImportForClass :: Maybe String -> String -> Import -> Bool
checkImportForClass mAlias classN Import {..} = maybe (not qualified) ((alias ==) . Just) mAlias && matchSpecsForClass specsList classN

matchSpecsForClass :: SpecsList -> String -> Bool
matchSpecsForClass (Include list) classN = any ((classN ==) . (^. name_)) list
matchSpecsForClass (Hide list) classN = all ((classN /=) . (^. name_)) list

lookForClass :: Payload -> String -> ModuleT -> Maybe ModuleT
lookForClass payload@Payload {..} classN moduleT =
  let modName' =
        if checkForClass classN moduleT then Just $ moduleT ^. #name
        else lookForClassInExport payload moduleT classN (moduleT ^. #exports)
  in modName' >>= (modulesMap !?)

lookForClassInExport :: Payload -> ModuleT -> String -> ExportList -> Maybe String
lookForClassInExport _ _ _ AllE = Nothing
lookForClassInExport payload moduleT classN (SomeE expList) = headMaybe . mapMaybe (lookForClassInExport' payload moduleT classN) $ expList

lookForClassInExport' :: Payload -> ModuleT -> String -> ExportItem -> Maybe String
lookForClassInExport' payload@Payload {..} moduleT classN ExportType {name = name', ..}
  | name' == classN =
    let imports' = filter (checkImportForClass qualifier classN) (moduleT ^. #imports)
    in fmap (^. #name) $ headMaybe $ mapMaybe (lookForClass payload classN . (modulesMap !) . (^. #_module)) imports'
  | otherwise = Nothing
lookForClassInExport' payload@Payload {..} moduleT classN ExportModule {name = name'}
  | name' /= moduleT ^. #name =
    case modulesMap !? name' of
      Just moduleT' -> (^. #name) <$> lookForClass payload classN moduleT'
      Nothing ->
        let imports' = filter (checkImportForClass (Just name') classN) (moduleT ^. #imports)
        in fmap (^. #name) $ headMaybe $ mapMaybe (lookForClass payload classN . (modulesMap !) . (^. #_module)) imports'
  | otherwise = Nothing
lookForClassInExport' _ _ _ ExportVar {} = Nothing

checkForClass :: String -> ModuleT -> Bool
checkForClass classN moduleT = checkExportListForClass (moduleT ^. #exports) classN && any ((classN ==) . (^. #name)) (moduleT ^. #classes)

checkExportListForClass :: ExportList -> String -> Bool
checkExportListForClass AllE _ = True
checkExportListForClass (SomeE items) classN = any (matchExportItemForClass classN) items

matchExportItemForClass :: String -> ExportItem -> Bool
matchExportItemForClass classN (ExportType {name = name', ..}) = isNothing qualifier && name' == classN
matchExportItemForClass _ (ExportModule {}) = False
matchExportItemForClass _ (ExportVar {}) = False
