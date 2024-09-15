module Parse.Class
  ( mkClassInstanceR
  , classOInstance
  , collectClassInstances
  ) where

import Types.Mod
import Language.Haskell.Exts
import Parse.Utils
import Parse.Variable (mkVar)
import Data.HashMap.Strict ((!), (!?))
import Data.Maybe ( mapMaybe, isJust )
import qualified Data.HashMap.Strict as HM
import Data.Foldable (foldr')
import qualified Data.HashSet as HS
import Types.Class (NameLens(name_), Merge ((<:>)))

collectClassInstances :: Map String ModuleT -> [ModuleT] -> [ModuleT]
collectClassInstances modulesMap modules =
  let conn = foldr' (scanModule modulesMap) (HM.empty, HM.empty) modules
  in map (connectClassInstances conn) modules

connectClassInstances :: ConnMap -> ModuleT -> ModuleT
connectClassInstances conn moduleT =
  let classes = map (updateClass conn (_nameModuleT moduleT)) $ _classes moduleT
      instances = filter (isJust . _moduleInstanceDesc) . map (updateInstance conn (_nameModuleT moduleT)) $ _instancesModuleT moduleT
  in moduleT { _classes = classes, _instancesModuleT = instances }

updateClass :: ConnMap -> String -> ClassDesc -> ClassDesc
updateClass (classToIns, _) modName classI =
  let key = EntityDef modName (_nameClassDesc classI)
  in maybe classI (\ list -> classI { _instancesClassDesc = list }) $ classToIns !? key

updateInstance :: ConnMap -> String -> InstanceDesc -> InstanceDesc
updateInstance (_, insToClass) modName instanceI =
  let key = InstanceDef modName (_headInstanceDesc instanceI)
  in maybe (instanceI { _moduleInstanceDesc = Nothing }) (\ EntityDef { _moduleEntityDef } -> instanceI { _moduleInstanceDesc = Just _moduleEntityDef }) $ insToClass !? key

scanModule :: Map String ModuleT -> ModuleT -> ConnMap -> ConnMap
scanModule modulesMap moduleT conn =
  let payload = Payload
        { _modulesMap = modulesMap
        , _importsPayload = _importsModuleT moduleT
        , _modName = _nameModuleT moduleT
        , _prefix = []
        , _localBindings = HS.empty
        }
  in foldr' (scanInstance payload) conn $ _instancesModuleT moduleT

scanInstance :: Payload -> InstanceDesc -> ConnMap -> ConnMap
scanInstance payload InstanceDesc {..} conn@(classToIns, insToClass) =
  case findEntityDefForClass payload (_moduleInstanceDesc, _classInstanceDesc) of
    Just classEntity ->
      let instanceDef = InstanceDef (_modName payload) _headInstanceDesc
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
        { _nameClassDesc = name'
        , _locationClassDesc = mkRange srcInfo
        , _stringifiedClassDesc = prettyPrint decl
        , _methodsClassDesc = maybe [] (foldr mkVarClassDescR []) mClassDecls
        , _instancesClassDesc = []
        }
  in Just . ClassE $ classDesc
mkClassInstance decl@(InstDecl srcInfo _ instRule mInstDecl) =
  let (qualifier, className) = getClassInstRule instRule
      instanceDesc = InstanceDesc
        { _classInstanceDesc = className
        , _moduleInstanceDesc = qualifier
        , _headInstanceDesc = prettyPrint instRule
        , _locationInstanceDesc = mkRange srcInfo
        , _stringifiedInstanceDesc = prettyPrint decl
        , _methodsInstanceDesc = maybe [] (foldr mkVarInstanceDescR []) mInstDecl
        }
  in Just . InstanceE $ instanceDesc
mkClassInstance _ = Nothing

mkVarClassDescR :: ClassDecl Src -> [VarDesc] -> [VarDesc]
mkVarClassDescR decl res = maybe res (<:> res) $ mkVarClassDesc decl

mkVarClassDesc :: ClassDecl Src -> Maybe VarDesc
mkVarClassDesc (ClsDecl _ decl) = mkVar [] decl
mkVarClassDesc classDecl@(ClsDefSig srcInfo name' _) =
  let desc = VarDesc
        { _nameVarDesc = getName name'
        , _locationVarDesc = mkRange srcInfo
        , _code = prettyPrint classDecl
        , _dependencies = []
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
  let moduleM = headMaybe . mapMaybe (lookForClass payload cName . (_modulesMap !) . _moduleImport) . filter (checkImportForClass (Just alias') cName) $ _importsPayload
  in (\ ModuleT {_nameModuleT} -> EntityDef _nameModuleT cName) <$> moduleM
findEntityDefForClass payload@Payload {..} (Nothing, cName) =
  let moduleM = headMaybe . mapMaybe (lookForClass payload cName) . ((_modulesMap ! _modName):) . map ((_modulesMap !) . _moduleImport) . filter (checkImportForClass Nothing cName) $ _importsPayload
  in (\ ModuleT {_nameModuleT} -> EntityDef _nameModuleT cName) <$> moduleM

checkImportForClass :: Maybe String -> String -> Import -> Bool
checkImportForClass mAlias classN Import {..} = maybe (not _qualified) ((_alias ==) . Just) mAlias && matchSpecsForClass _specsList classN

matchSpecsForClass :: SpecsList -> String -> Bool
matchSpecsForClass (Include list) classN = any ((classN ==) . name_) list
matchSpecsForClass (Hide list) classN = all ((classN /=) . name_) list

lookForClass :: Payload -> String -> ModuleT -> Maybe ModuleT
lookForClass payload@Payload {..} classN moduleT =
  let modName' =
        if checkForClass classN moduleT then Just $ _nameModuleT moduleT
        else lookForClassInExport payload moduleT classN (_exports moduleT)
  in modName' >>= (_modulesMap !?)

lookForClassInExport :: Payload -> ModuleT -> String -> ExportList -> Maybe String
lookForClassInExport _ _ _ AllE = Nothing
lookForClassInExport payload moduleT classN (SomeE expList) = headMaybe . mapMaybe (lookForClassInExport' payload moduleT classN) $ expList

lookForClassInExport' :: Payload -> ModuleT -> String -> ExportItem -> Maybe String
lookForClassInExport' payload@Payload {..} moduleT classN ExportType {..}
  | _nameExportItem == classN =
    let imports' = filter (checkImportForClass _qualifier classN) (_importsModuleT moduleT)
    in fmap _nameModuleT $ headMaybe $ mapMaybe (lookForClass payload classN . (_modulesMap !) . _moduleImport) imports'
  | otherwise = Nothing
lookForClassInExport' payload@Payload {..} moduleT classN ExportModule {_nameExportItem}
  | _nameExportItem /= _nameModuleT moduleT =
    case _modulesMap !? _nameExportItem of
      Just moduleT' -> _nameModuleT <$> lookForClass payload classN moduleT'
      Nothing ->
        let imports' = filter (checkImportForClass (Just _nameExportItem) classN) (_importsModuleT moduleT)
        in fmap _nameModuleT $ headMaybe $ mapMaybe (lookForClass payload classN . (_modulesMap !) . _moduleImport) imports'
  | otherwise = Nothing
lookForClassInExport' _ _ _ ExportVar {} = Nothing
