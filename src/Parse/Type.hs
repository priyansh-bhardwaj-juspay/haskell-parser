module Parse.Type
  ( mkTypeR
  , findEntityDefForType
  , findEntityDefForCons
  ) where

import Types
import Language.Haskell.Exts
import Parse.Utils
import Data.Maybe ( mapMaybe )
import Control.Lens ((^.))
import Data.HashMap.Strict ((!), (!?))
import Data.Foldable (find)
import GHC.Data.Maybe (isNothing)

mkTypeR :: Decl Src -> [TypeDesc] -> [TypeDesc]
mkTypeR decl res = maybe res (:res) $ mkType decl

mkType :: Decl SrcSpanInfo -> Maybe TypeDesc
mkType decl@(DataDecl srcInfo _ _ declHead consDecl _) =
  let constructors' = map mkConstructor consDecl
      name' = searchNameDecl declHead
      typeInfo = DataDesc
        { name = name'
        , location = mkRange srcInfo
        , stringified = prettyPrint decl
        , constructors = constructors'
        }
  in Just . DataT $ typeInfo
mkType decl@(TypeDecl srcInfo declHead _) =
  let name' = searchNameDecl declHead
      typeInfo = TypeSynDesc
        { name = name'
        , location = mkRange srcInfo
        , stringified = prettyPrint decl
        }
  in Just . TypeSynT $ typeInfo
mkType decl@(GDataDecl srcInfo _ _ declHead _ mConsDecl _) =
  let constructors' = map mkConstructorGadt mConsDecl
      name' = searchNameDecl declHead
      typeInfo = DataDesc
        { name = name'
        , location = mkRange srcInfo
        , stringified = prettyPrint decl
        , constructors = constructors'
        }
  in Just . DataT $ typeInfo
mkType _ = Nothing

mkConstructorGadt :: GadtDecl Src -> ConsDesc
mkConstructorGadt (GadtDecl _ name' _ _ (Just fieldDecls) _) = RecordCon (getName name') (mapMaybe getFieldName fieldDecls)
mkConstructorGadt (GadtDecl _ name' _ _ Nothing _) = OrdinaryCon (getName name')

mkConstructor :: QualConDecl Src -> ConsDesc
mkConstructor (QualConDecl _ _ _ (ConDecl _ name' _)) = OrdinaryCon (getName name')
mkConstructor (QualConDecl _ _ _ (InfixConDecl _ _ name' _)) = InfixCon (getName name')
mkConstructor (QualConDecl _ _ _ (RecDecl _ name' fieldDecls)) = RecordCon (getName name') (mapMaybe getFieldName fieldDecls)

getFieldName :: FieldDecl Src -> Maybe String
getFieldName (FieldDecl _ (name':_) _) = Just $ getName name'
getFieldName _ = Nothing

searchNameDecl :: DeclHead Src -> String
searchNameDecl (DHead _ name') = getName name'
searchNameDecl (DHApp _ declHead _) = searchNameDecl declHead
searchNameDecl _other = error $ "Unknown parseDataDecl : " <> show _other

findEntityDefForType :: Payload -> QName SrcSpanInfo -> Maybe EntityDef
findEntityDefForType payload@Payload {..} (Qual _ (ModuleName _ alias') tNameT) =
  let tName = getName tNameT
      moduleM = headMaybe . mapMaybe (lookForType payload tName . (modulesMap !) . (^. #_module)) . filter (checkImportForType (Just alias') tName) $ imports
  in (\ ModuleT {name = name'} -> EntityDef name' tName) <$> moduleM
findEntityDefForType payload@Payload {..} (UnQual _ tNameT) =
  let tName = getName tNameT
      moduleM = headMaybe . mapMaybe (lookForType payload tName) . ((modulesMap ! modName):) . map ((modulesMap !) . (^. #_module)) . filter (checkImportForType Nothing tName) $ imports
  in (\ ModuleT {name = name'} -> EntityDef name' tName) <$> moduleM
findEntityDefForType _ (Special _ _) = Nothing

checkImportForType :: Maybe String -> String -> Import -> Bool
checkImportForType mAlias typeN Import {..} = maybe (not qualified) ((alias ==) . Just) mAlias && matchSpecsForType specsList typeN

matchSpecsForType :: SpecsList -> String -> Bool
matchSpecsForType (Include list) typeN = any ((typeN ==) . (^. name_)) list
matchSpecsForType (Hide list) typeN = all ((typeN /=) . (^. name_)) list

lookForType :: Payload -> String -> ModuleT -> Maybe ModuleT
lookForType payload@Payload {..} typeN moduleT =
  let modName' =
        if checkForType typeN moduleT then Just $ moduleT ^. #name
        else lookForTypeInExport payload moduleT typeN (moduleT ^. #exports)
  in modName' >>= (modulesMap !?)

lookForTypeInExport :: Payload -> ModuleT -> String -> ExportList -> Maybe String
lookForTypeInExport _ _ _ AllE = Nothing
lookForTypeInExport payload moduleT typeN (SomeE expList) = headMaybe . mapMaybe (lookForTypeInExport' payload moduleT typeN) $ expList

lookForTypeInExport' :: Payload -> ModuleT -> String -> ExportItem -> Maybe String
lookForTypeInExport' payload@Payload {..} moduleT typeN ExportType {name = name', ..}
  | name' == typeN =
    let imports' = filter (checkImportForType qualifier typeN) (moduleT ^. #imports)
    in fmap (^. #name) $ headMaybe $ mapMaybe (lookForType payload typeN . (modulesMap !) . (^. #_module)) imports'
  | otherwise = Nothing
lookForTypeInExport' payload@Payload {..} moduleT typeN ExportModule {name = name'}
  | name' /= moduleT ^. #name =
    case modulesMap !? name' of
      Just moduleT' -> (^. #name) <$> lookForType payload typeN moduleT'
      Nothing ->
        let imports' = filter (checkImportForType (Just name') typeN) (moduleT ^. #imports)
        in fmap (^. #name) $ headMaybe $ mapMaybe (lookForType payload typeN . (modulesMap !) . (^. #_module)) imports'
  | otherwise = Nothing
lookForTypeInExport' _ _ _ ExportVar {} = Nothing

checkForType :: String -> ModuleT -> Bool
checkForType typeN moduleT = checkExportListForType (moduleT ^. #exports) typeN && any ((typeN ==) . (^. name_)) (moduleT ^. #types)

checkExportListForType :: ExportList -> String -> Bool
checkExportListForType AllE _ = True
checkExportListForType (SomeE items) typeN = any (matchExportItemForType typeN) items

matchExportItemForType :: String -> ExportItem -> Bool
matchExportItemForType typeN (ExportType {name = name', ..}) = isNothing qualifier && name' == typeN
matchExportItemForType _ (ExportModule {}) = False
matchExportItemForType _ (ExportVar {}) = False

findEntityDefForCons :: Payload -> QName SrcSpanInfo -> Maybe EntityDef
findEntityDefForCons payload@Payload {..} (Qual _ (ModuleName _ alias') cNameT) =
  let cName = getName cNameT
      moduleM = headMaybe . mapMaybe (lookForCons payload cName . (modulesMap !) . (^. #_module)) . filter (checkImportForCons payload (Just alias') cName) $ imports
  in moduleM >>= \ moduleT ->
     find (consExist cName) (moduleT ^. #types) >>=
     Just . EntityDef (moduleT ^. #name) . (^. name_)
findEntityDefForCons payload@Payload {..} (UnQual _ cNameT) =
  let cName = getName cNameT
      moduleM = headMaybe . mapMaybe (lookForCons payload cName) . ((modulesMap ! modName):) . map ((modulesMap !) . (^. #_module)) . filter (checkImportForCons payload Nothing cName) $ imports
  in moduleM >>= \ moduleT ->
     find (consExist cName) (moduleT ^. #types) >>=
     Just . EntityDef (moduleT ^. #name) . (^. name_)
findEntityDefForCons _ (Special _ _) = Nothing

consExist :: String -> TypeDesc -> Bool
consExist cName (DataT dataT) = elem cName . map (^. name_) $ dataT ^. #constructors
consExist _ (TypeSynT _) = False -- TODO: Redirection to main type can be done, skipping for now
consExist cName (GadtT gadtT) = elem cName . map (^. name_) $ gadtT ^. #constructors

checkImportForCons :: Payload -> Maybe String -> String -> Import -> Bool
checkImportForCons Payload {..} mAlias typeN Import {..} = maybe (not qualified) ((alias ==) . Just) mAlias && matchSpecsForCons (modulesMap ! _module) specsList typeN

matchSpecsForCons :: ModuleT -> SpecsList -> String -> Bool
matchSpecsForCons moduleT (Include list) consN = any (consIncluded moduleT consN) list
matchSpecsForCons moduleT (Hide list) consN = not $ any (consIncluded moduleT consN) list

consIncluded :: ModuleT -> String -> ImportItem -> Bool
consIncluded moduleT consN (TypeImportItem typeImport) = consIncluded' moduleT consN typeImport
consIncluded _ _ _ = False

consIncluded' :: ModuleT -> String -> TypeImport -> Bool
consIncluded' moduleT consN (TypeImport name' All) =
  let typeT = find ((name' ==) . (^. name_)) (moduleT ^. #types)
  in maybe False (consExist consN) typeT
consIncluded' _ consN (TypeImport _ (Some list)) = consN `elem` list
consIncluded' _ _ (TypeImport _ None) = False

lookForCons :: Payload -> String -> ModuleT -> Maybe ModuleT
lookForCons payload@Payload {..} consN moduleT =
  let modName' =
        if checkForCons consN moduleT then Just $ moduleT ^. #name
        else lookForConsInExport payload moduleT consN (moduleT ^. #exports)
  in modName' >>= (modulesMap !?)

lookForConsInExport :: Payload -> ModuleT -> String -> ExportList -> Maybe String
lookForConsInExport _ _ _ AllE = Nothing
lookForConsInExport payload moduleT consN (SomeE expList) = headMaybe . mapMaybe (lookForConsInExport' payload moduleT consN) $ expList

lookForConsInExport' :: Payload -> ModuleT -> String -> ExportItem -> Maybe String
lookForConsInExport' payload@Payload {..} moduleT consN ExportType {qualifier = qualifier} =
  let imports' = filter (checkImportForCons payload qualifier consN) (moduleT ^. #imports)
  in fmap (^. #name) $ headMaybe $ mapMaybe (lookForCons payload consN . (modulesMap !) . (^. #_module)) imports'
lookForConsInExport' payload@Payload {..} moduleT consN ExportModule {name = name'}
  | name' /= moduleT ^. #name =
    case modulesMap !? name' of
      Just moduleT' -> (^. #name) <$> lookForCons payload consN moduleT'
      Nothing ->
        let imports' = filter (checkImportForCons payload (Just name') consN) (moduleT ^. #imports)
        in fmap (^. #name) $ headMaybe $ mapMaybe (lookForCons payload consN . (modulesMap !) . (^. #_module)) imports'
  | otherwise = Nothing
lookForConsInExport' _ _ _ ExportVar {} = Nothing

checkForCons :: String -> ModuleT -> Bool
checkForCons consN moduleT = checkExportListForCons (moduleT ^. #exports) consN moduleT && any (consExist consN) (moduleT ^. #types)

checkExportListForCons :: ExportList -> String -> ModuleT -> Bool
checkExportListForCons AllE _ _ = True
checkExportListForCons (SomeE items) consN moduleT = any (matchExportItemForCons consN moduleT) items

matchExportItemForCons :: String -> ModuleT -> ExportItem -> Bool
matchExportItemForCons consN moduleT (ExportType name' Nothing All) =
  let typeT = find ((name' ==) . (^. name_)) (moduleT ^. #types)
  in maybe False (consExist consN) typeT
matchExportItemForCons _ _ (ExportType _ (Just _) _) = False
matchExportItemForCons consN _ (ExportType _ qualifier (Some list)) = isNothing qualifier && consN `elem` list
matchExportItemForCons _ _ (ExportType _ _ None) = False
matchExportItemForCons _ _ (ExportVar {}) = False
matchExportItemForCons _ _ (ExportModule {}) = False
