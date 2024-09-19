module Parse.Type
  ( mkTypeR
  , findEntityDefForType
  , findEntityDefForCons
  ) where

import Types.Mod
import Language.Haskell.Exts
import Parse.Utils
import Data.Maybe ( mapMaybe, fromMaybe )
import Data.HashMap.Strict ((!))
import Data.Foldable (find)
import Types.Class (NameLens(name_))

mkTypeR :: Decl Src -> [TypeDesc] -> [TypeDesc]
mkTypeR decl res = maybe res (:res) $ mkType decl

mkType :: Decl SrcSpanInfo -> Maybe TypeDesc
mkType decl@(DataDecl srcInfo _ _ declHead consDecl _) =
  let constructors' = map mkConstructor consDecl
      name' = searchNameDecl declHead
      typeInfo = DataDesc
        { _nameDataDesc = name'
        , _locationDataDesc = mkRange srcInfo
        , _stringifiedDataDesc = prettyPrint decl
        , _constructorsDataDesc = constructors'
        }
  in Just . DataT $ typeInfo
mkType decl@(TypeDecl srcInfo declHead _) =
  let name' = searchNameDecl declHead
      typeInfo = TypeSynDesc
        { _nameTypeSynDesc = name'
        , _locationTypeSynDesc = mkRange srcInfo
        , _stringifiedTypeSynDesc = prettyPrint decl
        }
  in Just . TypeSynT $ typeInfo
mkType decl@(GDataDecl srcInfo _ _ declHead _ mConsDecl _) =
  let constructors' = map mkConstructorGadt mConsDecl
      name' = searchNameDecl declHead
      typeInfo = DataDesc
        { _nameDataDesc = name'
        , _locationDataDesc = mkRange srcInfo
        , _stringifiedDataDesc = prettyPrint decl
        , _constructorsDataDesc = constructors'
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

findEntityDefForType :: Payload -> QName SrcSpanInfo -> Maybe EntityDef
findEntityDefForType payload@Payload {..} (Qual _ (ModuleName _ alias') tNameT) =
  let tName = getName tNameT
      moduleM = headMaybe $ mapMaybe (\ Import {_repositoryImport, _moduleImport} ->
        lookForType payload tName _repositoryImport (_reposMap ! _repositoryImport ! _moduleImport))
        (filter (checkImportForType (Just alias') tName) _importsPayload)
  in mkEntityDef tName <$> moduleM
findEntityDefForType payload@Payload {..} (UnQual _ tNameT) =
  let tName = getName tNameT
      moduleM = headMaybe $ mapMaybe (\ Import {_repositoryImport, _moduleImport} ->
        lookForType payload tName _repositoryImport (_reposMap ! _repositoryImport ! _moduleImport))
        (Import _repositoryPayload _modName False Nothing (Hide []) : filter (checkImportForType Nothing tName) _importsPayload)
  in mkEntityDef tName <$> moduleM
findEntityDefForType _ (Special _ _) = Nothing

mkEntityDef :: String -> (String, ModuleT) -> EntityDef
mkEntityDef tName (repoName, moduleT) = EntityDef repoName (_nameModuleT moduleT) tName

checkImportForType :: Maybe String -> String -> Import -> Bool
checkImportForType mAlias typeN Import {..} = maybe (not _qualified) ((_alias ==) . Just) mAlias && matchSpecsForType _specsList typeN

matchSpecsForType :: SpecsList -> String -> Bool
matchSpecsForType (Include list) typeN = any ((typeN ==) . name_) list
matchSpecsForType (Hide list) typeN = all ((typeN /=) . name_) list

lookForType :: Payload -> String -> String -> ModuleT -> Maybe (String, ModuleT)
lookForType payload@Payload {..} typeN repoName moduleT =
  let modWithRepo
        | checkForType typeN moduleT = Just (repoName, _nameModuleT moduleT)
        | not $ checkForClass typeN moduleT = lookForTypeInExport payload moduleT typeN (_exports moduleT)
        | otherwise = Nothing
  in modWithRepo >>= \ (repoName', modName') -> return (repoName', _reposMap ! repoName' ! modName')

lookForTypeInExport :: Payload -> ModuleT -> String -> ExportList -> Maybe (String, String)
lookForTypeInExport _ _ _ AllE = Nothing
lookForTypeInExport payload moduleT typeN (SomeE expList) = headMaybe . mapMaybe (lookForTypeInExport' payload moduleT typeN) $ expList

lookForTypeInExport' :: Payload -> ModuleT -> String -> ExportItem -> Maybe (String, String)
lookForTypeInExport' payload@Payload {..} moduleT typeN ExportType {..}
  | _nameExportItem == typeN =
    let imports' = filter (checkImportForType _qualifier typeN) (_importsModuleT moduleT)
    in headMaybe $ mapMaybe (\ Import {_repositoryImport, _moduleImport} ->
      fmap _nameModuleT <$> lookForType payload typeN _repositoryImport (_reposMap ! _repositoryImport ! _moduleImport)) imports'
  | otherwise = Nothing
lookForTypeInExport' payload@Payload {..} moduleT typeN ExportModule {_nameExportItem}
  | _nameExportItem /= _nameModuleT moduleT =
    let imports' = filter (\ importT -> _nameExportItem == fromMaybe (_moduleImport importT) (_alias importT)
          && checkImportForType (_alias importT) typeN importT) (_importsModuleT moduleT)
    in headMaybe $ mapMaybe (\ Import {_repositoryImport, _moduleImport} ->
      fmap _nameModuleT <$> lookForType payload typeN _repositoryImport (_reposMap ! _repositoryImport ! _moduleImport)) imports'
  | otherwise = Nothing
lookForTypeInExport' _ _ _ ExportVar {} = Nothing

findEntityDefForCons :: Payload -> QName SrcSpanInfo -> Maybe EntityDef
findEntityDefForCons payload@Payload {..} (Qual _ (ModuleName _ alias') cNameT) =
  let cName = getName cNameT
      moduleM = headMaybe $ mapMaybe (\ Import {_moduleImport, _repositoryImport} ->
        lookForCons payload cName _repositoryImport (_reposMap ! _repositoryImport ! _moduleImport))
        (filter (checkImportForCons payload (Just alias') cName) _importsPayload)
  in moduleM >>= \ (repoName, moduleT) ->
     find (consExist cName) (_types moduleT) >>=
     Just . EntityDef repoName (_nameModuleT moduleT) . name_
findEntityDefForCons payload@Payload {..} (UnQual _ cNameT) =
  let cName = getName cNameT
      moduleM = headMaybe $ mapMaybe (\ Import {_moduleImport, _repositoryImport} ->
        lookForCons payload cName _repositoryImport (_reposMap ! _repositoryImport ! _moduleImport))
        (Import _repositoryPayload _modName False Nothing (Hide []) : filter (checkImportForCons payload Nothing cName) _importsPayload)
  in moduleM >>= \ (repoName, moduleT) ->
     find (consExist cName) (_types moduleT) >>=
     Just . EntityDef repoName (_nameModuleT moduleT) . name_
findEntityDefForCons _ (Special _ _) = Nothing

checkImportForCons :: Payload -> Maybe String -> String -> Import -> Bool
checkImportForCons Payload {..} mAlias typeN Import {..} = maybe (not _qualified) ((_alias ==) . Just) mAlias
  && matchSpecsForCons (_reposMap ! _repositoryImport ! _moduleImport) _specsList typeN

matchSpecsForCons :: ModuleT -> SpecsList -> String -> Bool
matchSpecsForCons moduleT (Include list) consN = any (consIncluded moduleT consN) list
matchSpecsForCons moduleT (Hide list) consN = not $ any (consIncluded moduleT consN) list

consIncluded :: ModuleT -> String -> ImportItem -> Bool
consIncluded moduleT consN (TypeImportItem typeImport) = consIncluded' moduleT consN typeImport
consIncluded _ _ _ = False

consIncluded' :: ModuleT -> String -> TypeImport -> Bool
consIncluded' moduleT consN (TypeImport name' All) =
  let typeT = find ((name' ==) . name_) (_types moduleT)
  in maybe False (consExist consN) typeT
consIncluded' _ consN (TypeImport _ (Some list)) = consN `elem` list
consIncluded' _ _ (TypeImport _ None) = False

lookForCons :: Payload -> String -> String -> ModuleT -> Maybe (String, ModuleT)
lookForCons payload@Payload {..} consN repoName moduleT =
  let modWithRepo =
        if checkForCons consN moduleT then Just (repoName, _nameModuleT moduleT)
        else lookForConsInExport payload moduleT consN (_exports moduleT)
  in modWithRepo >>= \ (repoName', modName') -> return (repoName', _reposMap ! repoName' ! modName')

lookForConsInExport :: Payload -> ModuleT -> String -> ExportList -> Maybe (String, String)
lookForConsInExport _ _ _ AllE = Nothing
lookForConsInExport payload moduleT consN (SomeE expList) = headMaybe . mapMaybe (lookForConsInExport' payload moduleT consN) $ expList

lookForConsInExport' :: Payload -> ModuleT -> String -> ExportItem -> Maybe (String, String)
lookForConsInExport' payload@Payload {..} moduleT consN ExportType {_qualifier} =
  let imports' = filter (checkImportForCons payload _qualifier consN) (_importsModuleT moduleT)
  in headMaybe $ mapMaybe (\ Import {_repositoryImport, _moduleImport} ->
    fmap _nameModuleT <$> lookForCons payload consN _repositoryImport (_reposMap ! _repositoryImport ! _moduleImport)) imports'
lookForConsInExport' payload@Payload {..} moduleT consN ExportModule {_nameExportItem}
  | _nameExportItem /= _nameModuleT moduleT =
    let imports' = filter (\ importT -> _nameExportItem == fromMaybe (_moduleImport importT) (_alias importT)
          && checkImportForCons payload (Just _nameExportItem) consN importT) (_importsModuleT moduleT)
    in headMaybe $ mapMaybe (\ Import {_repositoryImport, _moduleImport} -> fmap _nameModuleT <$>
       lookForCons payload consN _repositoryImport (_reposMap ! _repositoryImport ! _moduleImport)) imports'
  | otherwise = Nothing
lookForConsInExport' _ _ _ ExportVar {} = Nothing
