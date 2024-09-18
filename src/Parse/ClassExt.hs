module Parse.ClassExt
  ( findEntityDefForClassMethod
  ) where

import Types.Mod
import Language.Haskell.Exts
import Parse.Utils
import Data.Maybe (mapMaybe)
import Data.HashMap.Strict ((!))
import Data.List (find)
import qualified Data.HashMap.Strict as HM

findEntityDefForClassMethod :: Payload -> QName SrcSpanInfo -> Maybe Entity
findEntityDefForClassMethod payload@Payload {..} (Qual _ (ModuleName _ alias') cmNameT) =
  let cmName = getName cmNameT
      moduleM = headMaybe $ mapMaybe (\ Import {_moduleImport} -> find (HM.member _moduleImport . _modulesMap) _repoModules
        >>= \ repo' -> lookForClassMethod payload cmName (_nameRepository' repo') (_modulesMap repo' ! _moduleImport))
        (filter (checkImportForClassMethod payload (Just alias') cmName) _importsPayload)
  in moduleM >>= \ (repoName, moduleT) ->
     find (cMethodExist cmName) (_classes moduleT) >>=
     Just . mkEntity repoName (_nameModuleT moduleT) cmName . _nameClassDesc
findEntityDefForClassMethod payload@Payload {..} (UnQual _ cmNameT) =
  let cmName = getName cmNameT
      moduleM = headMaybe $ mapMaybe (\ Import {_moduleImport} -> find (HM.member _moduleImport . _modulesMap) _repoModules
        >>= \ repo' -> lookForClassMethod payload cmName (_nameRepository' repo') (_modulesMap repo' ! _moduleImport))
        (Import _modName False Nothing (Hide []) : filter (checkImportForClassMethod payload Nothing cmName) _importsPayload)
  in moduleM >>= \ (repoName, moduleT) ->
     find (cMethodExist cmName) (_classes moduleT) >>=
     Just . mkEntity repoName (_nameModuleT moduleT) cmName . _nameClassDesc
findEntityDefForClassMethod _ (Special _ _) = Nothing

mkEntity :: String -> String -> String -> String -> Entity
mkEntity repoName modName cmName cName = InstanceMethod $ InstanceMethodDef repoName modName cName cmName

checkImportForClassMethod :: Payload -> Maybe String -> String -> Import -> Bool
checkImportForClassMethod Payload {..} mAlias typeN Import {..} = maybe (not _qualified) ((_alias ==) . Just) mAlias
  && matchSpecsForClassMethod ((! _moduleImport) . _modulesMap . head . filter (HM.member _moduleImport . _modulesMap) $ _repoModules) _specsList typeN

matchSpecsForClassMethod :: ModuleT -> SpecsList -> String -> Bool
matchSpecsForClassMethod moduleT (Include list) cMethodN = any (cMethodIncluded moduleT cMethodN) list
matchSpecsForClassMethod moduleT (Hide list) cMethodN = not $ any (cMethodIncluded moduleT cMethodN) list

cMethodIncluded :: ModuleT -> String -> ImportItem -> Bool
cMethodIncluded moduleT cMethodN (TypeImportItem typeImport) = cMethodIncluded' moduleT cMethodN typeImport
cMethodIncluded _ _ _ = False

cMethodIncluded' :: ModuleT -> String -> TypeImport -> Bool
cMethodIncluded' moduleT cMethodN (TypeImport name' All) =
  let classT = find ((name' ==) . _nameClassDesc) (_classes moduleT)
  in maybe False (cMethodExist cMethodN) classT
cMethodIncluded' _ cMethodN (TypeImport _ (Some list)) = cMethodN `elem` list
cMethodIncluded' _ _ (TypeImport _ None) = False

lookForClassMethod :: Payload -> String -> String -> ModuleT -> Maybe (String, ModuleT)
lookForClassMethod payload@Payload {..} cMethodN repoName moduleT =
  let modWithRepo
        | checkForClassMethod cMethodN moduleT = Just (repoName, _nameModuleT moduleT)
        | otherwise = lookForClassMethodInExport payload moduleT cMethodN (_exports moduleT)
  in modWithRepo >>= \ (repoName', modName') -> return (repoName', _reposMap ! repoName' ! modName')

lookForClassMethodInExport :: Payload -> ModuleT -> String -> ExportList -> Maybe (String, String)
lookForClassMethodInExport _ _ _ AllE = Nothing
lookForClassMethodInExport payload moduleT cMethodN (SomeE expList) = headMaybe . mapMaybe (lookForClassMethodInExport' payload moduleT cMethodN) $ expList

lookForClassMethodInExport' :: Payload -> ModuleT -> String -> ExportItem -> Maybe (String, String)
lookForClassMethodInExport' payload@Payload {..} moduleT cMethodN ExportType {_qualifier} =
  let imports' = filter (checkImportForClassMethod payload _qualifier cMethodN) (_importsModuleT moduleT)
  in headMaybe $ mapMaybe (\ Import {_moduleImport} -> find (HM.member _moduleImport . _modulesMap) _repoModules
    >>= \ repo' -> fmap _nameModuleT <$> lookForClassMethod payload cMethodN (_nameRepository' repo') (_modulesMap repo' ! _moduleImport)) imports'
lookForClassMethodInExport' payload@Payload {..} moduleT cMethodN ExportModule {_nameExportItem}
  | _nameExportItem /= _nameModuleT moduleT =
    case find (HM.member _nameExportItem . _modulesMap) _repoModules
      >>= \ RepoModuleMap {..} -> return (_nameRepository', _modulesMap ! _nameExportItem) of
      Just (repoName, moduleT') -> fmap _nameModuleT <$> lookForClassMethod payload cMethodN repoName moduleT'
      Nothing ->
        let imports' = filter (checkImportForClassMethod payload (Just _nameExportItem) cMethodN) (_importsModuleT moduleT)
        in headMaybe $ mapMaybe (\ Import {_moduleImport} -> find (HM.member _moduleImport . _modulesMap) _repoModules
          >>= \ repo' -> fmap _nameModuleT <$> lookForClassMethod payload cMethodN (_nameRepository' repo') (_modulesMap repo' ! _moduleImport)) imports'
  | otherwise = Nothing
lookForClassMethodInExport' _ _ _ ExportVar {} = Nothing
