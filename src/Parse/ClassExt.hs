module Parse.ClassExt
  ( findEntityDefForClassMethod
  ) where

import Types
import Language.Haskell.Exts
import Parse.Utils
import Data.Maybe (mapMaybe)
import Data.HashMap.Strict ((!), (!?))
import Data.List (find)

findEntityDefForClassMethod :: Payload -> QName SrcSpanInfo -> Maybe Entity
findEntityDefForClassMethod payload@Payload {..} (Qual _ (ModuleName _ alias') cmNameT) =
  let cmName = getName cmNameT
      moduleM = headMaybe . mapMaybe (lookForClassMethod payload cmName . (_modulesMap !) . _moduleImport)
        . filter (checkImportForClassMethod payload (Just alias') cmName) $ _importsPayload
  in moduleM >>= \ moduleT ->
     find (cMethodExist cmName) (_classes moduleT) >>=
     Just . mkEntity (_nameModuleT moduleT) cmName . _nameClassDesc
findEntityDefForClassMethod payload@Payload {..} (UnQual _ cmNameT) =
  let cmName = getName cmNameT
      moduleM = headMaybe . mapMaybe (lookForClassMethod payload cmName) . ((_modulesMap ! _modName):)
        . map ((_modulesMap !) . _moduleImport) . filter (checkImportForClassMethod payload Nothing cmName) $ _importsPayload
  in moduleM >>= \ moduleT ->
     find (cMethodExist cmName) (_classes moduleT) >>=
     Just . mkEntity (_nameModuleT moduleT) cmName . _nameClassDesc
findEntityDefForClassMethod _ (Special _ _) = Nothing

mkEntity :: String -> String -> String -> Entity
mkEntity modName cmName cName = InstanceMethod $ InstanceMethodDef modName cName cmName

checkImportForClassMethod :: Payload -> Maybe String -> String -> Import -> Bool
checkImportForClassMethod Payload {..} mAlias typeN Import {..} = maybe (not _qualified) ((_alias ==) . Just) mAlias
  && matchSpecsForClassMethod (_modulesMap ! _moduleImport) _specsList typeN

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

lookForClassMethod :: Payload -> String -> ModuleT -> Maybe ModuleT
lookForClassMethod payload@Payload {..} cMethodN moduleT =
  let modName'
        | checkForClassMethod cMethodN moduleT = Just $ _nameModuleT moduleT
        | otherwise = lookForClassMethodInExport payload moduleT cMethodN (_exports moduleT)
  in modName' >>= (_modulesMap !?)

lookForClassMethodInExport :: Payload -> ModuleT -> String -> ExportList -> Maybe String
lookForClassMethodInExport _ _ _ AllE = Nothing
lookForClassMethodInExport payload moduleT cMethodN (SomeE expList) = headMaybe . mapMaybe (lookForClassMethodInExport' payload moduleT cMethodN) $ expList

lookForClassMethodInExport' :: Payload -> ModuleT -> String -> ExportItem -> Maybe String
lookForClassMethodInExport' payload@Payload {..} moduleT cMethodN ExportType {_qualifier} =
  let imports' = filter (checkImportForClassMethod payload _qualifier cMethodN) (_importsModuleT moduleT)
  in fmap _nameModuleT $ headMaybe $ mapMaybe (lookForClassMethod payload cMethodN . (_modulesMap !) . _moduleImport) imports'
lookForClassMethodInExport' payload@Payload {..} moduleT cMethodN ExportModule {_nameExportItem}
  | _nameExportItem /= _nameModuleT moduleT =
    case _modulesMap !? _nameExportItem of
      Just moduleT' -> _nameModuleT <$> lookForClassMethod payload cMethodN moduleT'
      Nothing ->
        let imports' = filter (checkImportForClassMethod payload (Just _nameExportItem) cMethodN) (_importsModuleT moduleT)
        in fmap _nameModuleT $ headMaybe $ mapMaybe (lookForClassMethod payload cMethodN . (_modulesMap !) . _moduleImport) imports'
  | otherwise = Nothing
lookForClassMethodInExport' _ _ _ ExportVar {} = Nothing
