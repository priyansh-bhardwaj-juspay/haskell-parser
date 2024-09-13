module Parse.ClassExt
  ( findEntityDefForClassMethod
  ) where

import Types
import Language.Haskell.Exts
import Parse.Utils
import Data.Maybe (mapMaybe)
import Data.HashMap.Strict ((!), (!?))
import Control.Lens ((^.))
import Data.List (find)

findEntityDefForClassMethod :: Payload -> QName SrcSpanInfo -> Maybe Entity
findEntityDefForClassMethod payload@Payload {..} (Qual _ (ModuleName _ alias') cmNameT) =
  let cmName = getName cmNameT
      moduleM = headMaybe . mapMaybe (lookForClassMethod payload cmName . (modulesMap !) . (^. #_module))
        . filter (checkImportForClassMethod payload (Just alias') cmName) $ imports
  in moduleM >>= \ moduleT ->
     find (cMethodExist cmName) (moduleT ^. #classes) >>=
     Just . mkEntity (moduleT ^. #name) cmName . (^. #name)
findEntityDefForClassMethod payload@Payload {..} (UnQual _ cmNameT) =
  let cmName = getName cmNameT
      moduleM = headMaybe . mapMaybe (lookForClassMethod payload cmName) . ((modulesMap ! modName):)
        . map ((modulesMap !) . (^. #_module)) . filter (checkImportForClassMethod payload Nothing cmName) $ imports
  in moduleM >>= \ moduleT ->
     find (cMethodExist cmName) (moduleT ^. #classes) >>=
     Just . mkEntity (moduleT ^. #name) cmName . (^. #name)
findEntityDefForClassMethod _ (Special _ _) = Nothing

mkEntity :: String -> String -> String -> Entity
mkEntity modName cmName cName = InstanceMethod $ InstanceMethodDef modName cName cmName

checkImportForClassMethod :: Payload -> Maybe String -> String -> Import -> Bool
checkImportForClassMethod Payload {..} mAlias typeN Import {..} = maybe (not qualified) ((alias ==) . Just) mAlias
  && matchSpecsForClassMethod (modulesMap ! _module) specsList typeN

matchSpecsForClassMethod :: ModuleT -> SpecsList -> String -> Bool
matchSpecsForClassMethod moduleT (Include list) cMethodN = any (cMethodIncluded moduleT cMethodN) list
matchSpecsForClassMethod moduleT (Hide list) cMethodN = not $ any (cMethodIncluded moduleT cMethodN) list

cMethodIncluded :: ModuleT -> String -> ImportItem -> Bool
cMethodIncluded moduleT cMethodN (TypeImportItem typeImport) = cMethodIncluded' moduleT cMethodN typeImport
cMethodIncluded _ _ _ = False

cMethodIncluded' :: ModuleT -> String -> TypeImport -> Bool
cMethodIncluded' moduleT cMethodN (TypeImport name' All) =
  let classT = find ((name' ==) . (^. #name)) (moduleT ^. #classes)
  in maybe False (cMethodExist cMethodN) classT
cMethodIncluded' _ cMethodN (TypeImport _ (Some list)) = cMethodN `elem` list
cMethodIncluded' _ _ (TypeImport _ None) = False

lookForClassMethod :: Payload -> String -> ModuleT -> Maybe ModuleT
lookForClassMethod payload@Payload {..} cMethodN moduleT =
  let modName'
        | checkForClassMethod cMethodN moduleT = Just $ moduleT ^. #name
        | otherwise = lookForClassMethodInExport payload moduleT cMethodN (moduleT ^. #exports)
  in modName' >>= (modulesMap !?)

lookForClassMethodInExport :: Payload -> ModuleT -> String -> ExportList -> Maybe String
lookForClassMethodInExport _ _ _ AllE = Nothing
lookForClassMethodInExport payload moduleT cMethodN (SomeE expList) = headMaybe . mapMaybe (lookForClassMethodInExport' payload moduleT cMethodN) $ expList

lookForClassMethodInExport' :: Payload -> ModuleT -> String -> ExportItem -> Maybe String
lookForClassMethodInExport' payload@Payload {..} moduleT cMethodN ExportType {qualifier = qualifier} =
  let imports' = filter (checkImportForClassMethod payload qualifier cMethodN) (moduleT ^. #imports)
  in fmap (^. #name) $ headMaybe $ mapMaybe (lookForClassMethod payload cMethodN . (modulesMap !) . (^. #_module)) imports'
lookForClassMethodInExport' payload@Payload {..} moduleT cMethodN ExportModule {name = name'}
  | name' /= moduleT ^. #name =
    case modulesMap !? name' of
      Just moduleT' -> (^. #name) <$> lookForClassMethod payload cMethodN moduleT'
      Nothing ->
        let imports' = filter (checkImportForClassMethod payload (Just name') cMethodN) (moduleT ^. #imports)
        in fmap (^. #name) $ headMaybe $ mapMaybe (lookForClassMethod payload cMethodN . (modulesMap !) . (^. #_module)) imports'
  | otherwise = Nothing
lookForClassMethodInExport' _ _ _ ExportVar {} = Nothing
