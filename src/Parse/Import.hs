module Parse.Import 
  ( mkImport
  ) where

import Types.Mod
import Language.Haskell.Exts
import Parse.Utils
import Data.Maybe (mapMaybe)

mkImport :: ImportDecl Src -> [Import] -> [Import]
mkImport (ImportDecl _ (ModuleName _ name') qualified' _ _ _ importAs' mImportSpecList) res =
  let specsList' = maybe (Hide [])
        (\ (ImportSpecList _ hiding list) -> let list' = map mkImportSpec list in if hiding then Hide list' else Include list')
        mImportSpecList
      _import = Import
        { _repositoryImport = "unknown"
        , _moduleImport = name'
        , _qualified = qualified'
        , _alias = moduleName <$> importAs'
        , _specsList = specsList'
        }
  in _import : res

mkImportSpec :: ImportSpec Src -> ImportItem
mkImportSpec (IVar _ name') =
  VarImportItem $ getName name'
mkImportSpec (IAbs _ _ name') =
  TypeImportItem $ TypeImport
    { _nameTypeImport = getName name'
    , _includesTypeImport = None
    }
mkImportSpec (IThingAll _ name') =
  TypeImportItem $ TypeImport
    { _nameTypeImport = getName name'
    , _includesTypeImport = All
    }
mkImportSpec (IThingWith _ name' list) =
  TypeImportItem $ TypeImport
    { _nameTypeImport = getName name'
    , _includesTypeImport = Some (mapMaybe getConstructor list)
    }
