module Parse.Export 
  ( mkExportList
  ) where

import Types
import Language.Haskell.Exts
import GHC.Data.Maybe (mapMaybe)
import Parse.Utils

mkExportList :: Maybe (ExportSpecList SrcSpanInfo) -> ExportList
mkExportList Nothing = AllE
mkExportList (Just (ExportSpecList _ exportList)) = SomeE . map mkExportItem $ exportList

mkExportItem :: ExportSpec SrcSpanInfo -> ExportItem
mkExportItem (EVar _ qName) =
  let (qualifier, name') = openQName qName
  in ExportVar
    { _nameExportItem = name'
    , _qualifier = qualifier
    }
mkExportItem (EAbs _ _ qName) =
  let (qualifier, name') = openQName qName
  in ExportType
    { _nameExportItem = name'
    , _qualifier = qualifier
    , _includesExportItem = None
    }
mkExportItem (EThingWith _ (EWildcard _ _) qName _) =
  let (qualifier, name') = openQName qName
  in ExportType
    { _nameExportItem = name'
    , _qualifier = qualifier
    , _includesExportItem = All
    }
mkExportItem (EThingWith _ (NoWildcard _) qName cNameL) =
  let (qualifier, name') = openQName qName
      consList = mapMaybe mkExportCons cNameL
  in ExportType
    { _nameExportItem = name'
    , _qualifier = qualifier
    , _includesExportItem = Some consList
    }
mkExportItem (EModuleContents _ modName) = ExportModule . moduleName $ modName

mkExportCons :: CName SrcSpanInfo -> Maybe String
mkExportCons (VarName _ _) = Nothing
mkExportCons (ConName _ name') = Just $ getName name'
