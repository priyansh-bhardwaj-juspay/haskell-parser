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
    { name = name'
    , qualifier = qualifier
    }
mkExportItem (EAbs _ _ qName) =
  let (qualifier, name') = openQName qName
  in ExportType
    { name = name'
    , qualifier = qualifier
    , includes = None
    }
mkExportItem (EThingWith _ (EWildcard _ _) qName _) =
  let (qualifier, name') = openQName qName
  in ExportType
    { name = name'
    , qualifier = qualifier
    , includes = All
    }
mkExportItem (EThingWith _ (NoWildcard _) qName cNameL) =
  let (qualifier, name') = openQName qName
      consList = mapMaybe mkExportCons cNameL
  in ExportType
    { name = name'
    , qualifier = qualifier
    , includes = Some consList
    }
mkExportItem (EModuleContents _ modName) = ExportModule . moduleName $ modName

mkExportCons :: CName SrcSpanInfo -> Maybe String
mkExportCons (VarName _ _) = Nothing
mkExportCons (ConName _ name') = Just $ getName name'
