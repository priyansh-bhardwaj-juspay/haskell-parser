{-# OPTIONS_GHC -Wno-orphans #-}
module Types.JsonInstances () where

import Types.Mod
import Data.Aeson
import qualified Data.HashSet as HS
import Data.List (foldl')
import Data.Maybe (fromMaybe)

instance FromJSON Items where
  parseJSON = withObject "Items" $ \ hm -> do
    _type <- hm .: "type"
    case _type of
      "All" -> pure All
      "Some" -> Some <$> hm .: "items"
      "None" -> pure None
      _ -> fail $ "Unknown type: " <> _type

instance ToJSON Items where
  toJSON All = object [ "type" .= ("All" :: String) ]
  toJSON (Some items) = object
    [ "type" .= ("Some" :: String)
    , "items" .= items
    ]
  toJSON None = object [ "type" .= ("None" :: String) ]

instance FromJSON TypeImport where
  parseJSON = withObject "TypeImport" $ \ hm -> TypeImport
    <$> hm .: "name"
    <*> hm .: "includes"

instance ToJSON TypeImport where
  toJSON TypeImport {..} = object
    [ "name" .= _nameTypeImport
    , "includes" .= _includesTypeImport
    ]

instance FromJSON ImportItem where
  parseJSON = withObject "ImportItem" $ \ hm -> do
    _type <- hm .: "type"
    case _type of
      "TypeOrClass" -> TypeImportItem <$> hm .: "item"
      "Function" -> VarImportItem <$> hm .: "item"
      _ -> fail $ "Unknown type: " <> _type

instance ToJSON ImportItem where
  toJSON (TypeImportItem item) = object
    [ "type" .= ("TypeOrClass" :: String)
    , "item" .= item
    ]
  toJSON (VarImportItem item) = object
    [ "type" .= ("Function" :: String)
    , "item" .= item
    ]

instance FromJSON SpecsList where
  parseJSON = withObject "SpecsList" $ \ hm -> do
    _type <- hm .: "type"
    case _type of
      "Include" -> Include <$> hm .: "importItems"
      "Hide" -> Hide <$> hm .: "importItems"
      _ -> fail $ "Unknown type: " <> _type

instance ToJSON SpecsList where
  toJSON (Include importItems) = object
    [ "type" .= ("Include" :: String)
    , "importItems" .= importItems
    ]
  toJSON (Hide importItems) = object
    [ "type" .= ("Hide" :: String)
    , "importItems" .= importItems
    ]

instance FromJSON Position where
  parseJSON = withObject "Position" $ \ hm -> Position
    <$> hm .: "line"
    <*> hm .: "col"

instance ToJSON Position where
  toJSON (Position {..}) = object
    [ "line" .= _line
    , "col" .= _col
    ]

instance FromJSON Range where
  parseJSON = withObject "Range" $ \ hm -> Range
    <$> hm .: "start"
    <*> hm .: "end"

instance ToJSON Range where
  toJSON Range {..} = object
    [ "start" .= _start
    , "end" .= _end
    ]

instance FromJSON VarDesc where
  parseJSON = withObject "VarDesc" $ \ hm -> VarDesc
    <$> hm .: "name"
    <*> hm .: "location"
    <*> hm .: "code"
    <*> hm .: "dependencies"

instance ToJSON VarDesc where
  toJSON VarDesc {..} = object
    [ "name" .= _nameVarDesc
    , "location" .= _locationVarDesc
    , "code" .= _code
    , "dependencies" .= _dependencies
    ]

instance FromJSON Import where
  parseJSON = withObject "Import" $ \ hm -> Import
    <$> hm .: "repository"
    <*> hm .: "module"
    <*> hm .: "qualified"
    <*> hm .: "alias"
    <*> hm .: "specsList"

instance ToJSON Import where
  toJSON Import {..} = object
    [ "repository" .= _repositoryImport
    , "module" .= _moduleImport
    , "qualified" .= _qualified
    , "alias" .= _alias
    , "specsList" .= _specsList
    ]

instance FromJSON EntityDef where
  parseJSON = withObject "EntityDef" $ \ hm -> EntityDef
    <$> hm .: "repository"
    <*> hm .: "module"
    <*> hm .: "name"

instance ToJSON EntityDef where
  toJSON EntityDef {..} = object
    [ "repository" .= _repositoryEntityDef
    , "module" .= _moduleEntityDef
    , "name" .= _nameEntityDef
    ]

instance FromJSON InstanceMethodDef where
  parseJSON = withObject "InstanceMethodDef" $ \ hm -> InstanceMethodDef
    <$> hm .: "repository"
    <*> hm .: "module"
    <*> hm .: "class"
    <*> hm .: "method"

instance ToJSON InstanceMethodDef where
  toJSON InstanceMethodDef {..} = object
    [ "repository" .= _repositoryInstanceMethodDef
    , "module" .= _moduleInstanceMethodDef
    , "class" .= _classInstanceMethodDef
    , "method" .= _method
    ]

instance FromJSON Entity where
  parseJSON = withObject "Entity" $ \ hm -> do
    _type <- hm .: "type"
    case _type of
      "Type" -> Type <$> hm .: "def"
      "Function" -> Variable <$> hm .: "def"
      "InstanceMethod" -> InstanceMethod <$> hm .: "def"
      _ -> fail $ "Unknown type: " <> _type

instance ToJSON Entity where
  toJSON (Type def) = object ["type" .= ("Type" :: String), "def" .= def]
  toJSON (Variable def) = object ["type" .= ("Function" :: String), "def" .= def]
  toJSON (InstanceMethod def) = object ["type" .= ("InstanceMethod" :: String), "def" .= def]

instance FromJSON ExportList where
  parseJSON = withObject "ExportList" $ \ hm -> do
    _type <- hm .: "type"
    case _type of
      "All" -> pure AllE
      "Some" -> SomeE <$> hm .: "list"
      _ -> fail $ "Unknown type: " <> _type

instance ToJSON ExportList where
  toJSON AllE = object
    [ "type" .= ("All" :: String)
    ]
  toJSON (SomeE list) = object
    [ "type" .= ("Some" :: String)
    , "list" .= list
    ]

instance FromJSON ExportItem where
  parseJSON = withObject "ExportItem" $ \ hm -> do
    _type <- hm .: "type"
    def' <- hm .: "def"
    flip (withObject "def") def' $ \ def -> do
      case _type of
        "Type" -> ExportType
          <$> def .: "name"
          <*> def .: "qualifier"
          <*> def .: "includes"
        "Module" -> ExportModule
          <$> def .: "name"
        "Function" -> ExportVar
          <$> def .: "name"
          <*> def .: "qualifier"
        _ -> fail $ "Unknown type: " <> _type

instance ToJSON ExportItem where
  toJSON ExportType {..} = object
    [ "type" .= ("Type" :: String)
    , "def" .= object
      [ "name" .= _nameExportItem
      , "qualifier" .= _qualifier
      , "includes" .= _includesExportItem
      ]
    ]
  toJSON ExportModule {..} = object
    [ "type" .= ("Module" :: String)
    , "def" .= object
      [ "name" .= _nameExportItem
      ]
    ]
  toJSON ExportVar {..} = object
    [ "type" .= ("Function" :: String)
    , "def" .= object
      [ "name" .= _nameExportItem
      , "qualifier" .= _qualifier
      ]
    ]

instance FromJSON ConsDesc where
  parseJSON = withObject "ConsDesc" $ \ hm -> do
    _type <- hm .: "type"
    case _type of
      "Ordinary" -> OrdinaryCon <$> hm .: "name"
      "Infix" -> InfixCon <$> hm .: "name"
      "Record" -> RecordCon <$> hm .: "name" <*> hm .: "fields"
      _ -> fail $ "Unknown type: " <> _type

instance ToJSON ConsDesc where
  toJSON OrdinaryCon {..} = object
    [ "type" .= ("Ordinary" :: String)
    , "name" .= _nameConsDesc
    ]
  toJSON InfixCon {..} = object
    [ "type" .= ("Infix" :: String)
    , "name" .= _nameConsDesc
    ]
  toJSON RecordCon {..} = object
    [ "type" .= ("Record" :: String)
    , "name" .= _nameConsDesc
    , "fields" .= _fields
    ]

instance FromJSON DataDesc where
  parseJSON = withObject "DataDesc" $ \ hm -> DataDesc
    <$> hm .: "name"
    <*> hm .: "location"
    <*> hm .: "stringified"
    <*> hm .: "constructors"

instance ToJSON DataDesc where
  toJSON DataDesc {..} = object
    [ "name" .= _nameDataDesc
    , "location" .= _locationDataDesc
    , "stringified" .= _stringifiedDataDesc
    , "constructors" .= _constructorsDataDesc
    ]

instance FromJSON TypeSynDesc where
  parseJSON = withObject "TypeSynDesc" $ \ hm -> TypeSynDesc
    <$> hm .: "name"
    <*> hm .: "location"
    <*> hm .: "stringified"

instance ToJSON TypeSynDesc where
  toJSON TypeSynDesc {..} = object
    [ "name" .= _nameTypeSynDesc
    , "location" .= _locationTypeSynDesc
    , "stringified" .= _stringifiedTypeSynDesc
    ]

instance FromJSON GadtDesc where
  parseJSON = withObject "GadtDesc" $ \ hm -> GadtDesc
    <$> hm .: "name"
    <*> hm .: "location"
    <*> hm .: "stringified"
    <*> hm .: "constructors"

instance ToJSON GadtDesc where
  toJSON GadtDesc {..} = object
    [ "name" .= _nameGadtDesc
    , "localtion" .= _locationGadtDesc
    , "stringified" .= _stringifiedGadtDesc
    , "constructors" .= _constructorsGadtDesc
    ]

instance FromJSON TypeDesc where
  parseJSON = withObject "TypeDesc" $ \ hm -> do
    _type <- hm .: "type"
    case _type of
      "Data" -> DataT <$> hm .: "desc"
      "Type" -> TypeSynT <$> hm .: "desc"
      "GADT" -> GadtT <$> hm .: "desc"
      _ -> fail $ "Unknown type: " <> _type

instance ToJSON TypeDesc where
  toJSON (DataT desc) = object
    [ "type" .= ("Data" :: String)
    , "desc" .= desc
    ]
  toJSON (TypeSynT desc) = object
    [ "type" .= ("Type" :: String)
    , "desc" .= desc
    ]
  toJSON (GadtT desc) = object
    [ "type" .= ("GADT" :: String)
    , "desc" .= desc
    ]

instance FromJSON ClassDesc where
  parseJSON = withObject "ClassDesc" $ \ hm -> ClassDesc
    <$> hm .: "name"
    <*> hm .: "location"
    <*> hm .: "stringified"
    <*> hm .: "methods"
    <*> hm .: "instances"

instance ToJSON ClassDesc where
  toJSON (ClassDesc {..}) = object
    [ "name" .= _nameClassDesc
    , "location" .= _locationClassDesc
    , "stringified" .= _stringifiedClassDesc
    , "methods" .= _methodsClassDesc
    , "instances" .= _instancesClassDesc
    ]

instance FromJSON InstanceDef where
  parseJSON = withObject "InstanceDef" $ \ hm -> InstanceDef
    <$> hm .: "repository"
    <*> hm .: "module"
    <*> hm .: "head"

instance ToJSON InstanceDef where
  toJSON InstanceDef {..} = object
    [ "repository" .= _repositoryInstanceDef
    , "module" .= _moduleInstanceDef
    , "head" .= _headInstanceDef
    ]

instance FromJSON InstanceDesc where
  parseJSON = withObject "InstanceDesc" $ \ hm -> InstanceDesc
    <$> hm .: "class"
    <*> hm .: "module"
    <*> hm .: "head"
    <*> hm .: "location"
    <*> hm .: "stringified"
    <*> hm .: "methods"

instance ToJSON InstanceDesc where
  toJSON InstanceDesc {..} = object
    [ "class" .= _classInstanceDesc
    , "module" .= _moduleInstanceDesc
    , "head" .= _headInstanceDesc
    , "location" .= _locationInstanceDesc
    , "stringified" .= _stringifiedInstanceDesc
    , "methods" .= _methodsInstanceDesc
    ]

instance FromJSON ModuleT where
  parseJSON = withObject "ModuleT" $ \ hm -> do
    variables <- hm .: "variables"
    let variablesSet = foldl' (\ hs -> flip HS.insert hs . _nameVarDesc) HS.empty variables
    ModuleT
      <$> hm .: "name"
      <*> hm .: "imports"
      <*> return variables
      <*> hm .: "types"
      <*> hm .: "classes"
      <*> hm .: "instances"
      <*> hm .: "exports"
      <*> return variablesSet

instance ToJSON ModuleT where
  toJSON (ModuleT {..}) = object
    [ "name" .= _nameModuleT
    , "imports" .= _importsModuleT
    , "variables" .= _variables
    , "types" .= _types
    , "classes" .= _classes
    , "instances" .= _instancesModuleT
    , "exports" .= _exports
    ]

instance FromJSON ParseRepoInput where
  parseJSON = withObject "ParseRepoInput" $ \ hm -> do ParseRepoInput
    <$> hm .: "name"
    <*> hm .: "data"
    <*> (fromMaybe [] <$> hm .:? "dependencies")

instance FromJSON RepoInputData where
  parseJSON = withObject "RepoInputData" $ \ hm -> do
    _type <- hm .: "type"
    case _type of
      "Parsed" -> ParsedRepo <$> hm .: "dataPath"
      "Unparsed" -> UnparsedRepo <$> hm .: "repoSrcPath"
      _ -> fail $ "Unknown type: " <> _type
