{-# OPTIONS_GHC -Wno-orphans #-}
module Types.JsonInstances () where

import Types.Mod
import Data.Aeson (ToJSON(toJSON), (.=))
import Data.Aeson.Types (object)

instance ToJSON Items where
  toJSON All = object [ "type" .= ("All" :: String) ]
  toJSON (Some items) = object
    [ "type" .= ("Some" :: String)
    , "items" .= items
    ]
  toJSON None = object [ "type" .= ("None" :: String) ]

instance ToJSON TypeImport where
  toJSON TypeImport {..} = object
    [ "name" .= _nameTypeImport
    , "includes" .= _includesTypeImport
    ]

instance ToJSON ImportItem where
  toJSON (TypeImportItem item) = object
    [ "type" .= ("TypeOrClass" :: String)
    , "item" .= item
    ]
  toJSON (VarImportItem item) = object
    [ "type" .= ("Function" :: String)
    , "importItems" .= item
    ]

instance ToJSON SpecsList where
  toJSON (Include importItems) = object
    [ "type" .= ("Include" :: String)
    , "importItems" .= importItems
    ]
  toJSON (Hide importItems) = object
    [ "type" .= ("Hide" :: String)
    , "importItems" .= importItems
    ]

instance ToJSON Position where
  toJSON (Position {..}) = object
    [ "line" .= _line
    , "col" .= _col
    ]

instance ToJSON Range where
  toJSON Range {..} = object
    [ "start" .= _start
    , "end" .= _end
    ]

instance ToJSON VarDesc where
  toJSON VarDesc {..} = object
    [ "name" .= _nameVarDesc
    , "location" .= _locationVarDesc
    , "code" .= _code
    , "dependencies" .= _dependencies
    ]

instance ToJSON Import where
  toJSON Import {..} = object
    [ "module" .= _moduleImport
    , "qualified" .= _qualified
    , "alias" .= _alias
    , "specsList" .= _specsList
    ]

instance ToJSON EntityDef where
  toJSON EntityDef {..} = object
    [ "module" .= _moduleEntityDef
    , "name" .= _nameEntityDef
    ]

instance ToJSON InstanceMethodDef where
  toJSON InstanceMethodDef {..} = object
    [ "module" .= _moduleInstanceMethodDef
    , "class" .= _classInstanceMethodDef
    , "method" .= _method
    ]

instance ToJSON Entity where
  toJSON (Type def) = object ["type" .= ("Type" :: String), "def" .= def]
  toJSON (Variable def) = object ["type" .= ("Function" :: String), "def" .= def]
  toJSON (InstanceMethod def) = object ["type" .= ("InstanceMethod" :: String), "def" .= def]

instance ToJSON ExportList where
  toJSON AllE = object
    [ "type" .= ("All" :: String)
    ]
  toJSON (SomeE list) = object
    [ "type" .= ("Some" :: String)
    , "list" .= list
    ]

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
    [ "type" .= ("Module" :: String)
    , "def" .= object
      [ "name" .= _nameExportItem
      , "qualifier" .= _qualifier
      ]
    ]

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

instance ToJSON DataDesc where
  toJSON DataDesc {..} = object
    [ "name" .= _nameDataDesc
    , "localtion" .= _locationDataDesc
    , "stringified" .= _stringifiedDataDesc
    , "constructors" .= _constructorsDataDesc
    ]

instance ToJSON TypeSynDesc where
  toJSON TypeSynDesc {..} = object
    [ "name" .= _nameTypeSynDesc
    , "localtion" .= _locationTypeSynDesc
    , "stringified" .= _stringifiedTypeSynDesc
    ]

instance ToJSON GadtDesc where
  toJSON GadtDesc {..} = object
    [ "name" .= _nameGadtDesc
    , "localtion" .= _locationGadtDesc
    , "stringified" .= _stringifiedGadtDesc
    , "constructors" .= _constructorsGadtDesc
    ]

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

instance ToJSON ClassDesc where
  toJSON (ClassDesc {..}) = object
    [ "name" .= _nameClassDesc
    , "location" .= _locationClassDesc
    , "stringified" .= _stringifiedClassDesc
    , "methods" .= _methodsClassDesc
    , "instances" .= _instancesClassDesc
    ]

instance ToJSON InstanceDef where
  toJSON InstanceDef {..} = object
    [ "module" .= _moduleInstanceDef
    , "head" .= _headInstanceDef
    ]

instance ToJSON InstanceDesc where
  toJSON InstanceDesc {..} = object
    [ "class" .= _classInstanceDesc
    , "module" .= _moduleInstanceDesc
    , "head" .= _headInstanceDesc
    , "location" .= _locationInstanceDesc
    , "stringified" .= _stringifiedInstanceDesc
    , "methods" .= _methodsInstanceDesc
    ]

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
