{-# OPTIONS_GHC -Wno-partial-fields #-}
module Types
  ( module Types ) where

import GHC.Generics
import qualified Language.Haskell.Exts as LHE
import Data.Maybe (fromMaybe)
import Data.Aeson (ToJSON(toJSON), (.=))
import Data.Aeson.Types (object)
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import Data.Hashable (Hashable)

data Items = All | Some [String] | None
  deriving Show

instance ToJSON Items where
  toJSON All = object [ "type" .= ("All" :: String) ]
  toJSON (Some items) = object
    [ "type" .= ("Some" :: String)
    , "items" .= items
    ]
  toJSON None = object [ "type" .= ("None" :: String) ]

data TypeImport = TypeImport
  { _nameTypeImport :: String
  , _includesTypeImport :: Items
  }
  deriving Show

instance ToJSON TypeImport where
  toJSON TypeImport {..} = object
    [ "name" .= _nameTypeImport
    , "includes" .= _includesTypeImport
    ]

data ImportItem = TypeImportItem TypeImport | VarImportItem String
  deriving Show

instance ToJSON ImportItem where
  toJSON (TypeImportItem item) = object
    [ "type" .= ("TypeOrClass" :: String)
    , "item" .= item
    ]
  toJSON (VarImportItem item) = object
    [ "type" .= ("Function" :: String)
    , "importItems" .= item
    ]

data SpecsList = Include [ImportItem] | Hide [ImportItem]
  deriving Show

instance ToJSON SpecsList where
  toJSON (Include importItems) = object
    [ "type" .= ("Include" :: String)
    , "importItems" .= importItems
    ]
  toJSON (Hide importItems) = object
    [ "type" .= ("Hide" :: String)
    , "importItems" .= importItems
    ]

data Position = Position
  { _line :: Int
  , _col :: Int
  }
  deriving (Show, Eq)

instance ToJSON Position where
  toJSON (Position {..}) = object
    [ "line" .= _line
    , "col" .= _col
    ]

instance Ord Position where
  compare :: Position -> Position -> Ordering
  compare pos1@Position {_line = line1, _col = col1} pos2@Position {_line = line2, _col = col2}
    | pos1 == pos2 = EQ
    | line1 < line2 || (line1 == line2 && col1 < col2 ) = LT
    | otherwise = GT

data Range = Range
  { _start :: Position
  , _end :: Position
  }
  deriving Show

instance ToJSON Range where
  toJSON Range {..} = object
    [ "start" .= _start
    , "end" .= _end
    ]

data VarDesc = VarDesc
  { _nameVarDesc :: String
  , _locationVarDesc :: Range
  , _code :: String
  , _dependencies :: [Entity]
  }
  deriving Show

instance ToJSON VarDesc where
  toJSON VarDesc {..} = object
    [ "name" .= _nameVarDesc
    , "location" .= _locationVarDesc
    , "code" .= _code
    , "dependencies" .= _dependencies
    ]

data Import = Import
  { _moduleImport :: String
  , _qualified :: Bool
  , _alias :: Maybe String
  , _specsList :: SpecsList
  }
  deriving Show

instance ToJSON Import where
  toJSON Import {..} = object
    [ "module" .= _moduleImport
    , "qualified" .= _qualified
    , "alias" .= _alias
    , "specsList" .= _specsList
    ]

data EntityDef = EntityDef
  { _moduleEntityDef :: String
  , _nameEntityDef :: String
  }
  deriving (Show, Eq, Generic, Hashable)

instance ToJSON EntityDef where
  toJSON EntityDef {..} = object
    [ "module" .= _moduleEntityDef
    , "name" .= _nameEntityDef
    ]

data InstanceMethodDef = InstanceMethodDef
  { _moduleInstanceMethodDef :: String
  , _classInstanceMethodDef :: String
  , _method :: String
  }
  deriving (Show, Eq)

instance ToJSON InstanceMethodDef where
  toJSON InstanceMethodDef {..} = object
    [ "module" .= _moduleInstanceMethodDef
    , "class" .= _classInstanceMethodDef
    , "method" .= _method
    ]

data Entity = Type EntityDef | Variable EntityDef | InstanceMethod InstanceMethodDef
  deriving (Show, Eq)

instance ToJSON Entity where
  toJSON (Type def) = object ["type" .= ("Type" :: String), "def" .= def]
  toJSON (Variable def) = object ["type" .= ("Function" :: String), "def" .= def]
  toJSON (InstanceMethod def) = object ["type" .= ("InstanceMethod" :: String), "def" .= def]

class NameLens s t where
  name_ :: s -> t

class ModuleLens s t where
  module_ :: s -> t

instance NameLens TypeDesc String where
  name_ (DataT dataT) = _nameDataDesc dataT
  name_ (TypeSynT typeSynT) = _nameTypeSynDesc typeSynT
  name_ (GadtT gadtT) = _nameGadtDesc gadtT

instance NameLens ConsDesc String where
  name_ = _nameConsDesc
  -- name_ f OrdinaryCon {..} = OrdinaryCon <$> f name
  -- name_ f InfixCon {..} = InfixCon <$> f name
  -- name_ f RecordCon {..} = (`RecordCon` fields) <$> f name

instance NameLens ImportItem String where
  name_ (TypeImportItem typeImport) = _nameTypeImport typeImport
  name_ (VarImportItem varImport) = varImport

data ExportList = AllE | SomeE [ExportItem]
  deriving Show

instance ToJSON ExportList where
  toJSON AllE = object
    [ "type" .= ("All" :: String)
    ]
  toJSON (SomeE list) = object
    [ "type" .= ("Some" :: String)
    , "list" .= list
    ]

data ExportItem
  = ExportType
    { _nameExportItem :: String
    , _qualifier :: Maybe String
    , _includesExportItem :: Items
    }
  | ExportModule
    { _nameExportItem :: String
    }
  | ExportVar
    { _nameExportItem :: String
    , _qualifier :: Maybe String
    }
  deriving Show

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

data ConsDesc
  = OrdinaryCon
    { _nameConsDesc :: String
    }
  | InfixCon
    { _nameConsDesc :: String
    }
  | RecordCon
    { _nameConsDesc :: String
    , _fields :: [String]
    }
  deriving Show

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

data DataDesc = DataDesc
  { _nameDataDesc :: String
  , _locationDataDesc :: Range
  , _stringifiedDataDesc :: String
  , _constructorsDataDesc :: [ConsDesc]
  }
  deriving Show

instance ToJSON DataDesc where
  toJSON DataDesc {..} = object
    [ "name" .= _nameDataDesc
    , "localtion" .= _locationDataDesc
    , "stringified" .= _stringifiedDataDesc
    , "constructors" .= _constructorsDataDesc
    ]

data TypeSynDesc = TypeSynDesc
  { _nameTypeSynDesc :: String
  , _locationTypeSynDesc :: Range
  , _stringifiedTypeSynDesc :: String
  }
  deriving Show

instance ToJSON TypeSynDesc where
  toJSON TypeSynDesc {..} = object
    [ "name" .= _nameTypeSynDesc
    , "localtion" .= _locationTypeSynDesc
    , "stringified" .= _stringifiedTypeSynDesc
    ]

data GadtDesc = GadtDesc
  { _nameGadtDesc :: String
  , _locationGadtDesc :: Range
  , _stringifiedGadtDesc :: String
  , _constructorsGadtDesc :: [ConsDesc]
  }
  deriving Show

instance ToJSON GadtDesc where
  toJSON GadtDesc {..} = object
    [ "name" .= _nameGadtDesc
    , "localtion" .= _locationGadtDesc
    , "stringified" .= _stringifiedGadtDesc
    , "constructors" .= _constructorsGadtDesc
    ]

data TypeDesc = DataT DataDesc | TypeSynT TypeSynDesc | GadtT GadtDesc
  deriving Show

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

data ClassDesc = ClassDesc
  { _nameClassDesc :: String
  , _locationClassDesc :: Range
  , _stringifiedClassDesc :: String
  , _methodsClassDesc :: [VarDesc]
  , _instancesClassDesc :: [InstanceDef]
  }
  deriving Show

instance ToJSON ClassDesc where
  toJSON (ClassDesc {..}) = object
    [ "name" .= _nameClassDesc
    , "location" .= _locationClassDesc
    , "stringified" .= _stringifiedClassDesc
    , "methods" .= _methodsClassDesc
    , "instances" .= _instancesClassDesc
    ]

data InstanceDef = InstanceDef
  { _moduleInstanceDef :: String
  , _headInstanceDef :: String
  }
  deriving (Show, Eq, Generic, Hashable)

instance ToJSON InstanceDef where
  toJSON InstanceDef {..} = object
    [ "module" .= _moduleInstanceDef
    , "head" .= _headInstanceDef
    ]

data InstanceDesc = InstanceDesc
  { _classInstanceDesc :: String
  , _moduleInstanceDesc :: Maybe String
  , _headInstanceDesc :: String
  , _locationInstanceDesc :: Range
  , _stringifiedInstanceDesc :: String
  , _methodsInstanceDesc :: [VarDesc]
  }
  deriving Show

instance ToJSON InstanceDesc where
  toJSON InstanceDesc {..} = object
    [ "class" .= _classInstanceDesc
    , "module" .= _moduleInstanceDesc
    , "head" .= _headInstanceDesc
    , "location" .= _locationInstanceDesc
    , "stringified" .= _stringifiedInstanceDesc
    , "methods" .= _methodsInstanceDesc
    ]

data ClassOInstance = ClassE ClassDesc | InstanceE InstanceDesc
  deriving Show

data DepsMapKey = EntityD EntityDef | InstanceD InstanceDef
  deriving (Show, Eq, Generic, Hashable)

data ModuleT = ModuleT
  { _nameModuleT :: String
  , _importsModuleT :: [Import]
  , _variables :: [VarDesc]
  , _types :: [TypeDesc]
  , _classes :: [ClassDesc]
  , _instancesModuleT :: [InstanceDesc]
  , _exports :: ExportList
  }
  deriving Show

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

type Src = LHE.SrcSpanInfo

type ConnMap = (Map EntityDef [InstanceDef], Map InstanceDef EntityDef)

class Merge a where
  similar :: a -> a -> Bool

  merge :: a -> a -> a

  (<:>) :: a -> [a] -> [a]
  x <:> ys =
    let merged = mergeAux x ys
    in fromMaybe (x:ys) merged

  mergeAux :: a -> [a] -> Maybe [a]
  mergeAux _ [] = Nothing
  mergeAux x (y:ys)
    | similar x y = Just $ merge x y : ys
    | otherwise = fmap (y:) (mergeAux x ys)

instance Merge VarDesc where
  similar :: VarDesc -> VarDesc -> Bool
  similar x y = _nameVarDesc x == _nameVarDesc y

  merge :: VarDesc -> VarDesc -> VarDesc
  merge x y =
    let code = if _start (_locationVarDesc x) <= _start (_locationVarDesc y)
        then _code x <> "\n" <> _code y
        else _code y <> "\n" <> _code x
    in VarDesc
      { _nameVarDesc = _nameVarDesc x
      , _locationVarDesc = Range (min (_start (_locationVarDesc x)) (_start (_locationVarDesc y))) (max (_end (_locationVarDesc x)) (_end (_locationVarDesc y)))
      , _code = code
      , _dependencies = _dependencies x
      }

(<#) :: [a] -> a -> [a]
list <# x = list <> [x]
infixr 1 <#

(<:) :: Eq a => a -> [a] -> [a]
x <: xr | x `elem` xr = xr
        | otherwise = x : xr

type Map = HM.HashMap
data Payload = Payload
  { _modulesMap :: Map String ModuleT
  , _importsPayload :: [Import]
  , _modName :: String
  , _prefix :: [String]
  , _localBindings :: HS.HashSet String
  }
