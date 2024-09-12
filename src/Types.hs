{-# OPTIONS_GHC -Wno-partial-fields #-}
module Types
  ( module Types ) where

import GHC.Generics
import qualified Language.Haskell.Exts as LHE
import Data.Maybe (fromMaybe)
import Control.Lens ((^.), (.~), (&))
import Data.Generics.Labels ()
import Data.Aeson
    ( ToJSON(toJSON),
      (.=),
      genericToJSON,
      defaultOptions,
      Options(fieldLabelModifier, omitNothingFields),
      Value )
import Data.Aeson.Types (object)
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import Data.Hashable (Hashable)
import Control.Lens.Lens (Lens')

toJsonOptions :: Options
toJsonOptions = defaultOptions { fieldLabelModifier = dropWhile (== '_'), omitNothingFields = True }

data Items = All | Some [String] | None
  deriving (Show, Generic)

instance ToJSON Items where
  toJSON :: Items -> Value
  toJSON All = object [ "type" .= ("All" :: String) ]
  toJSON (Some items) = object
    [ "type" .= ("Some" :: String)
    , "items" .= items
    ]
  toJSON None = object [ "type" .= ("None" :: String) ]

data TypeImport = TypeImport
  { name :: String
  , includes :: Items
  }
  deriving (Show, Generic, ToJSON)

data ImportItem = TypeImportItem TypeImport | VarImportItem String
  deriving (Show, Generic)

instance ToJSON ImportItem where
  toJSON :: ImportItem -> Value
  toJSON (TypeImportItem item) = object
    [ "type" .= ("TypeOrClass" :: String)
    , "item" .= item
    ]
  toJSON (VarImportItem item) = object
    [ "type" .= ("Function" :: String)
    , "importItems" .= item
    ]

data SpecsList = Include [ImportItem] | Hide [ImportItem]
  deriving (Show, Generic)

instance ToJSON SpecsList where
  toJSON :: SpecsList -> Value
  toJSON (Include importItems) = object
    [ "type" .= ("Include" :: String)
    , "importItems" .= importItems
    ]
  toJSON (Hide importItems) = object
    [ "type" .= ("Hide" :: String)
    , "importItems" .= importItems
    ]

data Position = Position
  { line :: Int
  , col :: Int
  }
  deriving (Show, Generic, Eq, ToJSON)

instance Ord Position where
  compare :: Position -> Position -> Ordering
  compare pos1 pos2
    | pos1 == pos2 = EQ
    | pos1 ^. #line < pos2 ^. #line || (pos1 ^. #line == pos2 ^. #line && pos1 ^. #col < pos2 ^. #col) = LT
    | otherwise = GT

data Range = Range
  { start :: Position
  , end :: Position
  }
  deriving (Show, Generic, ToJSON)

data VarDesc = VarDesc
  { name :: String
  , location :: Range
  , code :: String
  , dependencies :: [Entity]
  }
  deriving (Show, Generic, ToJSON)

data Import = Import
  { _module :: String
  , qualified :: Bool
  , alias :: Maybe String
  , specsList :: SpecsList
  }
  deriving (Show, Generic)

instance ToJSON Import where
  toJSON :: Import -> Value
  toJSON = genericToJSON toJsonOptions

data EntityDef = EntityDef
  { _module :: String
  , name :: String
  }
  deriving (Show, Generic, Eq, Hashable)

instance ToJSON EntityDef where
  toJSON :: EntityDef -> Value
  toJSON = genericToJSON toJsonOptions

data InstanceMethodDef = InstanceMethodDef
  { _module :: String
  , _class :: String
  , method :: String
  }
  deriving (Show, Generic, Eq)

instance ToJSON InstanceMethodDef where
  toJSON :: InstanceMethodDef -> Value
  toJSON = genericToJSON toJsonOptions

data Entity = Type EntityDef | Variable EntityDef | InstanceMethod InstanceMethodDef
  deriving (Show, Generic, Eq)

instance ToJSON Entity where
  toJSON :: Entity -> Value
  toJSON (Type def) = object ["type" .= ("Type" :: String), "def" .= def]
  toJSON (Variable def) = object ["type" .= ("Function" :: String), "def" .= def]
  toJSON (InstanceMethod def) = object ["type" .= ("InstanceMethod" :: String), "def" .= def]

class NameLens s t where
  name_ :: Lens' s t

class ModuleLens s t where
  module_ :: Lens' s t

instance NameLens TypeDesc String where
  name_ :: Lens' TypeDesc String
  name_ f (DataT dataT) = (\ name' -> DataT $ dataT & #name .~ name') <$> f (dataT ^. #name)
  name_ f (TypeSynT typeSynT) = (\ name' -> TypeSynT $ typeSynT & #name .~ name') <$> f (typeSynT ^. #name)
  name_ f (GadtT gadtT) = (\ name' -> GadtT $ gadtT & #name .~ name') <$> f (gadtT ^. #name)

instance NameLens ConsDesc String where
  name_ :: Lens' ConsDesc String
  name_ f OrdinaryCon {..} = OrdinaryCon <$> f name
  name_ f InfixCon {..} = InfixCon <$> f name
  name_ f RecordCon {..} = (`RecordCon` fields) <$> f name

instance NameLens ImportItem String where
  name_ :: Lens' ImportItem String
  name_ f (TypeImportItem typeImport) = (\ name' -> TypeImportItem $ typeImport & #name .~ name') <$> f (typeImport ^. #name)
  name_ f (VarImportItem varImport) = VarImportItem <$> f varImport

data ExportList = AllE | SomeE [ExportItem]
  deriving (Show, Generic)

instance ToJSON ExportList where
  toJSON :: ExportList -> Value
  toJSON AllE = object
    [ "type" .= ("All" :: String)
    ]
  toJSON (SomeE list) = object
    [ "type" .= ("Some" :: String)
    , "list" .= list
    ]

data ExportItem
  = ExportType
    { name :: String
    , qualifier :: Maybe String
    , includes :: Items
    }
  | ExportModule
    { name :: String
    }
  | ExportVar
    { name :: String
    , qualifier :: Maybe String
    }
  deriving (Show)

instance ToJSON ExportItem where
  toJSON :: ExportItem -> Value
  toJSON ExportType {..} = object
    [ "type" .= ("Type" :: String)
    , "def" .= object
      [ "name" .= name
      , "qualifier" .= qualifier
      , "includes" .= includes
      ]
    ]
  toJSON ExportModule {..} = object
    [ "type" .= ("Module" :: String)
    , "def" .= object
      [ "name" .= name
      ]
    ]
  toJSON ExportVar {..} = object
    [ "type" .= ("Module" :: String)
    , "def" .= object
      [ "name" .= name
      , "qualifier" .= qualifier
      ]
    ]

data ConsDesc
  = OrdinaryCon
    { name :: String
    }
  | InfixCon
    { name :: String
    }
  | RecordCon
    { name :: String
    , fields :: [String]
    }
  deriving (Show)

instance ToJSON ConsDesc where
  toJSON :: ConsDesc -> Value
  toJSON OrdinaryCon {..} = object
    [ "type" .= ("Ordinary" :: String)
    , "name" .= name
    ]
  toJSON InfixCon {..} = object
    [ "type" .= ("Infix" :: String)
    , "name" .= name
    ]
  toJSON RecordCon {..} = object
    [ "type" .= ("Record" :: String)
    , "name" .= name
    , "fields" .= fields
    ]

data DataDesc = DataDesc
  { name :: String
  , location :: Range
  , stringified :: String
  , constructors :: [ConsDesc]
  }
  deriving (Show, Generic, ToJSON)

data TypeSynDesc = TypeSynDesc
  { name :: String
  , location :: Range
  , stringified :: String
  }
  deriving (Show, Generic, ToJSON)

data GadtDesc = GadtDesc
  { name :: String
  , location :: Range
  , stringified :: String
  , constructors :: [ConsDesc]
  }
  deriving (Show, Generic, ToJSON)

data TypeDesc = DataT DataDesc | TypeSynT TypeSynDesc | GadtT GadtDesc
  deriving (Show)

instance ToJSON TypeDesc where
  toJSON :: TypeDesc -> Value
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
  { name :: String
  , location :: Range
  , stringified :: String
  , methods :: [VarDesc]
  , instances :: [InstanceDef]
  }
  deriving (Show, Generic, ToJSON)

data InstanceDef = InstanceDef
  { _module :: String
  , head :: String
  }
  deriving (Show, Generic, Eq, Hashable)

instance ToJSON InstanceDef where
  toJSON :: InstanceDef -> Value
  toJSON = genericToJSON toJsonOptions

data InstanceDesc = InstanceDesc
  { _class :: String
  , _module :: Maybe String
  , head :: String
  , location :: Range
  , stringified :: String
  , methods :: [VarDesc]
  }
  deriving (Show, Generic)

instance ToJSON InstanceDesc where
  toJSON :: InstanceDesc -> Value
  toJSON = genericToJSON toJsonOptions

data ClassOInstance = ClassE ClassDesc | InstanceE InstanceDesc
  deriving (Show)

data ModuleT = ModuleT
  { name :: String
  , imports :: [Import]
  , variables :: [VarDesc]
  , types :: [TypeDesc]
  , classes :: [ClassDesc]
  , instances :: [InstanceDesc]
  , exports :: ExportList
  }
  deriving (Show, Generic, ToJSON)

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
  similar x y = x ^. #name == y ^. #name

  merge :: VarDesc -> VarDesc -> VarDesc
  merge x y =
    let code = if x ^. #location . #start <= y ^. #location . #start
        then x ^. #code <> "\n" <> y ^. #code
        else y ^. #code <> "\n" <> x ^. #code
    in VarDesc
      { name = x ^. #name
      , location = Range (min (x ^. #location . #start) (y ^. #location . #start)) (max (x ^. #location . #end) (y ^. #location . #end))
      , code = code
      , dependencies = x ^. #dependencies
      }

(<#) :: [a] -> a -> [a]
list <# x = list <> [x]
infixr 1 <#

(<:) :: Eq a => a -> [a] -> [a]
x <: xr | x `elem` xr = xr
        | otherwise = x : xr

type Map = HM.HashMap
data Payload = Payload
  { modulesMap :: Map String ModuleT
  , imports :: [Import]
  , modName :: String
  , prefix :: [String]
  , localBindings :: HS.HashSet String
  }
  deriving (Generic)
