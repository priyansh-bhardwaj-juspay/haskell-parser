{-# LANGUAGE InstanceSigs #-}
{-# OPTIONS_GHC -Wno-partial-fields #-}
module Types
  ( module Types ) where

import GHC.Generics
import qualified Language.Haskell.Exts as LHE
import Data.Maybe (fromMaybe)
import Control.Lens ((^.), (.~), (&))
import Data.Generics.Labels ()
import Data.Aeson ( ToJSON(toJSON), (.=) )
import Data.Aeson.Types (object)
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import Data.Hashable (Hashable)
import Control.Lens.Lens (Lens')

data Items = All | Some [String] | None
  deriving (Show, Generic)

instance ToJSON Items where
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
  toJSON (TypeImportItem item) = object
    [ "type" .= ("Type" :: String)
    , "item" .= item
    ]
  toJSON (VarImportItem item) = object
    [ "type" .= ("Function" :: String)
    , "importItems" .= item
    ]

data SpecsList = Include [ImportItem] | Hide [ImportItem]
  deriving (Show, Generic)

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
  toJSON Import {..} = object
    [ "module" .= _module
    , "qualified" .= qualified
    , "alias" .= alias
    , "specsList" .= specsList
    ]

data EntityDef = EntityDef
  { _module :: String
  , name :: String
  }
  deriving (Show, Generic, Eq, Hashable)

instance ToJSON EntityDef where
  toJSON EntityDef {..} = object ["module" .= _module, "name" .= name]

data Entity = Type EntityDef | Variable EntityDef
  deriving (Show, Generic, Eq, Hashable)

instance ToJSON Entity where
  toJSON (Type def) = object ["type" .= ("Type" :: String), "def" .= def]
  toJSON (Variable def) = object ["type" .= ("Function" :: String), "def" .= def]

name_ :: Lens' Entity String
name_ f (Type def) = (\ name' -> Type $ def & #name .~ name') <$> f (def ^. #name)
name_ f (Variable def) = (\ name' -> Variable $ def & #name .~ name') <$> f (def ^. #name)

module_ :: Lens' Entity String
module_ f (Type def) = (\ module' -> Type $ def & #_module .~ module') <$> f (def ^. #_module)
module_ f (Variable def) = (\ module' -> Variable $ def & #_module .~ module') <$> f (def ^. #_module)

data ExportList = AllE | SomeE [ExportItem]
  deriving (Show, Generic)

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

data ModuleT = ModuleT
  { name :: String
  , imports :: [Import]
  , variables :: [VarDesc]
  , exports :: ExportList
  }
  deriving (Show, Generic, ToJSON)

type Src = LHE.SrcSpanInfo

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
