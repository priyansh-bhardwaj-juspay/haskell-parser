{-# OPTIONS_GHC -Wno-partial-fields #-}
module Types.Mod
  ( module Types.Mod ) where

import GHC.Generics
import qualified Language.Haskell.Exts as LHE
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import Data.Hashable (Hashable)

data Items = All | Some [String] | None
  deriving Show

data TypeImport = TypeImport
  { _nameTypeImport :: String
  , _includesTypeImport :: Items
  }
  deriving Show

data ImportItem = TypeImportItem TypeImport | VarImportItem String
  deriving Show

data SpecsList = Include [ImportItem] | Hide [ImportItem]
  deriving Show

data Position = Position
  { _line :: Int
  , _col :: Int
  }
  deriving (Show, Eq)

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

data VarDesc = VarDesc
  { _nameVarDesc :: String
  , _locationVarDesc :: Range
  , _code :: String
  , _dependencies :: [Entity]
  }
  deriving Show

data Import = Import
  { _repositoryImport :: String
  , _moduleImport :: String
  , _qualified :: Bool
  , _alias :: Maybe String
  , _specsList :: SpecsList
  }
  deriving Show

data EntityDef = EntityDef
  { _repositoryEntityDef :: String
  , _moduleEntityDef :: String
  , _nameEntityDef :: String
  }
  deriving (Show, Eq, Generic, Hashable)

data InstanceMethodDef = InstanceMethodDef
  { _repositoryInstanceMethodDef :: String
  , _moduleInstanceMethodDef :: String
  , _classInstanceMethodDef :: String
  , _method :: String
  }
  deriving (Show, Eq)

data Entity = Type EntityDef | Variable EntityDef | InstanceMethod InstanceMethodDef
  deriving (Show, Eq)

data ExportList = AllE | SomeE [ExportItem]
  deriving Show

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

data DataDesc = DataDesc
  { _nameDataDesc :: String
  , _locationDataDesc :: Range
  , _stringifiedDataDesc :: String
  , _constructorsDataDesc :: [ConsDesc]
  }
  deriving Show

data TypeSynDesc = TypeSynDesc
  { _nameTypeSynDesc :: String
  , _locationTypeSynDesc :: Range
  , _stringifiedTypeSynDesc :: String
  }
  deriving Show

data GadtDesc = GadtDesc
  { _nameGadtDesc :: String
  , _locationGadtDesc :: Range
  , _stringifiedGadtDesc :: String
  , _constructorsGadtDesc :: [ConsDesc]
  }
  deriving Show

data TypeDesc = DataT DataDesc | TypeSynT TypeSynDesc | GadtT GadtDesc
  deriving Show

data ClassDesc = ClassDesc
  { _nameClassDesc :: String
  , _locationClassDesc :: Range
  , _stringifiedClassDesc :: String
  , _methodsClassDesc :: [VarDesc]
  , _instancesClassDesc :: [InstanceDef]
  }
  deriving Show

data InstanceDef = InstanceDef
  { _repositoryInstanceDef :: String
  , _moduleInstanceDef :: String
  , _headInstanceDef :: String
  }
  deriving (Show, Eq, Generic, Hashable)

data InstanceDesc = InstanceDesc
  { _classInstanceDesc :: String
  , _moduleInstanceDesc :: Maybe String
  , _headInstanceDesc :: String
  , _locationInstanceDesc :: Range
  , _stringifiedInstanceDesc :: String
  , _methodsInstanceDesc :: [VarDesc]
  }
  deriving Show

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
  , _variablesSet :: HS.HashSet String
  }
  deriving Show

data Repository = Repository
  { _nameRepository :: String
  , _modules :: [ModuleT]
  }
  deriving Show

type Src = LHE.SrcSpanInfo

type ConnMap = (Map EntityDef [InstanceDef], Map InstanceDef EntityDef)

(<#) :: [a] -> a -> [a]
list <# x = list <> [x]
infixr 1 <#

(<:) :: Eq a => a -> [a] -> [a]
x <: xr | x `elem` xr = xr
        | otherwise = x : xr

type Map = HM.HashMap
data Payload = Payload
  { _repositoryPayload :: String
  , _reposMap :: Map String (Map String ModuleT)
  , _importsPayload :: [Import]
  , _modName :: String
  , _prefix :: [String]
  , _localBindings :: HS.HashSet String
  }

type RepoName = String

data ParseRepoInput = ParseRepoInput
  { _nameParseModuleInput :: String
  , _data :: RepoInputData
  , _dependenciesParseRepoInput :: [RepoName]
  }
  deriving Show

type DataJsonPath = String
type RepoSrcPath = String
type InputJsonFile = String

data RepoInputData = ParsedRepo DataJsonPath | UnparsedRepo RepoSrcPath
  deriving Show

data Command
  = ParseSingleRepo RepoName RepoSrcPath
  | ParseRepoGraph InputJsonFile
