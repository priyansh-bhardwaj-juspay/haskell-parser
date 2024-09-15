module Types.Class
  ( NameLens(..)
  , Merge(..)
  ) where

import Types.Mod
import Data.Maybe (fromMaybe)

class NameLens s t where
  name_ :: s -> t

instance NameLens TypeDesc String where
  name_ (DataT dataT) = _nameDataDesc dataT
  name_ (TypeSynT typeSynT) = _nameTypeSynDesc typeSynT
  name_ (GadtT gadtT) = _nameGadtDesc gadtT

instance NameLens ConsDesc String where
  name_ = _nameConsDesc

instance NameLens ImportItem String where
  name_ (TypeImportItem typeImport) = _nameTypeImport typeImport
  name_ (VarImportItem varImport) = varImport


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

