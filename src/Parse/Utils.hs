module Parse.Utils where

import Types
import Language.Haskell.Exts

mkRange :: Src -> Range
mkRange (SrcSpanInfo (SrcSpan _ startLine' startCol' endLine' endCol') _) = Range (Position startLine' startCol') (Position endLine' endCol')

getNameFromPat :: Match Src -> String
getNameFromPat (Match _ name' _ _ _) = getName name'
getNameFromPat (InfixMatch _ _ name' _ _ _) = getName name'

getName :: Name Src -> String
getName (Ident _ name') = name'
getName (Symbol _ name') = name'

getConstructor :: CName Src -> Maybe String
getConstructor (ConName _ name') = Just $ getName name'
getConstructor _ = Nothing

