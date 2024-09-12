module Parse.Type
  ( mkTypeR
  ) where

import Types
import Language.Haskell.Exts
import Parse.Utils
import Data.Maybe ( mapMaybe )

mkTypeR :: Decl Src -> [TypeDesc] -> [TypeDesc]
mkTypeR decl res = maybe res (:res) $ mkType decl

mkType :: Decl SrcSpanInfo -> Maybe TypeDesc
mkType decl@(DataDecl srcInfo _ _ declHead consDecl _) =
  let constructors' = map mkConstructor consDecl
      name' = searchNameDecl declHead
      typeInfo = DataDesc
        { name = name'
        , location = mkRange srcInfo
        , stringified = prettyPrint decl
        , constructors = constructors'
        }
  in Just . DataT $ typeInfo
mkType decl@(TypeDecl srcInfo declHead _) =
  let name' = searchNameDecl declHead
      typeInfo = TypeSynDesc
        { name = name'
        , location = mkRange srcInfo
        , stringified = prettyPrint decl
        }
  in Just . TypeSynT $ typeInfo
mkType decl@(GDataDecl srcInfo _ _ declHead _ mConsDecl _) =
  let constructors' = map mkConstructorGadt mConsDecl
      name' = searchNameDecl declHead
      typeInfo = DataDesc
        { name = name'
        , location = mkRange srcInfo
        , stringified = prettyPrint decl
        , constructors = constructors'
        }
  in Just . DataT $ typeInfo
mkType _ = Nothing

mkConstructorGadt :: GadtDecl Src -> ConsDesc
mkConstructorGadt (GadtDecl _ name' _ _ (Just fieldDecls) _) = RecordCon (getName name') (mapMaybe getFieldName fieldDecls)
mkConstructorGadt (GadtDecl _ name' _ _ Nothing _) = OrdinaryCon (getName name')

mkConstructor :: QualConDecl Src -> ConsDesc
mkConstructor (QualConDecl _ _ _ (ConDecl _ name' _)) = OrdinaryCon (getName name')
mkConstructor (QualConDecl _ _ _ (InfixConDecl _ _ name' _)) = InfixCon (getName name')
mkConstructor (QualConDecl _ _ _ (RecDecl _ name' fieldDecls)) = RecordCon (getName name') (mapMaybe getFieldName fieldDecls)

getFieldName :: FieldDecl Src -> Maybe String
getFieldName (FieldDecl _ (name':_) _) = Just $ getName name'
getFieldName _ = Nothing

searchNameDecl :: DeclHead Src -> String
searchNameDecl (DHead _ name') = getName name'
searchNameDecl (DHApp _ declHead _) = searchNameDecl declHead
searchNameDecl _other = error $ "Unknown parseDataDecl : " <> show _other
