module Parse.Utils 
  ( module Parse.Utils ) where

import Types
import Language.Haskell.Exts
import Control.Lens ((^.))
import Data.Foldable (find)
import Data.Maybe (isNothing)

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

openQName :: QName SrcSpanInfo -> (Maybe String, String)
openQName (Qual _ modName name') = (Just $ moduleName modName, getName name')
openQName (UnQual _ name') = (Nothing, getName name')
openQName _other = error $ "Unknown qName : " <> show _other

moduleName :: ModuleName Src -> String
moduleName (ModuleName _ name') = name'

headMaybe :: [a] -> Maybe a
headMaybe [] = Nothing
headMaybe (x:_) = Just x

getNameQ :: QName Src -> String
getNameQ (Qual _ (ModuleName _ _) cNameT) = getName cNameT
getNameQ (UnQual _ cNameT) = getName cNameT
getNameQ _other@(Special _ _) = error $ "Unexpected patter in getNameQ : " <> show _other

searchNameDecl :: DeclHead Src -> String
searchNameDecl (DHead _ name') = getName name'
searchNameDecl (DHApp _ declHead _) = searchNameDecl declHead
searchNameDecl _other = error $ "Unknown parseDataDecl : " <> show _other

cMethodExist :: String -> ClassDesc -> Bool
cMethodExist cmName (ClassDesc { methods }) = elem cmName . map (^. #name) $ methods

checkForClassMethod :: String -> ModuleT -> Bool
checkForClassMethod cMethodN moduleT = checkExportListForClassMethod (moduleT ^. #exports) cMethodN moduleT && any (cMethodExist cMethodN) (moduleT ^. #classes)

checkExportListForClassMethod :: ExportList -> String -> ModuleT -> Bool
checkExportListForClassMethod AllE _ _ = True
checkExportListForClassMethod (SomeE items) cMethodN moduleT = any (matchExportItemForClassMethod cMethodN moduleT) items

matchExportItemForClassMethod :: String -> ModuleT -> ExportItem -> Bool
matchExportItemForClassMethod cMethodN moduleT (ExportType name' Nothing All) =
  let classT = find ((name' ==) . (^. #name)) (moduleT ^. #classes)
  in maybe False (cMethodExist cMethodN) classT
matchExportItemForClassMethod _ _ (ExportType _ (Just _) _) = False
matchExportItemForClassMethod cMethodN _ (ExportType _ qualifier (Some list)) = isNothing qualifier && cMethodN `elem` list
matchExportItemForClassMethod _ _ (ExportType _ _ None) = False
matchExportItemForClassMethod _ _ (ExportVar {}) = False
matchExportItemForClassMethod _ _ (ExportModule {}) = False

consExist :: String -> TypeDesc -> Bool
consExist cName (DataT dataT) = elem cName . map (^. name_) $ dataT ^. #constructors
consExist _ (TypeSynT _) = False
consExist cName (GadtT gadtT) = elem cName . map (^. name_) $ gadtT ^. #constructors

checkForCons :: String -> ModuleT -> Bool
checkForCons consN moduleT = checkExportListForCons (moduleT ^. #exports) consN moduleT && any (consExist consN) (moduleT ^. #types)

checkExportListForCons :: ExportList -> String -> ModuleT -> Bool
checkExportListForCons AllE _ _ = True
checkExportListForCons (SomeE items) consN moduleT = any (matchExportItemForCons consN moduleT) items

matchExportItemForCons :: String -> ModuleT -> ExportItem -> Bool
matchExportItemForCons consN moduleT (ExportType name' Nothing All) =
  let typeT = find ((name' ==) . (^. name_)) (moduleT ^. #types)
  in maybe False (consExist consN) typeT
matchExportItemForCons _ _ (ExportType _ (Just _) _) = False
matchExportItemForCons consN _ (ExportType _ qualifier (Some list)) = isNothing qualifier && consN `elem` list
matchExportItemForCons _ _ (ExportType _ _ None) = False
matchExportItemForCons _ _ (ExportVar {}) = False
matchExportItemForCons _ _ (ExportModule {}) = False

checkForType :: String -> ModuleT -> Bool
checkForType typeN moduleT = checkExportListForType (moduleT ^. #exports) typeN && any ((typeN ==) . (^. name_)) (moduleT ^. #types)

checkExportListForType :: ExportList -> String -> Bool
checkExportListForType AllE _ = True
checkExportListForType (SomeE items) typeN = any (matchExportItemForType typeN) items

matchExportItemForType :: String -> ExportItem -> Bool
matchExportItemForType typeN (ExportType {name = name', ..}) = isNothing qualifier && name' == typeN
matchExportItemForType _ (ExportModule {}) = False
matchExportItemForType _ (ExportVar {}) = False

checkForClass :: String -> ModuleT -> Bool
checkForClass classN moduleT = checkExportListForClass (moduleT ^. #exports) classN && any ((classN ==) . (^. #name)) (moduleT ^. #classes)

checkExportListForClass :: ExportList -> String -> Bool
checkExportListForClass AllE _ = True
checkExportListForClass (SomeE items) classN = any (matchExportItemForClass classN) items

matchExportItemForClass :: String -> ExportItem -> Bool
matchExportItemForClass classN (ExportType {name = name', ..}) = isNothing qualifier && name' == classN
matchExportItemForClass _ (ExportModule {}) = False
matchExportItemForClass _ (ExportVar {}) = False
