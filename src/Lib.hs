module Lib
  ( run,
  )
where

import Data.List ( isSuffixOf, intercalate, find )
import Language.Haskell.Exts as LHE
import System.Directory (doesDirectoryExist, doesFileExist, listDirectory)
import Types
import Control.Lens hiding (List)
import Data.Maybe (mapMaybe, catMaybes)
import Data.Generics.Labels ()
import Text.Pretty.Simple
import GHC.List (foldl')
import qualified Data.HashMap.Strict as HM
import Data.Aeson.Encode.Pretty (encodePretty)
import Data.ByteString.Lazy.Char8 (unpack)
import Data.HashMap.Internal.Strict ((!))
import Control.Applicative ((<|>))
import qualified Data.HashSet as HS
import Parse.Variable
import Parse.Import
import Parse.Utils

run :: IO ()
run = do
  let srcPath = "/Users/priyanshbhardwaj/Documents/newton-hs/src/"
  files <- filter (isSuffixOf ".hs") <$> allFiles srcPath
  moduleInfo <- mapM fileModules files
  let modules'' = map parseModuleT moduleInfo
      moduleNames :: [String] = map (^. #name) modules''
      modules' = map (clearImports moduleNames) modules''
      modulesMap = foldl' (\ hm mod' -> HM.insert (mod' ^. #name) mod' hm) HM.empty modules'
      modules = map (collectVarModule modulesMap) moduleInfo
  -- pPrint modulesT
  writeFile "data.json" $ unpack $ encodePretty modules
  return ()

clearImports :: [String] -> ModuleT -> ModuleT
clearImports moduleNames module' =
  module' & #imports .~ filter (\ importT -> importT ^. #_module `elem` moduleNames) (module' ^. #imports)

allFiles :: FilePath -> IO [FilePath]
allFiles basePath = allFilesHelper basePath =<< listDirectory basePath
  where
    allFilesHelper _ [] = return []
    allFilesHelper basePath' (fp : fps) = do
      fileExists <- doesFileExist $ basePath' <> fp
      dirExists <- doesDirectoryExist $ basePath' <> fp
      if fileExists
        then (basePath' <> fp :) <$> allFilesHelper basePath' fps
        else
          if dirExists
            then do
              l1 <- allFilesHelper (basePath' <> fp <> "/") =<< listDirectory (basePath' <> fp)
              l2 <- allFilesHelper basePath' fps
              return $ l1 <> l2
            else allFilesHelper basePath' fps

fileModules :: FilePath -> IO (Module Src)
fileModules fname = do
  fcontents <- readFile fname
  -- case parse $ sanitize fcontents of
  let parsedFile =
        parseWithMode
          ( defaultParseMode
              { parseFilename = fname,
                ignoreLinePragmas = False,
                extensions =
                  [ EnableExtension PackageImports,
                    EnableExtension UndecidableInstances,
                    EnableExtension DeriveAnyClass,
                    EnableExtension ExplicitForAll,
                    EnableExtension BangPatterns,
                    EnableExtension TemplateHaskell,
                    EnableExtension ConstraintKinds,
                    EnableExtension DataKinds,
                    EnableExtension DefaultSignatures,
                    EnableExtension DeriveFunctor,
                    EnableExtension DeriveGeneric,
                    -- , EnableExtension DuplicateRecordFields
                    EnableExtension ExplicitNamespaces,
                    EnableExtension FlexibleContexts,
                    EnableExtension FlexibleInstances,
                    EnableExtension FunctionalDependencies,
                    EnableExtension GADTs,
                    EnableExtension LambdaCase,
                    EnableExtension MultiParamTypeClasses,
                    EnableExtension MultiWayIf,
                    EnableExtension NamedFieldPuns,
                    EnableExtension OverloadedStrings,
                    EnableExtension PatternSynonyms,
                    EnableExtension PolyKinds,
                    EnableExtension RankNTypes,
                    EnableExtension RecordWildCards,
                    EnableExtension ScopedTypeVariables,
                    EnableExtension TupleSections,
                    EnableExtension TypeApplications,
                    EnableExtension TypeFamilies,
                    EnableExtension TypeOperators,
                    EnableExtension ViewPatterns,
                    EnableExtension BlockArguments,
                    EnableExtension StandaloneDeriving
                  ],
                ignoreLanguagePragmas = False,
                fixities = Just $ infixl_ 8 ["^."]
              }
          )
          fcontents
  case parsedFile of
    (ParseOk moduleInfo) -> return moduleInfo
    (ParseFailed (SrcLoc _ line col) err) ->
      error $
        "Failed to parse module in " ++ fname ++ ":\n" ++ "  (" ++ show line ++ ":" ++ show col ++ ") " ++ err ++ "\n" ++ "  " ++ getLineCol fcontents (line, col)
  where
    getLineCol fcontents (line, col) =
      ln ++ "\n" ++ "  " ++ replicate (col' - 3) ' ' ++ "^^^"
      where
        ln = lines fcontents !! line
        col' =
          let l = length ln
           in min col l

parseModuleT :: Module Src -> ModuleT
parseModuleT (Module _ (Just (ModuleHead _ (ModuleName _ name') _ _)) _ importDecls decls) =
  let imports' = foldr mkImport [] importDecls
      variables' = foldr (mkVarR []) [] decls
  in ModuleT
    { name = name'
    , imports = imports'
    , variables = variables'
    }
parseModuleT _other = error $ "Unknown module type " <> show _other
