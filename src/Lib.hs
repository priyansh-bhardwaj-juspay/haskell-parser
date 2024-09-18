module Lib
  ( run
  )
where

import Data.List ( isSuffixOf )
import Language.Haskell.Exts as LHE
import System.Directory (doesDirectoryExist, doesFileExist, listDirectory)
import Types.Mod
import Types.JsonInstances ()
import GHC.List (foldl')
import qualified Data.HashMap.Strict as HM
import Data.Aeson.Encode.Pretty (encodePretty)
import Data.ByteString.Lazy.Char8 (unpack)
import Parse.Variable
import Parse.Import
import Parse.Export
import Data.Time (getCurrentTime, diffUTCTime)
import Parse.Type
import Parse.Class
import qualified Data.HashSet as HS

run :: IO ()
run = do
  start <- getCurrentTime
  let srcPath = "/Users/priyanshbhardwaj/Documents/newton-hs/src/"
      repoName = "newton-hs"
  repository <- parseSingleRepo repoName srcPath
  let modules = _modules repository
  writeFile (repoName <> "-data.json") $ unpack $ encodePretty modules
  getCurrentTime >>= (\ stop -> putStrLn $ "Execution Time: " <> show (diffUTCTime stop start))
  putStrLn $ "Total Functions: " <> show (foldl' (\ sm mod' -> sm + length (_variables mod')) 0 modules)
  putStrLn $ "Total Types: " <> show (foldl' (\ sm mod' -> sm + length (_types mod')) 0 modules)
  putStrLn $ "Total Classes: " <> show (foldl' (\ sm mod' -> sm + length (_classes mod')) 0 modules)
  putStrLn $ "Total Instances: " <> show (foldl' (\ sm mod' -> sm + length (_instancesModuleT mod')) 0 modules)

parseSingleRepo :: String -> String -> IO Repository
parseSingleRepo repoName repoPath = do
  (modules, modulesInfo) <- parseRepoModules repoPath
  return $ parseDependencies (Repository repoName modules) modulesInfo []

parseRepoModules :: String -> IO ([ModuleT], [Module Src])
parseRepoModules srcPath = do
  files <- filter (isSuffixOf ".hs") <$> allFiles srcPath
  modulesInfo <- mapM fileModules files
  let !modules = map parseModuleT modulesInfo
  return (modules, modulesInfo)

parseDependencies :: Repository -> [Module Src] -> [Repository] -> Repository
parseDependencies Repository {..} modulesInfo depRepos =
  let mkModulesMap = foldl' (\ hm mod' -> HM.insert (_nameModuleT mod') mod' hm) HM.empty
      repoModules = map (\ Repository {..} -> RepoModuleMap _nameRepository (mkModulesMap _modules)) depRepos
      repoModules1 = RepoModuleMap _nameRepository (mkModulesMap _modules) : repoModules
      moduleNames = foldl' (\ hs RepoModuleMap {_modulesMap} -> foldl' (flip HS.insert) hs $ HM.keys _modulesMap) HS.empty repoModules1
      modules' = map (clearImports moduleNames) _modules
      repoModules2 = RepoModuleMap _nameRepository (mkModulesMap modules') : repoModules
      modules = collectClassInstances _nameRepository repoModules2 $ map (collectVarModule _nameRepository repoModules2) modulesInfo
  in Repository _nameRepository modules

clearImports :: HS.HashSet String -> ModuleT -> ModuleT
clearImports moduleNames module' =
  module' { _importsModuleT = filter (\ importT -> _moduleImport importT `elem` moduleNames) (_importsModuleT module') }

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
parseModuleT (Module _ (Just (ModuleHead _ (ModuleName _ name') _ mExportSpecList)) _ importDecls decls) =
  let imports' = foldr mkImport [] importDecls
      variables' = foldr (mkVarR []) [] decls
      types' = foldr mkTypeR [] decls
      (classes', instances') = classOInstance $ foldr mkClassInstanceR [] decls
      exports' = mkExportList mExportSpecList
  in ModuleT
    { _nameModuleT = name'
    , _importsModuleT = imports'
    , _variables = variables'
    , _types = types'
    , _classes = classes'
    , _instancesModuleT = instances'
    , _exports = exports'
    , _variablesSet = foldl' (\ hs -> flip HS.insert hs . _nameVarDesc) HS.empty variables'
    }
parseModuleT _other = error $ "Unknown module type " <> show _other
