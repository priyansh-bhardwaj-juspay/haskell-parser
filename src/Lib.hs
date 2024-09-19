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
import Control.Monad (foldM)
import Data.HashMap.Strict ((!), (!?))
import Data.Aeson (eitherDecodeFileStrict')
import Data.Maybe (mapMaybe)

run :: Command -> IO ()
run (ParseSingleRepo repoName srcPath) = do
  start <- getCurrentTime
  !repository <- parseSingleRepo repoName srcPath
  let !modules = _modules repository
  dumpRepository repository
  getCurrentTime >>= (\ stop -> putStrLn $ "Execution Time: " <> show (diffUTCTime stop start))
  putStrLn $ "Total Functions: " <> show (foldl' (\ sm mod' -> sm + length (_variables mod')) 0 modules)
  putStrLn $ "Total Types: " <> show (foldl' (\ sm mod' -> sm + length (_types mod')) 0 modules)
  putStrLn $ "Total Classes: " <> show (foldl' (\ sm mod' -> sm + length (_classes mod')) 0 modules)
  putStrLn $ "Total Instances: " <> show (foldl' (\ sm mod' -> sm + length (_instancesModuleT mod')) 0 modules)
run (ParseRepoGraph inputFile) = do
  start <- getCurrentTime
  !parseInput <- either fail return =<< eitherDecodeFileStrict' inputFile
  !repositories <- parseRepoGraph parseInput
  getCurrentTime >>= (\ stop -> putStrLn $ "Parsing Time: " <> show (diffUTCTime stop start))
  mapM_ dumpRepository repositories
  getCurrentTime >>= (\ stop -> putStrLn $ "Execution Time: " <> show (diffUTCTime stop start))

parseSingleRepo :: String -> String -> IO Repository
parseSingleRepo repoName srcPath = do
  (modules, modulesInfo) <- parseRepoModules srcPath
  return $ parseDependencies (Repository repoName modules) modulesInfo []

parseRepoGraph :: [ParseRepoInput] -> IO [Repository]
parseRepoGraph repoGraph = do
  let inputMap = foldl' (\ hm input -> HM.insert (_nameParseModuleInput input) input hm) HM.empty repoGraph
  reposMap <- foldM (parseModuleDfs inputMap) HM.empty repoGraph
  let repoList = map ((reposMap !) . _nameParseModuleInput) repoGraph
  return repoList

parseModuleDfs :: Map String ParseRepoInput -> Map String Repository -> ParseRepoInput -> IO (Map String Repository)
parseModuleDfs _ reposMap ParseRepoInput {_nameParseModuleInput} | HM.member _nameParseModuleInput reposMap = return reposMap
parseModuleDfs inputMap reposMap ParseRepoInput {..} = do
  reposMap' <- foldM (\ hm -> parseModuleDfs inputMap hm . (inputMap !)) reposMap _dependenciesParseRepoInput
  repo <- case _data of
    ParsedRepo dataPath -> do
      modules <- either fail return =<< eitherDecodeFileStrict' dataPath
      return $ Repository _nameParseModuleInput modules
    UnparsedRepo srcPath -> do
      (modules, modulesInfo) <- parseRepoModules srcPath
      let depRepos = map (reposMap' !) _dependenciesParseRepoInput
          repo = parseDependencies (Repository _nameParseModuleInput modules) modulesInfo depRepos
      return repo
  return $ HM.insert _nameParseModuleInput repo reposMap

parseRepoModules :: String -> IO ([ModuleT], [Module Src])
parseRepoModules srcPath = do
  files <- filter (isSuffixOf ".hs") <$> allFiles srcPath
  modulesInfo <- mapM fileModules files
  let !modules = map parseModuleT modulesInfo
  return (modules, modulesInfo)

parseDependencies :: Repository -> [Module Src] -> [Repository] -> Repository
parseDependencies selfRepo@Repository {..} modulesInfo depRepos =
  let repoModules = depRepos
      repoModules1 = selfRepo : repoModules
      moduleNames = foldl' (\ hm1 Repository {..} ->
        foldl' (\ hm2 ModuleT {_nameModuleT} -> HM.insert _nameModuleT _nameRepository hm2) hm1 _modules) HM.empty repoModules1
      modules' = map (clearImports moduleNames) _modules
      repoModules2 = Repository _nameRepository modules' : repoModules
      modules = collectClassInstances _nameRepository repoModules2 $ map (collectVarModule _nameRepository repoModules2) modulesInfo
  in Repository _nameRepository modules

dumpRepository :: Repository -> IO ()
dumpRepository Repository {..} = writeFile (_nameRepository <> "-data.json") $ unpack $ encodePretty _modules

clearImports :: Map String String -> ModuleT -> ModuleT
clearImports moduleNames module' =
  module' { _importsModuleT = mapMaybe (\ importT -> fmap (\ repo -> importT { _repositoryImport = repo })
    $ moduleNames !? _moduleImport importT) (_importsModuleT module') }

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
                    EnableExtension StandaloneDeriving,
                    EnableExtension DerivingStrategies,
                    EnableExtension GeneralizedNewtypeDeriving,
                    EnableExtension OverloadedLabels,
                    EnableExtension DerivingVia,
                    EnableExtension InstanceSigs,
                    EnableExtension RoleAnnotations,
                    EnableExtension QuantifiedConstraints
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
