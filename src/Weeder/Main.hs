{-# language ApplicativeDo #-}
{-# language BlockArguments #-}
{-# language FlexibleContexts #-}
{-# language NamedFieldPuns #-}
{-# language OverloadedStrings #-}
{-# language LambdaCase #-}
{-# language RecordWildCards #-}

-- | This module provides an entry point to the Weeder executable.

module Weeder.Main ( main, mainWithConfig, getHieFiles, runWeeder, Weed(..), formatWeed ) where

-- base
import Control.Concurrent ( getChanContents, newChan, writeChan, forkIO )
import Control.Monad ( guard, unless, when )
import Data.Foldable
import Data.Function ((&))
import Data.List ( isSuffixOf, sortOn )
import Data.Maybe ( isJust, catMaybes )
import Data.Version ( showVersion )
import System.Exit ( ExitCode(..), exitWith )
import System.IO ( stderr, hPutStrLn, hPrint )

-- containers
import qualified Data.Map.Strict as Map
import Data.Set ( Set )
import qualified Data.Set as Set

-- toml-reader
import qualified TOML

-- directory
import System.Directory ( canonicalizePath, doesDirectoryExist, doesFileExist, doesPathExist, listDirectory, withCurrentDirectory )

-- filepath
import System.FilePath ( isExtensionOf )

-- ghc
import GHC.Iface.Ext.Binary ( HieFileResult( HieFileResult, hie_file_result ), readHieFileWithVersion )
import GHC.Iface.Ext.Types ( HieFile( hie_hs_file, hie_asts ), hieVersion, HieASTs (getAsts) )
import GHC.Iface.Ext.Utils ( generateReferencesMap )
import GHC.Unit.Module ( moduleName, moduleNameString )
import GHC.Types.Name.Cache ( initNameCache, NameCache )
import GHC.Types.Name ( occNameString )

-- regex-tdfa
import Text.Regex.TDFA ( matchTest )

-- optparse-applicative
import Options.Applicative

-- parallel
import Control.Parallel (pseq)
import Control.Parallel.Strategies ( rdeepseq, parMap )

-- text
import qualified Data.Text.IO as T

-- transformers
import Control.Monad.Trans.State.Strict ( execState )

-- weeder
import Weeder
import Weeder.Config
import Paths_weeder (version)


exitHieVersionFailure, exitConfigFailure, exitWeedsFound, exitNoHieFilesFailure :: ExitCode
exitHieVersionFailure = ExitFailure 2
exitConfigFailure = ExitFailure 3
exitNoHieFilesFailure = ExitFailure 4
exitWeedsFound = ExitFailure 228


data CLIArguments = CLIArguments
  { configPath :: FilePath
  , hieExt :: String
  , hieDirectories :: [FilePath]
  , requireHsFiles :: Bool
  , writeDefaultConfig :: Bool
  , noDefaultFields :: Bool
  }


parseCLIArguments :: Parser CLIArguments
parseCLIArguments = do
    configPath <- strOption
        ( long "config"
            <> help "A file path for Weeder's configuration."
            <> value "./weeder.toml"
            <> metavar "<weeder.toml>"
        )
    hieExt <- strOption
        ( long "hie-extension"
            <> value ".hie"
            <> help "Extension of HIE files"
            <> showDefault
        )
    hieDirectories <- many (
        strOption
            ( long "hie-directory"
                <> help "A directory to look for .hie files in. Maybe specified multiple times. Default ./."
            )
        )
    requireHsFiles <- switch
          ( long "require-hs-files"
              <> help "Skip stale .hie files with no matching .hs modules"
          )
    writeDefaultConfig <- switch
          ( long "write-default-config"
              <> help "Write a default configuration file if the one specified by --config does not exist"
          )
    noDefaultFields <- switch
          ( long "no-default-fields"
              <> help "Do not use default field values for missing fields in the configuration."
          )
    pure CLIArguments{..}


data Weed = Weed
  { weedPath :: FilePath
    -- ^ Path to the weed
  , weedLoc :: Int
    -- ^ Line number
  , weedDeclaration :: Declaration
    -- ^ The unused declaration
  , weedPrettyPrintedType :: Maybe String
    -- ^ Pretty-printed type of the declaration -
    -- currently only for displaying type class instances
  }


formatWeed :: Weed -> String
formatWeed Weed{..} =
  weedPath <> ":" <> show weedLoc <> ": "
    <> case weedPrettyPrintedType of
      Nothing -> occNameString ( declOccName weedDeclaration )
      Just t -> "(Instance) :: " <> t


-- | Parse command line arguments and into a 'Config' and run 'mainWithConfig'.
main :: IO ()
main = do
  CLIArguments{..} <-
    execParser $
      info (parseCLIArguments <**> helper <**> versionP) mempty

  configExists <-
    doesFileExist configPath

  unless (writeDefaultConfig ==> configExists) do
    hPutStrLn stderr $ "Did not find config: wrote default config to " ++ configPath
    writeFile configPath (configToToml defaultConfig)

  decodeConfig noDefaultFields configPath
    >>= either handleConfigError pure
    >>= mainWithConfig hieExt hieDirectories requireHsFiles
  where
    handleConfigError e = do
      hPrint stderr e
      exitWith exitConfigFailure

    decodeConfig noDefaultFields =
      if noDefaultFields
        then fmap (TOML.decodeWith decodeNoDefaults) . T.readFile
        else TOML.decodeFile

    versionP = infoOption ( "weeder version "
                            <> showVersion version
                            <> "\nhie version "
                            <> show hieVersion )
        ( long "version" <> help "Show version" )


-- | Run Weeder in the current working directory with a given 'Config'.
--
-- This will recursively find all files with the given extension in the given directories, perform
-- analysis, and report all unused definitions according to the 'Config'.
mainWithConfig :: String -> [FilePath] -> Bool -> Config -> IO ()
mainWithConfig hieExt hieDirectories requireHsFiles weederConfig = do
  hieFiles <-
    getHieFiles hieExt hieDirectories requireHsFiles

  when (null hieFiles) do
    hPutStrLn stderr $
      "No HIE files found: check that the directory is correct " ++
      "and that the -fwrite-ide-info compilation flag is set."
    exitWith exitNoHieFilesFailure

  let 
    (weeds, _) = 
      runWeeder weederConfig hieFiles
    
  mapM_ (putStrLn . formatWeed) weeds

  unless (null weeds) $ exitWith exitWeedsFound


-- | Find and read all .hie files in the given directories according to the given parameters,
-- exiting if any are incompatible with the current version of GHC.
--
-- The list of hie files is returned while it is still being read.
getHieFiles :: String -> [FilePath] -> Bool -> IO [HieFile]
getHieFiles hieExt hieDirectories requireHsFiles = do
  hieFilePaths <-
    concat <$>
      traverse ( getFilesIn hieExt )
        ( if null hieDirectories
          then ["./."]
          else hieDirectories
        )

  hsFilePaths <-
    if requireHsFiles
      then getFilesIn ".hs" "./."
      else pure []

  hieFileResultsChan <- newChan

  nameCache <-
    initNameCache 'z' []

  _ <- forkIO do
    readHieFiles nameCache hieFilePaths hieFileResultsChan hsFilePaths
    writeChan hieFileResultsChan Nothing

  catMaybes . takeWhile isJust <$> getChanContents hieFileResultsChan

  where

    readHieFiles nameCache hieFilePaths hieFileResultsChan hsFilePaths =
      for_ hieFilePaths \hieFilePath -> do
        hieFileResult <-
          readCompatibleHieFileOrExit nameCache hieFilePath
        let hsFileExists = any ( hie_hs_file hieFileResult `isSuffixOf` ) hsFilePaths
        when (requireHsFiles ==> hsFileExists) $
          writeChan hieFileResultsChan (Just hieFileResult)


-- | Run Weeder on the given .hie files with the given 'Config'.
--
-- Returns a list of 'Weed's that can be displayed using
-- 'formatWeed', and the final 'Analysis'.
runWeeder :: Config -> [HieFile] -> ([Weed], Analysis)
runWeeder weederConfig@Config{ rootPatterns, typeClassRoots, rootInstances } hieFiles =
  let
    asts = concatMap (Map.elems . getAsts . hie_asts) hieFiles

    rf = generateReferencesMap asts

    -- Evaluating 1 HieFile at a time has almost identical performance to 
    -- doing this in batches of 100
    analyses =
      parMap rdeepseq (\hf -> execState (analyseHieFile weederConfig hf) emptyAnalysis) hieFiles

    analyseEvidenceUses' = 
      if typeClassRoots
        then id
        else analyseEvidenceUses rf

    analysis1 = 
      foldl' mappend mempty analyses

    -- Evaluating 'analysis1' first allows us to begin analysis 
    -- while hieFiles is still being read (since rf depends on all hie files)
    analysis = analysis1 `pseq`
      analyseEvidenceUses' analysis1

    -- We limit ourselves to outputable declarations only rather than all
    -- declarations in the graph. This has a slight performance benefit,
    -- at the cost of having to assume that a non-outputable declaration
    -- will always either be an implicit root or irrelevant.
    roots =
      Set.filter
        ( \d ->
            any
              (`matchTest` displayDeclaration d)
              rootPatterns
        )
        ( outputableDeclarations analysis )

    reachableSet =
      reachable
        analysis
        ( Set.map DeclarationRoot roots <> filterImplicitRoots analysis ( implicitRoots analysis ) )

    -- We only care about dead declarations if they have a span assigned,
    -- since they don't show up in the output otherwise
    dead =
      outputableDeclarations analysis Set.\\ reachableSet

    warnings =
      Map.unionsWith (++) $
      foldMap
        ( \d ->
            fold $ do
              moduleFilePath <- Map.lookup ( declModule d ) ( modulePaths analysis )
              starts <- Map.lookup d ( declarationSites analysis )
              guard $ not $ null starts
              return [ Map.singleton moduleFilePath ( liftA2 (,) (Set.toList starts) (pure d) ) ]
        )
        dead

    weeds =
      Map.toList warnings & concatMap \( weedPath, declarations ) ->
        sortOn fst declarations & map \( weedLoc, weedDeclaration ) ->
          Weed { weedPrettyPrintedType = Map.lookup weedDeclaration (prettyPrintedType analysis)
               , weedPath
               , weedLoc
               , weedDeclaration
               }

  in (weeds, analysis)

  where

    filterImplicitRoots :: Analysis -> Set Root -> Set Root
    filterImplicitRoots Analysis{ prettyPrintedType, modulePaths } = Set.filter $ \case
      DeclarationRoot _ -> True -- keep implicit roots for rewrite rules etc

      ModuleRoot _ -> True

      InstanceRoot d c -> typeClassRoots || matchingType
        where
          matchingType = 
            let mt = Map.lookup d prettyPrintedType
                matches = maybe (const False) (flip matchTest) mt
            in any (maybe True matches) filteredInstances

          filteredInstances = 
            map instancePattern 
            . filter (maybe True (`matchTest` displayDeclaration c) . classPattern) 
            . filter (maybe True modulePathMatches . modulePattern) 
            $ rootInstances

          modulePathMatches p = maybe False (p `matchTest`) (Map.lookup ( declModule d ) modulePaths)


displayDeclaration :: Declaration -> String
displayDeclaration d = 
  moduleNameString ( moduleName ( declModule d ) ) <> "." <> occNameString ( declOccName d )


-- | Recursively search for files with the given extension in given directory
getFilesIn
  :: String
  -- ^ Only files with this extension are considered
  -> FilePath
  -- ^ Directory to look in
  -> IO [FilePath]
getFilesIn ext path = do
  exists <-
    doesPathExist path

  if exists
    then do
      isFile <-
        doesFileExist path

      if isFile && ext `isExtensionOf` path
        then do
          path' <-
            canonicalizePath path

          return [ path' ]

        else do
          isDir <-
            doesDirectoryExist path

          if isDir
            then do
              cnts <-
                listDirectory path

              withCurrentDirectory path ( foldMap ( getFilesIn ext ) cnts )

            else
              return []

    else
      return []


-- | Read a .hie file, exiting if it's an incompatible version.
readCompatibleHieFileOrExit :: NameCache -> FilePath -> IO HieFile
readCompatibleHieFileOrExit nameCache path = do
  res <- readHieFileWithVersion (\(v, _) -> v == hieVersion) nameCache path
  case res of
    Right HieFileResult{ hie_file_result } ->
      return hie_file_result
    Left ( v, _ghcVersion ) -> do
      putStrLn $ "incompatible hie file: " <> path
      putStrLn $ "    this version of weeder was compiled with GHC version "
               <> show hieVersion
      putStrLn $ "    the hie files in this project were generated with GHC version "
               <> show v
      putStrLn $ "    weeder must be built with the same GHC version"
               <> " as the project it is used on"
      exitWith exitHieVersionFailure


infixr 5 ==>


-- | An infix operator for logical implication
(==>) :: Bool -> Bool -> Bool
True  ==> x = x
False ==> _ = True
