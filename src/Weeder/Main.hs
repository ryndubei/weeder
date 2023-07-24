{-# language ApplicativeDo #-}
{-# language BlockArguments #-}
{-# language FlexibleContexts #-}
{-# language NamedFieldPuns #-}
{-# language OverloadedStrings #-}
{-# language LambdaCase #-}
{-# language RecordWildCards #-}

-- | This module provides an entry point to the Weeder executable.

module Weeder.Main ( main, mainWithConfig, getHieFiles, showWeeds, runAnalysis ) where

-- base
import Control.Exception ( throwIO )
import Control.Monad ( guard, unless )
import Data.Foldable
import Data.Function ((&))
import Data.List ( isSuffixOf, sortOn )
import Data.Version ( showVersion )
import System.Exit ( ExitCode(..), exitWith )
import System.IO ( stderr, hPutStrLn )

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
import GHC.Iface.Ext.Types ( HieFile( hie_hs_file ), hieVersion )
import GHC.Unit.Module ( moduleName, moduleNameString )
import GHC.Types.Name.Cache ( initNameCache, NameCache )
import GHC.Types.Name ( occNameString )
import GHC.Types.SrcLoc ( RealSrcLoc, realSrcSpanStart, srcLocLine )

-- regex-tdfa
import Text.Regex.TDFA ( (=~) )

-- optparse-applicative
import Options.Applicative

-- text
import qualified Data.Text.IO as T

-- transformers
import Control.Monad.Trans.State.Strict ( execState )

-- weeder
import Weeder
import Weeder.Config
import Paths_weeder (version)


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
    >>= either throwIO pure
    >>= mainWithConfig hieExt hieDirectories requireHsFiles
  where
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

  let 
    (weeds, _) = 
      runWeeder weederConfig hieFiles
    
  mapM_ putStrLn weeds

  unless (null weeds) $ exitWith (ExitFailure 2)


-- | Find and read all .hie files in the given directories according to the given parameters,
-- exiting if any are incompatible with the current version of GHC.
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

  nameCache <-
    initNameCache 'z' []

  hieFileResults <-
    mapM ( readCompatibleHieFileOrExit nameCache ) hieFilePaths

  let
    hieFileResults' = flip filter hieFileResults \hieFileResult ->
      let hsFileExists = any ( hie_hs_file hieFileResult `isSuffixOf` ) hsFilePaths
       in requireHsFiles ==> hsFileExists

  pure hieFileResults'


runWeeder :: Config -> [HieFile] -> ([String], Analysis)
runWeeder weederConfig hieFiles =
  let 
    analysis = 
      runAnalysis weederConfig hieFiles
    
    dead = 
      deadDeclarations weederConfig analysis
    
  in 
    (deadToWeeds analysis dead, analysis)


runAnalysis :: Config -> [HieFile] -> Analysis
runAnalysis weederConfig hieFiles =
  execState ( analyseHieFiles weederConfig hieFiles ) emptyAnalysis


deadDeclarations :: Config -> Analysis -> Set Declaration
deadDeclarations Config{ rootPatterns, typeClassRoots, rootClasses, rootInstances } analysis =
  let
    roots =
      Set.filter
        ( \d ->
            any
              ( ( moduleNameString ( moduleName ( declModule d ) ) <> "." <> occNameString ( declOccName d ) ) =~ )
              rootPatterns
        )
        ( allDeclarations analysis )

    reachableSet =
      reachable
        analysis
        ( Set.map DeclarationRoot roots <> filterImplicitRoots analysis ( implicitRoots analysis ) )

  in 
    allDeclarations analysis Set.\\ reachableSet

  where

    filterImplicitRoots :: Analysis -> Set Root -> Set Root
    filterImplicitRoots Analysis{ prettyPrintedType, modulePaths } = Set.filter $ \case
      DeclarationRoot _ -> True -- keep implicit roots for rewrite rules etc

      ModuleRoot _ -> True

      InstanceRoot d c -> typeClassRoots || matchingClass || matchingType
        where
          matchingClass = any (maybe True (occNameString c =~)) (filterOnModule rootClasses)

          matchingType = case Map.lookup d prettyPrintedType of
            Just t -> any (maybe True (t =~)) (filterOnModule rootInstances)
            Nothing -> False

          filterOnModule :: Set PatternWithModule -> Set (Maybe String)
          filterOnModule = Set.map mainPattern . Set.filter (maybe True modulePathMatches . modulePattern)

          modulePathMatches :: String -> Bool
          modulePathMatches p = maybe False (=~ p) (Map.lookup ( declModule d ) modulePaths)


deadToWeeds :: Analysis -> Set Declaration -> [String]
deadToWeeds analysis dead = 
  let
    warnings =
      Map.unionsWith (++) $
      foldMap
        ( \d ->
            fold $ do
              moduleFilePath <- Map.lookup ( declModule d ) ( modulePaths analysis )
              spans <- Map.lookup d ( declarationSites analysis )
              guard $ not $ null spans
              let starts = map realSrcSpanStart $ Set.toList spans
              return [ Map.singleton moduleFilePath ( liftA2 (,) starts (pure d) ) ]
        )
        dead
  in
    Map.toList warnings & concatMap \( path, declarations ) ->
      sortOn (srcLocLine . fst) declarations & map \( start, d ) ->
        case Map.lookup d (prettyPrintedType analysis) of
          Nothing -> showWeed path start d
          Just t -> showPath path start <> "(Instance) :: " <> t


showWeed :: FilePath -> RealSrcLoc -> Declaration -> String
showWeed path start d =
  showPath path start
    <> occNameString ( declOccName d)


showPath :: FilePath -> RealSrcLoc -> String
showPath path start =
  path <> ":" <> show ( srcLocLine start ) <> ": "


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
      exitWith (ExitFailure 3)


infixr 5 ==>


-- | An infix operator for logical implication
(==>) :: Bool -> Bool -> Bool
True  ==> x = x
False ==> _ = True
