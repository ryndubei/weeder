{-# language ApplicativeDo #-}
{-# language BlockArguments #-}
{-# language FlexibleContexts #-}
{-# language NamedFieldPuns #-}
{-# language OverloadedStrings #-}
{-# language LambdaCase #-}
{-# language RecordWildCards #-}

-- | This module provides an entry point to the Weeder executable.

module Weeder.Main ( main, mainWithConfig, getHieFiles ) where

-- base
import Control.Monad ( unless, when )
import Data.List ( isSuffixOf )
import Data.Version ( showVersion )
import System.Exit ( ExitCode(..), exitWith )
import System.IO ( stderr, hPutStrLn, hPrint )

-- toml-reader
import qualified TOML

-- directory
import System.Directory ( canonicalizePath, doesDirectoryExist, doesFileExist, doesPathExist, listDirectory, withCurrentDirectory )

-- filepath
import System.FilePath ( isExtensionOf )

-- ghc
import GHC.Iface.Ext.Binary ( HieFileResult( HieFileResult, hie_file_result ), readHieFileWithVersion )
import GHC.Iface.Ext.Types ( HieFile( hie_hs_file ), hieVersion )
import GHC.Types.Name.Cache ( initNameCache, NameCache )

-- optparse-applicative
import Options.Applicative

-- text
import qualified Data.Text.IO as T

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
