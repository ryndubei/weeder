{-# language NamedFieldPuns #-}
{-# language CPP #-}

module Weeder.Compat ( readHieFiles ) where


-- ghc
import GHC.Iface.Ext.Types ( HieFile, hieVersion )
import GHC.Types.Name.Cache ( initNameCache, NameCache )
import GHC.Iface.Ext.Binary 
  ( HieFileResult( HieFileResult, hie_file_result )
  , readHieFileWithVersion 
  )
#if !MIN_VERSION_ghc(9,3,0)
import GHC.Iface.Ext.Binary ( NameCacheUpdater(..) )
import GHC.Types.Unique.Supply ( mkSplitUniqSupply )

-- base
import Data.IORef ( IORef, newIORef, atomicModifyIORef )
#endif
import System.Exit ( exitFailure )

-- | Read multiple .hie files, exiting if any are of an incompatible version.
readHieFiles :: (Traversable f) => f FilePath -> IO (f HieFile)
readHieFiles hieFilePaths = do

#if MIN_VERSION_ghc(9,3,0)
  nameCache <- initNameCache 'z' []
#else
  us <-
    mkSplitUniqSupply 'z'
  nameCache <-
    newIORef (initNameCache us [])
#endif

  mapM (readCompatibleHieFileOrExit nameCache) hieFilePaths

-- | Read a .hie file, exiting if it's an incompatible version.
#if MIN_VERSION_ghc(9,3,0)
readCompatibleHieFileOrExit :: NameCache -> FilePath -> IO HieFile
readCompatibleHieFileOrExit nameCache path = do
  res <- readHieFileWithVersion (\(v, _) -> v == hieVersion) nameCache path
#else
readCompatibleHieFileOrExit :: IORef NameCache -> FilePath -> IO HieFile
readCompatibleHieFileOrExit nameCache path = do
  res <- readHieFileWithVersion (\(v, _) -> v == hieVersion) (NCU $ atomicModifyIORef nameCache) path
#endif
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
      exitFailure
