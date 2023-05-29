{-# LANGUAGE LambdaCase #-}
import qualified Weeder.Config
import qualified Weeder.Main
import qualified Dhall
import qualified Data.Text as T

import System.Directory
import System.FilePath
import System.IO.Silently (hCapture_)
import System.IO (stdout)
import Test.Hspec
import Control.Monad (zipWithM_)
import Control.Exception (handle)
import System.Exit (ExitCode(..))

main :: IO ()
main = do
  stdoutFiles <- discoverIntegrationTests
  let hieDirectories = map dropExtension stdoutFiles
  hspec $
    describe "Integration tests" $
      zipWithM_ integrationTestSpec stdoutFiles hieDirectories

integrationTestSpec :: FilePath -> FilePath -> Spec
integrationTestSpec stdoutFile hieDirectory = do
  it ("produces the expected output for " ++ hieDirectory) $ do
    expectedOutput <- readFile stdoutFile
    actualOutput <- integrationTestOutput hieDirectory
    actualOutput `shouldBe` expectedOutput

discoverIntegrationTests :: IO [FilePath]
discoverIntegrationTests = do
  contents <- listDirectory "./test/Spec"
  pure . map ("./test/Spec" </>) $ filter (".stdout" `isExtensionOf`) contents

integrationTestOutput :: FilePath -> IO String
integrationTestOutput hieDirectory = hCapture_ [stdout] $ ignoreExit $
  Dhall.input Weeder.Config.config (T.pack dhallFile) >>= 
    Weeder.Main.mainWithConfig ".hie" [hieDirectory] True
  where 
    dhallFile = hieDirectory <.> ".dhall"

ignoreExit :: IO () -> IO ()
ignoreExit = handle (\case ExitFailure _ -> pure (); ExitSuccess -> pure ())