{-# LANGUAGE OverloadedStrings #-}

module LocalSpec (spec) where

import Data.String (fromString)

import Test.Hspec
import System.IO.Temp
import System.Directory
import System.FilePath ((</>), splitDirectories)
import Control.Exception (try, SomeException)
import qualified Data.ByteString as BS

import File
import Local (localFileOps)
import RelativeLocal (relativeFileOps, relativeShowOps)
import AbsoluteLocal (absoluteFileOps, absoluteShowOps)

spec :: Spec
spec = do
  describe "Local file system implementation" $ do
    describe "save and load" $ do
      it "saves and loads a ByteString to a relative file" $ do
        withSystemTempDirectory "abstract-files-test" $ \tmpDir -> do
          -- Change to temp directory
          originalDir <- getCurrentDirectory
          setCurrentDirectory tmpDir
          
          let content = "Hello, World!" :: BS.ByteString
          let fileName = "test.txt"
          let folder = Path ["subdir"]
          let file = File folder fileName
          
          -- Save the file
          saveFile (saveOps localFileOps) content file
          
          -- Load the file
          loaded <- loadFile (loadOps localFileOps) file
          loaded `shouldBe` content
          
          -- Verify file exists at expected location
          exists <- doesFileExist $ "subdir" </> "test.txt"
          exists `shouldBe` True
          
          -- Restore original directory
          setCurrentDirectory originalDir

      it "saves and loads a ByteString to a file in different directory" $ do
        withSystemTempDirectory "abstract-files-test" $ \tmpDir -> do
          -- Change to temp directory
          originalDir <- getCurrentDirectory
          setCurrentDirectory tmpDir
          
          let content = "Different dir test" :: BS.ByteString
          let fileName = "test.txt"
          let folder = Path ["different", "subdir"]
          let file = File folder fileName
          
          -- Save the file
          saveFile (saveOps localFileOps) content file
          
          -- Load the file
          loaded <- loadFile (loadOps localFileOps) file
          loaded `shouldBe` content
          
          -- Verify file exists at expected location
          exists <- doesFileExist $ "different" </> "subdir" </> "test.txt"
          exists `shouldBe` True
          
          -- Restore original directory
          setCurrentDirectory originalDir

      it "creates nested directories when saving" $ do
        withSystemTempDirectory "abstract-files-test" $ \tmpDir -> do
          -- Change to temp directory
          originalDir <- getCurrentDirectory
          setCurrentDirectory tmpDir
          
          let content = "Nested directory test" :: BS.ByteString
          let fileName = "nested.txt"
          let folder = Path ["level1", "level2", "level3"]
          let file = File folder fileName
          
          -- Directory should not exist initially
          exists <- doesDirectoryExist "level1/level2/level3"
          exists `shouldBe` False
          
          -- Save the file
          saveFile (saveOps localFileOps) content file
          
          -- Directory should now exist
          exists' <- doesDirectoryExist "level1/level2/level3"
          exists' `shouldBe` True
          
          -- File should exist and have correct content
          loaded <- loadFile (loadOps localFileOps) file
          loaded `shouldBe` content
          
          -- Restore original directory
          setCurrentDirectory originalDir

      it "overwrites existing files" $ do
        withSystemTempDirectory "abstract-files-test" $ \tmpDir -> do
          -- Change to temp directory
          originalDir <- getCurrentDirectory
          setCurrentDirectory tmpDir
          
          let content1 = "Original content" :: BS.ByteString
          let content2 = "New content" :: BS.ByteString
          let fileName = "overwrite.txt"
          let folder = Path []
          let file = File folder fileName
          
          -- Save first version
          saveFile (saveOps localFileOps) content1 file
          loaded1 <- loadFile (loadOps localFileOps) file
          loaded1 `shouldBe` content1
          
          -- Save second version
          saveFile (saveOps localFileOps) content2 file
          loaded2 <- loadFile (loadOps localFileOps) file
          loaded2 `shouldBe` content2
          
          -- Restore original directory
          setCurrentDirectory originalDir

      it "handles empty ByteStrings" $ do
        withSystemTempDirectory "abstract-files-test" $ \tmpDir -> do
          -- Change to temp directory
          originalDir <- getCurrentDirectory
          setCurrentDirectory tmpDir
          
          let content = BS.empty
          let fileName = "empty.txt"
          let folder = Path ["empty-test"]
          let file = File folder fileName
          
          -- Save empty file
          saveFile (saveOps localFileOps) content file
          
          -- Load empty file
          loaded <- loadFile (loadOps localFileOps) file
          loaded `shouldBe` content
          
          -- Verify file exists but is empty
          exists <- doesFileExist $ "empty-test" </> "empty.txt"
          exists `shouldBe` True
          size <- getFileSizeBytes $ "empty-test" </> "empty.txt"
          size `shouldBe` 0
          
          -- Restore original directory
          setCurrentDirectory originalDir

      it "handles files with special characters in names" $ do
        withSystemTempDirectory "abstract-files-test" $ \tmpDir -> do
          -- Change to temp directory
          originalDir <- getCurrentDirectory
          setCurrentDirectory tmpDir
          
          let content = "Special chars test" :: BS.ByteString
          let fileName = "test file (with) [special] {chars}.txt"
          let folder = Path ["special-dir"]
          let file = File folder fileName
          
          -- Save and load
          saveFile (saveOps localFileOps) content file
          loaded <- loadFile (loadOps localFileOps) file
          loaded `shouldBe` content
          
          -- Restore original directory
          setCurrentDirectory originalDir

    describe "absolute path operations" $ do
      it "saves and loads using absolute paths" $ do
        withSystemTempDirectory "abstract-files-test" $ \tmpDir -> do
          let content = "Absolute path content" :: BS.ByteString
          let fileName = "absolute-test.txt"
          let folder = Path (map fromString (drop 1 (splitDirectories tmpDir)) ++ ["abs-subdir"])
          let file = File folder fileName
          
          -- Save using absolute operations
          saveFile (saveOps absoluteFileOps) content file
          
          -- Load using absolute operations
          loaded <- loadFile (loadOps absoluteFileOps) file
          loaded `shouldBe` content
          
          -- Verify file exists at expected absolute location
          exists <- doesFileExist $ tmpDir </> "abs-subdir" </> "absolute-test.txt"
          exists `shouldBe` True

      it "demonstrates relative vs absolute behavior" $ do
        withSystemTempDirectory "abstract-files-test" $ \tmpDir -> do
          originalDir <- getCurrentDirectory
          setCurrentDirectory tmpDir
          
          let content = "Path test" :: BS.ByteString
          let fileName = "path-test.txt"
          let folder = Path ["test-dir"]
          let file = File folder fileName
          
          -- Save with relative operations (relative to current dir)
          saveFile (saveOps relativeFileOps) content file
          
          -- File should exist relative to current directory
          relativeExists <- doesFileExist $ "test-dir" </> "path-test.txt"
          relativeExists `shouldBe` True
          
          -- Now test absolute operations with absolute path
          let absFile = File (Path (map fromString (drop 1 (splitDirectories tmpDir)) ++ ["abs-test-dir"])) fileName
          saveFile (saveOps absoluteFileOps) content absFile
          
          -- File should exist at absolute location
          absoluteExists <- doesFileExist $ tmpDir </> "abs-test-dir" </> "path-test.txt"
          absoluteExists `shouldBe` True
          
          setCurrentDirectory originalDir

    describe "error handling" $ do
      it "throws exception when loading non-existent file" $ do
        withSystemTempDirectory "abstract-files-test" $ \tmpDir -> do
          -- Change to temp directory
          originalDir <- getCurrentDirectory
          setCurrentDirectory tmpDir
          
          let fileName = "does-not-exist.txt"
          let folder = Path ["missing"]
          let file = File folder fileName
          
          -- Attempt to load non-existent file should throw
          result <- try (loadFile (loadOps localFileOps) file) :: IO (Either SomeException BS.ByteString)
          case result of
            Left _ -> return () -- Expected
            Right _ -> expectationFailure "Expected exception when loading non-existent file"
          
          -- Restore original directory
          setCurrentDirectory originalDir

      it "handles permission errors gracefully" $ do
        -- This test is platform-dependent and may need adjustment
        pendingWith "Permission testing is platform-dependent"

    describe "ShowOps implementations" $ do
      describe "absoluteShowOps" $ do
        it "shows absolute paths with leading slash" $ do
          let path = Path ["home", "user", "documents"]
          showPath absoluteShowOps path `shouldBe` "/home/user/documents"

        it "shows absolute files with full path" $ do
          let file = File (Path ["home", "user"]) "document.txt"
          showFile absoluteShowOps file `shouldBe` "/home/user/document.txt"

        it "handles empty path" $ do
          let path = Path []
          showPath absoluteShowOps path `shouldBe` "/"

        it "handles file in root" $ do
          let file = File (Path []) "root-file.txt"
          showFile absoluteShowOps file `shouldBe` "/root-file.txt"

      describe "relativeShowOps" $ do
        it "shows relative paths without leading slash" $ do
          let path = Path ["docs", "projects"]
          showPath relativeShowOps path `shouldBe` "docs/projects"

        it "shows relative files without leading slash" $ do
          let file = File (Path ["src", "main"]) "app.hs"
          showFile relativeShowOps file `shouldBe` "src/main/app.hs"

        it "handles empty relative path" $ do
          let path = Path []
          showPath relativeShowOps path `shouldBe` ""

        it "handles file in current directory" $ do
          let file = File (Path []) "local-file.txt"
          showFile relativeShowOps file `shouldBe` "local-file.txt"

      describe "ShowOps integration with FileOps" $ do
        it "absoluteFileOps includes absoluteShowOps" $ do
          let path = Path ["test", "absolute"]
          showPath (showOps (absoluteFileOps :: FileOps IO BS.ByteString)) path `shouldBe` "/test/absolute"

        it "relativeFileOps includes relativeShowOps" $ do
          let path = Path ["test", "relative"]
          showPath (showOps (relativeFileOps :: FileOps IO BS.ByteString)) path `shouldBe` "test/relative"

        it "demonstrates context-dependent display" $ do
          let file = File (Path ["project", "src"]) "Main.hs"
          -- Same file, different context-dependent display
          showFile (showOps (absoluteFileOps :: FileOps IO BS.ByteString)) file `shouldBe` "/project/src/Main.hs"
          showFile (showOps (relativeFileOps :: FileOps IO BS.ByteString)) file `shouldBe` "project/src/Main.hs"

-- Helper function to get file size
getFileSizeBytes :: FilePath -> IO Integer
getFileSizeBytes path = fromIntegral . BS.length <$> BS.readFile path