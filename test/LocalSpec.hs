{-# LANGUAGE OverloadedStrings #-}

module LocalSpec (spec) where

import Test.Hspec
import System.IO.Temp
import System.Directory
import System.FilePath
import Control.Exception (try, SomeException)
import qualified Data.ByteString as BS

import File
import Local ()

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
          let fileName = FileName "test.txt"
          let folder = Folder ["subdir"]
          let file = RelativeFile folder fileName
          
          -- Save the file
          save content file
          
          -- Load the file
          loaded <- load file
          loaded `shouldBe` content
          
          -- Verify file exists at expected location
          exists <- doesFileExist $ "subdir" </> "test.txt"
          exists `shouldBe` True
          
          -- Restore original directory
          setCurrentDirectory originalDir

      it "saves and loads a ByteString to an absolute file" $ do
        withSystemTempDirectory "abstract-files-test" $ \tmpDir -> do
          let content = "Absolute path test" :: BS.ByteString
          let fileName = FileName "absolute-test.txt"
          let folder = Folder [tmpDir, "absolute-subdir"]
          let file = AbsoluteFile folder fileName
          
          -- Save the file
          save content file
          
          -- Load the file
          loaded <- load file
          loaded `shouldBe` content
          
          -- Verify file exists at expected location
          exists <- doesFileExist $ tmpDir </> "absolute-subdir" </> "absolute-test.txt"
          exists `shouldBe` True

      it "creates nested directories when saving" $ do
        withSystemTempDirectory "abstract-files-test" $ \tmpDir -> do
          -- Change to temp directory
          originalDir <- getCurrentDirectory
          setCurrentDirectory tmpDir
          
          let content = "Nested directory test" :: BS.ByteString
          let fileName = FileName "nested.txt"
          let folder = Folder ["level1", "level2", "level3"]
          let file = RelativeFile folder fileName
          
          -- Directory should not exist initially
          exists <- doesDirectoryExist "level1/level2/level3"
          exists `shouldBe` False
          
          -- Save the file
          save content file
          
          -- Directory should now exist
          exists' <- doesDirectoryExist "level1/level2/level3"
          exists' `shouldBe` True
          
          -- File should exist and have correct content
          loaded <- load file
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
          let fileName = FileName "overwrite.txt"
          let folder = Folder []
          let file = RelativeFile folder fileName
          
          -- Save first version
          save content1 file
          loaded1 <- load file
          loaded1 `shouldBe` content1
          
          -- Save second version
          save content2 file
          loaded2 <- load file
          loaded2 `shouldBe` content2
          
          -- Restore original directory
          setCurrentDirectory originalDir

      it "handles empty ByteStrings" $ do
        withSystemTempDirectory "abstract-files-test" $ \tmpDir -> do
          -- Change to temp directory
          originalDir <- getCurrentDirectory
          setCurrentDirectory tmpDir
          
          let content = BS.empty
          let fileName = FileName "empty.txt"
          let folder = Folder ["empty-test"]
          let file = RelativeFile folder fileName
          
          -- Save empty file
          save content file
          
          -- Load empty file
          loaded <- load file
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
          let fileName = FileName "test file (with) [special] {chars}.txt"
          let folder = Folder ["special-dir"]
          let file = RelativeFile folder fileName
          
          -- Save and load
          save content file
          loaded <- load file
          loaded `shouldBe` content
          
          -- Restore original directory
          setCurrentDirectory originalDir

    describe "error handling" $ do
      it "throws exception when loading non-existent file" $ do
        withSystemTempDirectory "abstract-files-test" $ \tmpDir -> do
          -- Change to temp directory
          originalDir <- getCurrentDirectory
          setCurrentDirectory tmpDir
          
          let fileName = FileName "does-not-exist.txt"
          let folder = Folder ["missing"]
          let file = RelativeFile folder fileName
          
          -- Attempt to load non-existent file should throw
          result <- try (load file) :: IO (Either SomeException BS.ByteString)
          case result of
            Left _ -> return () -- Expected
            Right _ -> expectationFailure "Expected exception when loading non-existent file"
          
          -- Restore original directory
          setCurrentDirectory originalDir

      it "handles permission errors gracefully" $ do
        -- This test is platform-dependent and may need adjustment
        pendingWith "Permission testing is platform-dependent"

-- Helper function to get file size
getFileSizeBytes :: FilePath -> IO Integer
getFileSizeBytes path = fromIntegral . BS.length <$> BS.readFile path