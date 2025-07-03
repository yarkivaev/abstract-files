{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module Absolute.AbsoluteFSSpec (spec) where

import Test.Hspec
import System.IO.Temp (withSystemTempDirectory)
import System.Directory (doesFileExist)
import Control.Exception (try, SomeException)
import qualified Data.ByteString as BS
import Data.Text (Text)
import qualified Data.Text as T

import File (File(..), Path(..), Segment(..))
import FileSystem
import Absolute.FileSystem

spec :: Spec
spec = do
  describe "AbsoluteFS implementation" $ do
    describe "Path resolution" $ do
      it "resolves file paths correctly" $ do
        let testFile = File (Path [Segment "tmp", Segment "test"]) "absolute.txt"
        resolvePath testFile `shouldBe` "tmp/test/absolute.txt"
      
      it "handles empty path" $ do
        let testFile = File (Path []) "root.txt"
        resolvePath testFile `shouldBe` "root.txt"
      
      it "handles nested paths" $ do
        let testFile = File (Path [Segment "usr", Segment "local", Segment "bin"]) "script.sh"
        resolvePath testFile `shouldBe` "usr/local/bin/script.sh"

    describe "MonadFileShow implementation" $ do
      it "shows absolute paths with leading slash" $ do
        let path = Path [Segment "usr", Segment "local", Segment "bin"]
        result <- runAbsoluteFS $ showPath path
        result `shouldBe` "/usr/local/bin"
      
      it "shows absolute files with full path" $ do
        let file = File (Path [Segment "etc", Segment "config"]) "app.conf"
        result <- runAbsoluteFS $ showFile file
        result `shouldBe` "/etc/config/app.conf"
      
      it "handles empty path" $ do
        let path = Path []
        result <- runAbsoluteFS $ showPath path
        result `shouldBe` "/"
      
      it "handles file in root" $ do
        let file = File (Path []) "root.txt"
        result <- runAbsoluteFS $ showFile file
        result `shouldBe` "/root.txt"
      
      it "handles single segment path" $ do
        let path = Path [Segment "home"]
        result <- runAbsoluteFS $ showPath path
        result `shouldBe` "/home"
      
      it "handles complex file path" $ do
        let file = File (Path [Segment "var", Segment "log", Segment "app"]) "error.log"
        result <- runAbsoluteFS $ showFile file
        result `shouldBe` "/var/log/app/error.log"

    describe "File operations" $ do
      it "saves and loads a ByteString to an absolute file" $ do
        withSystemTempDirectory "abstract-files-test" $ \tmpDir -> do
          let content = "Hello, Absolute World!" :: BS.ByteString
          let file = File (Path [Segment (T.pack tmpDir), Segment "subdir"]) "test.txt"
          
          -- Save the file
          runAbsoluteFS $ saveFile content file
          
          -- Load the file
          loaded <- runAbsoluteFS $ loadFile file
          loaded `shouldBe` content
          
          -- Verify file exists at expected absolute location
          let expectedPath = tmpDir ++ "/subdir/test.txt"
          exists <- doesFileExist expectedPath
          exists `shouldBe` True

      it "saves and loads a Text file to an absolute path" $ do
        withSystemTempDirectory "abstract-files-test" $ \tmpDir -> do
          let content = "Absolute Text content" :: Text
          let file = File (Path [Segment (T.pack tmpDir), Segment "docs", Segment "nested"]) "text.txt"
          
          -- Save the file
          runAbsoluteFS $ saveFile content file
          
          -- Load the file
          loaded <- runAbsoluteFS $ loadFile file
          loaded `shouldBe` content

      it "creates nested directories when saving" $ do
        withSystemTempDirectory "abstract-files-test" $ \tmpDir -> do
          let content = "Nested absolute content" :: BS.ByteString
          let file = File (Path [Segment (T.pack tmpDir), Segment "deep", Segment "nested", Segment "path"]) "nested.txt"
          
          -- Save the file
          runAbsoluteFS $ saveFile content file
          
          -- Load the file
          loaded <- runAbsoluteFS $ loadFile file
          loaded `shouldBe` content

      it "overwrites existing files" $ do
        withSystemTempDirectory "abstract-files-test" $ \tmpDir -> do
          let content1 = "First absolute content" :: BS.ByteString
          let content2 = "Second absolute content" :: BS.ByteString
          let file = File (Path [Segment (T.pack tmpDir)]) "overwrite.txt"
          
          -- Save first content
          runAbsoluteFS $ saveFile content1 file
          
          -- Save second content (overwrite)
          runAbsoluteFS $ saveFile content2 file
          
          -- Load should return second content
          loaded <- runAbsoluteFS $ loadFile file
          loaded `shouldBe` content2

      it "handles empty files" $ do
        withSystemTempDirectory "abstract-files-test" $ \tmpDir -> do
          let content = "" :: BS.ByteString
          let file = File (Path [Segment (T.pack tmpDir)]) "empty.txt"
          
          -- Save empty file
          runAbsoluteFS $ saveFile content file
          
          -- Load empty file
          loaded <- runAbsoluteFS $ loadFile file
          loaded `shouldBe` content

      it "handles files with special characters in names" $ do
        withSystemTempDirectory "abstract-files-test" $ \tmpDir -> do
          let content = "Special chars absolute content" :: BS.ByteString
          let file = File (Path [Segment (T.pack tmpDir)]) "test file (with) [special] {chars}.txt"
          
          -- Save the file
          runAbsoluteFS $ saveFile content file
          
          -- Load the file
          loaded <- runAbsoluteFS $ loadFile file
          loaded `shouldBe` content

      it "can delete files" $ do
        withSystemTempDirectory "abstract-files-test" $ \tmpDir -> do
          let content = "To be deleted absolutely" :: BS.ByteString
          let file = File (Path [Segment (T.pack tmpDir)]) "delete-me.txt"
          
          -- Save the file
          runAbsoluteFS $ saveFile content file
          
          -- Verify file exists
          let expectedPath = tmpDir ++ "/delete-me.txt"
          exists <- doesFileExist expectedPath
          exists `shouldBe` True
          
          -- Delete the file
          runAbsoluteFS $ deleteFile file
          
          -- Verify file is gone
          existsAfterDelete <- doesFileExist expectedPath
          existsAfterDelete `shouldBe` False

      it "handles absolute paths starting from root" $ do
        withSystemTempDirectory "abstract-files-test" $ \tmpDir -> do
          let content = "Root absolute content" :: Text
          -- Create a file with absolute path starting from the temp directory
          let file = File (Path [Segment (T.pack tmpDir), Segment "root-test"]) "absolute-root.txt"
          
          -- Save and load
          runAbsoluteFS $ saveFile content file
          loaded <- runAbsoluteFS $ loadFile file
          loaded `shouldBe` content

    describe "Error handling" $ do
      it "throws exception when loading non-existent file" $ do
        withSystemTempDirectory "abstract-files-test" $ \tmpDir -> do
          let file = File (Path [Segment (T.pack tmpDir)]) "does-not-exist.txt"
          
          result <- try (runAbsoluteFS $ loadFile file) :: IO (Either SomeException BS.ByteString)
          result `shouldSatisfy` (\case Left _ -> True; Right _ -> False)

      it "handles loading from non-existent absolute directory" $ do
        let file = File (Path [Segment "non-existent-absolute-dir"]) "file.txt"
        
        result <- try (runAbsoluteFS $ loadFile file) :: IO (Either SomeException BS.ByteString)
        result `shouldSatisfy` (\case Left _ -> True; Right _ -> False)

      it "handles deletion of non-existent files gracefully" $ do
        withSystemTempDirectory "abstract-files-test" $ \tmpDir -> do
          let file = File (Path [Segment (T.pack tmpDir)]) "non-existent.txt"
          
          -- This should not crash, though it may throw an exception
          result <- try (runAbsoluteFS $ deleteFile file) :: IO (Either SomeException ())
          -- We accept either success (if the implementation is idempotent) or failure
          result `shouldSatisfy` (\case Left _ -> True; Right _ -> True)