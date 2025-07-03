{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module Relative.RelativeFSSpec (spec) where

import Test.Hspec
import System.IO.Temp (withSystemTempDirectory)
import System.Directory (doesFileExist)
import Control.Exception (try, SomeException)
import qualified Data.ByteString as BS
import Data.Text (Text)

import File (File(..), Path(..), Segment(..))
import FileStorage
import Relative.FileSystem

spec :: Spec
spec = do
  describe "RelativeFS implementation" $ do
    describe "File operations" $ do
      it "saves and loads a ByteString to a relative file" $ do
        withSystemTempDirectory "abstract-files-test" $ \tmpDir -> do
          let content = "Hello, World!" :: BS.ByteString
          let file = File (Path [Segment "subdir"]) "test.txt"
          
          -- Save the file
          runRelativeFS tmpDir $ saveFile content file
          
          -- Load the file
          loaded <- runRelativeFS tmpDir $ loadFile file
          loaded `shouldBe` content
          
          -- Verify file exists at expected location
          let expectedPath = tmpDir ++ "/subdir/test.txt"
          exists <- doesFileExist expectedPath
          exists `shouldBe` True

      it "saves and loads a Text file to a different directory" $ do
        withSystemTempDirectory "abstract-files-test" $ \tmpDir -> do
          let content = "Text content" :: Text
          let file = File (Path [Segment "docs", Segment "nested"]) "text.txt"
          
          -- Save the file
          runRelativeFS tmpDir $ saveFile content file
          
          -- Load the file
          loaded <- runRelativeFS tmpDir $ loadFile file
          loaded `shouldBe` content

      it "creates nested directories when saving" $ do
        withSystemTempDirectory "abstract-files-test" $ \tmpDir -> do
          let content = "Nested content" :: BS.ByteString
          let file = File (Path [Segment "deep", Segment "nested", Segment "path"]) "nested.txt"
          
          -- Save the file
          runRelativeFS tmpDir $ saveFile content file
          
          -- Load the file
          loaded <- runRelativeFS tmpDir $ loadFile file
          loaded `shouldBe` content

      it "overwrites existing files" $ do
        withSystemTempDirectory "abstract-files-test" $ \tmpDir -> do
          let content1 = "First content" :: BS.ByteString
          let content2 = "Second content" :: BS.ByteString
          let file = File (Path []) "overwrite.txt"
          
          -- Save first content
          runRelativeFS tmpDir $ saveFile content1 file
          
          -- Save second content (overwrite)
          runRelativeFS tmpDir $ saveFile content2 file
          
          -- Load should return second content
          loaded <- runRelativeFS tmpDir $ loadFile file
          loaded `shouldBe` content2

      it "handles empty files" $ do
        withSystemTempDirectory "abstract-files-test" $ \tmpDir -> do
          let content = "" :: BS.ByteString
          let file = File (Path []) "empty.txt"
          
          -- Save empty file
          runRelativeFS tmpDir $ saveFile content file
          
          -- Load empty file
          loaded <- runRelativeFS tmpDir $ loadFile file
          loaded `shouldBe` content

      it "handles files with special characters in names" $ do
        withSystemTempDirectory "abstract-files-test" $ \tmpDir -> do
          let content = "Special chars content" :: BS.ByteString
          let file = File (Path []) "test file (with) [special] {chars}.txt"
          
          -- Save the file
          runRelativeFS tmpDir $ saveFile content file
          
          -- Load the file
          loaded <- runRelativeFS tmpDir $ loadFile file
          loaded `shouldBe` content

      it "can delete files" $ do
        withSystemTempDirectory "abstract-files-test" $ \tmpDir -> do
          let content = "To be deleted" :: BS.ByteString
          let file = File (Path []) "delete-me.txt"
          
          -- Save the file
          runRelativeFS tmpDir $ saveFile content file
          
          -- Verify file exists
          let expectedPath = tmpDir ++ "/delete-me.txt"
          exists <- doesFileExist expectedPath
          exists `shouldBe` True
          
          -- Delete the file
          runRelativeFS tmpDir $ deleteFile file
          
          -- Verify file is gone
          existsAfterDelete <- doesFileExist expectedPath
          existsAfterDelete `shouldBe` False

    describe "Error handling" $ do
      it "throws exception when loading non-existent file" $ do
        withSystemTempDirectory "abstract-files-test" $ \tmpDir -> do
          let file = File (Path []) "does-not-exist.txt"
          
          result <- try (runRelativeFS tmpDir $ loadFile file) :: IO (Either SomeException BS.ByteString)
          result `shouldSatisfy` (\case Left _ -> True; Right _ -> False)

      it "handles loading from non-existent directory" $ do
        withSystemTempDirectory "abstract-files-test" $ \tmpDir -> do
          let file = File (Path [Segment "non-existent"]) "file.txt"
          
          result <- try (runRelativeFS tmpDir $ loadFile file) :: IO (Either SomeException BS.ByteString)
          result `shouldSatisfy` (\case Left _ -> True; Right _ -> False)

    describe "MonadFileShow implementation" $ do
      it "shows relative paths without leading slash" $ do
        let path = Path [Segment "src", Segment "main"]
        result <- runRelativeFS "/tmp" $ showPath path
        result `shouldBe` "src/main"
      
      it "shows relative files without leading slash" $ do
        let file = File (Path [Segment "docs"]) "readme.md"
        result <- runRelativeFS "/tmp" $ showFile file
        result `shouldBe` "docs/readme.md"
      
      it "handles empty relative path" $ do
        let path = Path []
        result <- runRelativeFS "/tmp" $ showPath path
        result `shouldBe` ""
      
      it "handles file in current directory" $ do
        let file = File (Path []) "current.txt"
        result <- runRelativeFS "/tmp" $ showFile file
        result `shouldBe` "current.txt"
      
      it "handles single segment path" $ do
        let path = Path [Segment "src"]
        result <- runRelativeFS "/tmp" $ showPath path
        result `shouldBe` "src"
      
      it "handles complex file path" $ do
        let file = File (Path [Segment "test", Segment "unit", Segment "specs"]) "MySpec.hs"
        result <- runRelativeFS "/tmp" $ showFile file
        result `shouldBe` "test/unit/specs/MySpec.hs"

    describe "Different root directories" $ do
      it "works with different root directories" $ do
        let content = "Test content" :: BS.ByteString
        let file = File (Path [Segment "test"]) "file.txt"
        
        withSystemTempDirectory "root1" $ \root1 -> do
          withSystemTempDirectory "root2" $ \root2 -> do
            -- Save to first root
            runRelativeFS root1 $ saveFile content file
            
            -- Try to load from second root (should fail)
            result <- try (runRelativeFS root2 $ loadFile file) :: IO (Either SomeException BS.ByteString)
            result `shouldSatisfy` (\case Left _ -> True; Right _ -> False)
            
            -- Load from first root (should succeed)
            loaded <- runRelativeFS root1 $ loadFile file
            loaded `shouldBe` content