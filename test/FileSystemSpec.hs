{-# LANGUAGE OverloadedStrings #-}

module FileSystemSpec (spec) where

import Test.Hspec
import System.IO.Temp (withSystemTempDirectory)
import System.Directory (doesFileExist)
import Data.Text (Text)

import File (File(..), Path(..), Segment(..))
import FileSystem
import Relative.FileSystem
import Absolute.FileSystem

spec :: Spec
spec = do
  describe "FileSystem" $ do
    describe "RelativeFS" $ do
      it "can save and load text files" $ do
        withSystemTempDirectory "test" $ \tmpDir -> do
          let testFile = File (Path [Segment "subdir"]) "test.txt"
              testContent = "Hello, World!" :: Text
          
          -- Save file
          runRelativeFS tmpDir $ saveFile testContent testFile
          
          -- Check file exists
          let expectedPath = tmpDir ++ "/subdir/test.txt"
          exists <- doesFileExist expectedPath
          exists `shouldBe` True
          
          -- Load file
          loadedContent <- runRelativeFS tmpDir $ loadFile testFile
          loadedContent `shouldBe` testContent
          
          -- Delete file
          runRelativeFS tmpDir $ deleteFile testFile
          existsAfterDelete <- doesFileExist expectedPath
          existsAfterDelete `shouldBe` False

    describe "AbsoluteFS" $ do
      it "creates absolute paths correctly" $ do
        let testFile = File (Path [Segment "tmp", Segment "test"]) "absolute.txt"
        
        -- Test path resolution function
        resolvePath testFile `shouldBe` "tmp/test/absolute.txt"

  describe "Path resolution" $ do
    it "resolves paths correctly" $ do
      let testFile = File (Path [Segment "dir1", Segment "dir2"]) "file.txt"
      resolvePath testFile `shouldBe` "dir1/dir2/file.txt"
    
    it "handles empty path" $ do
      let testFile = File (Path []) "file.txt"
      resolvePath testFile `shouldBe` "file.txt"
    
    it "handles single segment path" $ do
      let testFile = File (Path [Segment "single"]) "file.txt"
      resolvePath testFile `shouldBe` "single/file.txt"