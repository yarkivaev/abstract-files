{-# LANGUAGE OverloadedStrings #-}

module FileSpec (spec) where

import Test.Hspec
import Control.Exception (evaluate)
import Data.Aeson (encode, decode)

import File

-- Helper for equality testing without Show
shouldEqual :: (Eq a, HasCallStack) => a -> a -> Expectation
shouldEqual actual expected = 
  if actual == expected 
    then return ()
    else expectationFailure "Values are not equal"

spec :: Spec
spec = do
  describe "File type safety" $ do
    describe "Segment validation" $ do
      it "accepts valid strings without slashes" $ do
        let validName = "valid-filename.txt" :: Segment
        show validName `shouldBe` "valid-filename.txt"

      it "rejects strings containing forward slash" $ do
        evaluate ("invalid/file.txt" :: Segment) `shouldThrow` anyErrorCall

      it "rejects empty strings" $ do
        evaluate ("" :: Segment) `shouldThrow` anyErrorCall

    describe "fileName validation" $ do
      it "accepts valid filenames" $ do
        let validFile = "document.pdf" :: FileName
        validFile `shouldBe` "document.pdf"

      it "allows any strings including slashes" $ do
        let fileName = "path/to/file.txt" :: FileName
        fileName `shouldBe` "path/to/file.txt"

    describe "Path validation" $ do
      it "accepts valid folder components" $ do
        let validPath = Path ["docs", "projects", "2024"]
        validPath `shouldEqual` Path ["docs", "projects", "2024"]

      it "rejects folder components with slashes" $ do
        evaluate ("invalid/path" :: Segment) `shouldThrow` anyErrorCall

      it "rejects empty folder components" $ do
        evaluate ("" :: Segment) `shouldThrow` anyErrorCall

    describe "File construction" $ do
      it "creates valid files successfully" $ do
        let file = File (Path ["home", "user"]) "document.txt"
        file `shouldEqual` File (Path ["home", "user"]) "document.txt"

      it "prevents files with invalid paths at compile/runtime" $ do
        evaluate ("home/invalid" :: Segment) `shouldThrow` anyErrorCall

  describe "JSON serialization" $ do
    it "encodes and decodes File to/from JSON" $ do
      let file = File (Path ["home", "user"]) "document.txt"
      let encoded = encode file
      case decode encoded of
        Just decoded -> decoded `shouldEqual` file
        Nothing -> expectationFailure "Failed to decode File from JSON"

    it "encodes and decodes Segment to/from JSON" $ do
      let safeStr = "valid-string" :: Segment
      let encoded = encode safeStr
      decode encoded `shouldBe` Just safeStr

    it "encodes and decodes FileName to/from JSON" $ do
      let name = "test.txt" :: FileName
      let encoded = encode name
      decode encoded `shouldBe` Just name

    it "encodes and decodes Path to/from JSON" $ do
      let folder = Path ["path", "to", "folder"]
      let encoded = encode folder
      case decode encoded of
        Just decoded -> decoded `shouldEqual` folder
        Nothing -> expectationFailure "Failed to decode Path from JSON"

