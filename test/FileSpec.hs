{-# LANGUAGE OverloadedStrings #-}

module FileSpec (spec) where

import Test.Hspec
import Control.Exception (evaluate)
import Data.Aeson (encode, decode)

import File

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

    describe "FileName validation" $ do
      it "accepts valid filenames" $ do
        let validFile = FileName "document.pdf"
        show validFile `shouldBe` show (FileName "document.pdf")

      it "rejects filenames with slashes" $ do
        evaluate (FileName "path/to/file.txt") `shouldThrow` anyErrorCall

    describe "Path validation" $ do
      it "accepts valid folder components" $ do
        let validPath = Path ["docs", "projects", "2024"]
        show validPath `shouldBe` show (Path ["docs", "projects", "2024"])

      it "rejects folder components with slashes" $ do
        evaluate ("invalid/path" :: Segment) `shouldThrow` anyErrorCall

      it "rejects empty folder components" $ do
        evaluate ("" :: Segment) `shouldThrow` anyErrorCall

    describe "File construction" $ do
      it "creates valid files successfully" $ do
        let file = File (Path ["home", "user"]) (FileName "document.txt")
        show file `shouldBe` show (File (Path ["home", "user"]) (FileName "document.txt"))

      it "prevents files with invalid paths at compile/runtime" $ do
        evaluate ("home/invalid" :: Segment) `shouldThrow` anyErrorCall

  describe "JSON serialization" $ do
    it "encodes and decodes File to/from JSON" $ do
      let file = File (Path ["home", "user"]) (FileName "document.txt")
      let encoded = encode file
      decode encoded `shouldBe` Just file

    it "encodes and decodes Segment to/from JSON" $ do
      let safeStr = "valid-string" :: Segment
      let encoded = encode safeStr
      decode encoded `shouldBe` Just safeStr

    it "encodes and decodes FileName to/from JSON" $ do
      let fileName = FileName "test.txt"
      let encoded = encode fileName
      decode encoded `shouldBe` Just fileName

    it "encodes and decodes Path to/from JSON" $ do
      let folder = Path ["path", "to", "folder"]
      let encoded = encode folder
      decode encoded `shouldBe` Just folder