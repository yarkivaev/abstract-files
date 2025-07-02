{-# LANGUAGE OverloadedStrings #-}

module FileSpec (spec) where

import Test.Hspec
import Control.Exception (evaluate)
import Data.Aeson (encode, decode)

import File

spec :: Spec
spec = do
  describe "File type safety" $ do
    describe "SafeString validation" $ do
      it "accepts valid strings without slashes" $ do
        let validName = "valid-filename.txt" :: SafeString
        validName `shouldBe` SafeString "valid-filename.txt"

      it "rejects strings containing forward slash" $ do
        evaluate ("invalid/file.txt" :: SafeString) `shouldThrow` anyErrorCall

      it "rejects empty strings" $ do
        evaluate ("" :: SafeString) `shouldThrow` anyErrorCall

    describe "FileName validation" $ do
      it "accepts valid filenames" $ do
        let validFile = FileName "document.pdf"
        validFile `shouldBe` FileName (SafeString "document.pdf")

      it "rejects filenames with slashes" $ do
        evaluate (FileName "path/to/file.txt") `shouldThrow` anyErrorCall

    describe "Folder validation" $ do
      it "accepts valid folder components" $ do
        let validFolder = Folder ["docs", "projects", "2024"]
        validFolder `shouldBe` Folder [SafeString "docs", SafeString "projects", SafeString "2024"]

      it "rejects folder components with slashes" $ do
        evaluate ("invalid/path" :: SafeString) `shouldThrow` anyErrorCall

      it "rejects empty folder components" $ do
        evaluate ("" :: SafeString) `shouldThrow` anyErrorCall

    describe "File construction" $ do
      it "creates valid files successfully" $ do
        let file = File (Folder ["home", "user"]) (FileName "document.txt")
        file `shouldBe` File (Folder [SafeString "home", SafeString "user"]) 
                           (FileName (SafeString "document.txt"))

      it "prevents files with invalid paths at compile/runtime" $ do
        evaluate ("home/invalid" :: SafeString) `shouldThrow` anyErrorCall

  describe "JSON serialization" $ do
    it "encodes and decodes File to/from JSON" $ do
      let file = File (Folder ["home", "user"]) (FileName "document.txt")
      let encoded = encode file
      decode encoded `shouldBe` Just file

    it "encodes and decodes SafeString to/from JSON" $ do
      let safeStr = "valid-string" :: SafeString
      let encoded = encode safeStr
      decode encoded `shouldBe` Just safeStr

    it "encodes and decodes FileName to/from JSON" $ do
      let fileName = FileName "test.txt"
      let encoded = encode fileName
      decode encoded `shouldBe` Just fileName

    it "encodes and decodes Folder to/from JSON" $ do
      let folder = Folder ["path", "to", "folder"]
      let encoded = encode folder
      decode encoded `shouldBe` Just folder