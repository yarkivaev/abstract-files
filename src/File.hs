{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module File
  ( FileName(..)
  , Folder(..)
  , File(..)
  , SafeString(..)
  , SaveOps(..)
  , LoadOps(..)
  , DeleteOps(..)
  , FileOps(..)
  , defaultFileOps
  ) where

import Data.String (IsString(fromString))
import Data.Aeson (ToJSON(..), FromJSON(..), withText, withObject, (.=), (.:), object)
import qualified Data.Text as T

-- Safe string that cannot contain path separators
newtype SafeString = SafeString String
  deriving (Show, Eq)

instance IsString SafeString where
  fromString s 
    | '/' `elem` s = error $ "SafeString cannot contain '/': " ++ s
    | null s = error "SafeString cannot be empty"
    | otherwise = SafeString s

instance ToJSON SafeString where
  toJSON (SafeString s) = toJSON s

instance FromJSON SafeString where
  parseJSON = withText "SafeString" $ \t -> 
    pure $ fromString (T.unpack t)


newtype FileName = FileName SafeString
  deriving (Show, Eq, IsString, ToJSON, FromJSON)

newtype Folder = Folder [SafeString]
  deriving (Show, Eq, ToJSON, FromJSON)

data File = File Folder FileName
  deriving (Show, Eq)

instance ToJSON File where
  toJSON (File folder fileName) = object
    [ "folder" .= folder
    , "fileName" .= fileName
    ]

instance FromJSON File where
  parseJSON = withObject "File" $ \v -> File
    <$> v .: "folder"
    <*> v .: "fileName"

-- Capability records
data SaveOps m content = SaveOps
  { saveFile :: content -> File -> m ()
  }

data LoadOps m content = LoadOps
  { loadFile :: File -> m content
  }

data DeleteOps m = DeleteOps
  { deleteFile :: File -> m ()
  }

-- Combined capability record
data FileOps m content = FileOps
  { saveOps   :: SaveOps m content
  , loadOps   :: LoadOps m content
  , deleteOps :: DeleteOps m
  }

-- Default implementation with error messages
defaultFileOps :: FileOps m content
defaultFileOps = FileOps
  { saveOps = SaveOps
      { saveFile = \_ _ -> error "Save operation not implemented"
      }
  , loadOps = LoadOps
      { loadFile = \_ -> error "Load operation not implemented"
      }
  , deleteOps = DeleteOps
      { deleteFile = \_ -> error "Delete operation not implemented"
      }
  }

