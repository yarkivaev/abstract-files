{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module File
  ( FileName
  , Path(..)
  , File(..)
  , Segment
  , SaveOps(..)
  , LoadOps(..)
  , DeleteOps(..)
  , ShowOps(..)
  , FileOps(..)
  , defaultShowOps
  , defaultFileOps
  ) where

import Data.String (IsString(fromString))
import Data.Aeson (ToJSON(..), FromJSON(..), withText, withObject, (.=), (.:), object)
import Data.Text (Text)
import qualified Data.Text as T

-- Safe string that cannot contain path separators
newtype Segment = Segment Text
  deriving (Eq)

instance Show Segment where
  show (Segment t) = T.unpack t

instance IsString Segment where
  fromString s 
    | '/' `elem` s = error $ "Segment cannot contain '/': " ++ s
    | null s = error "Segment cannot be empty"
    | otherwise = Segment (T.pack s)

instance ToJSON Segment where
  toJSON (Segment t) = toJSON t

instance FromJSON Segment where
  parseJSON = withText "Segment" $ \t -> 
    pure $ Segment t

type FileName = Text

newtype Path = Path [Segment]
  deriving (Eq, ToJSON, FromJSON)

data File = File 
  { filePath :: Path
  , fileName :: FileName
  } deriving (Eq)

instance ToJSON File where
  toJSON (File path name) = object
    [ "path" .= path
    , "fileName" .= name
    ]

instance FromJSON File where
  parseJSON = withObject "File" $ \v -> File
    <$> v .: "path"
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
  , showOps   :: ShowOps
  }

-- Context-dependent showing operations
data ShowOps = ShowOps
  { showPath :: Path -> Text
  , showFile :: File -> Text
  }

-- Default implementations
defaultShowOps :: ShowOps
defaultShowOps = ShowOps
  { showPath = \_ -> error "Show path operation not implemented"
  , showFile = \_ -> error "Show file operation not implemented"
  }

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
  , showOps = defaultShowOps
  }

