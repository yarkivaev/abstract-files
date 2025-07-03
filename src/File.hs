{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module File
  ( FileName
  , Path(..)
  , File(..)
  , Segment(..)
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


