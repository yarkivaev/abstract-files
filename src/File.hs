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

-- Safe string that cannot contain path separators
newtype SafeString = SafeString String
  deriving (Show, Eq)

instance IsString SafeString where
  fromString s 
    | '/' `elem` s = error $ "SafeString cannot contain '/': " ++ s
    | null s = error "SafeString cannot be empty"
    | otherwise = SafeString s


newtype FileName = FileName SafeString
  deriving (Show, Eq, IsString)

newtype Folder = Folder [SafeString]
  deriving (Show, Eq)

data File = File Folder FileName
  deriving (Show, Eq)

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

