{-# LANGUAGE MultiParamTypeClasses #-}

module File
  ( FileName(..)
  , Folder(..)
  , File(..)
  , SaveFile(..)
  , LoadFile(..)
  ) where

newtype FileName = FileName String

newtype Folder = Folder [String]

data File = RelativeFile Folder FileName 
          | AbsoluteFile Folder FileName

class Monad m => SaveFile m content where
  save :: content -> File -> m ()

class Monad m => LoadFile m content where
  load :: File -> m content

