{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module FileStorage
  ( MonadFileSave(..)
  , MonadFileLoad(..)
  , MonadFileDelete(..)
  , MonadFileShow(..)
  , MonadFileStorage
  ) where

import Data.Text (Text)

import File (File(..), Path(..))

-- Typeclasses for file operations
class Monad m => MonadFileSave m c where
  saveFile :: c -> File -> m ()

class Monad m => MonadFileLoad m c where
  loadFile :: File -> m c

class Monad m => MonadFileDelete m where
  deleteFile :: File -> m ()

class Monad m => MonadFileShow m where
  showPath :: Path -> m Text
  showFile :: File -> m Text

class (MonadFileSave m c, MonadFileLoad m c, MonadFileDelete m, MonadFileShow m) => MonadFileStorage m c

