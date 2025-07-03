{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Absolute.Text () where

import System.Directory (createDirectoryIfMissing, removeFile)
import System.FilePath ((</>), takeDirectory)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

import File (File(..))
import FileStorage
import Absolute.Common
import Absolute.Instances ()

-- AbsoluteFS instances for Text
instance MonadFileSave AbsoluteFS Text where
  saveFile content file = AbsoluteFS $ do
    let absolutePath = "/" </> resolvePath file
    createDirectoryIfMissing True (takeDirectory absolutePath)
    TIO.writeFile absolutePath content

instance MonadFileLoad AbsoluteFS Text where
  loadFile file = AbsoluteFS $ do
    let absolutePath = "/" </> resolvePath file
    TIO.readFile absolutePath


instance MonadFileStorage AbsoluteFS Text