{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Absolute.ByteString () where

import System.Directory (createDirectoryIfMissing, removeFile)
import System.FilePath ((</>), takeDirectory)
import qualified Data.ByteString as BS

import File (File(..))
import FileStorage
import Absolute.Common
import Absolute.Instances ()

-- AbsoluteFS instances for ByteString
instance MonadFileSave AbsoluteFS BS.ByteString where
  saveFile content file = AbsoluteFS $ do
    let absolutePath = "/" </> resolvePath file
    createDirectoryIfMissing True (takeDirectory absolutePath)
    BS.writeFile absolutePath content

instance MonadFileLoad AbsoluteFS BS.ByteString where
  loadFile file = AbsoluteFS $ do
    let absolutePath = "/" </> resolvePath file
    BS.readFile absolutePath

instance MonadFileStorage AbsoluteFS BS.ByteString