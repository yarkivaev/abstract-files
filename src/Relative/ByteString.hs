{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Relative.ByteString () where

import Control.Monad.Reader (ask)
import Control.Monad.IO.Class (liftIO)
import System.Directory (createDirectoryIfMissing)
import System.FilePath ((</>), takeDirectory)
import qualified Data.ByteString as BS

import File (File(..))
import FileSystem
import Relative.Common
import Relative.Instances ()

-- RelativeFS instances for ByteString
instance MonadFileSave RelativeFS BS.ByteString where
  saveFile content file = RelativeFS $ do
    rootDir <- ask
    let relativePath = rootDir </> resolvePath file
    liftIO $ do
      createDirectoryIfMissing True (takeDirectory relativePath)
      BS.writeFile relativePath content

instance MonadFileLoad RelativeFS BS.ByteString where
  loadFile file = RelativeFS $ do
    rootDir <- ask
    let relativePath = rootDir </> resolvePath file
    liftIO $ BS.readFile relativePath

instance MonadFileSystem RelativeFS BS.ByteString