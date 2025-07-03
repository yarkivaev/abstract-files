{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Relative.Text () where

import Control.Monad.Reader (ask)
import Control.Monad.IO.Class (liftIO)
import System.Directory (createDirectoryIfMissing, removeFile)
import System.FilePath ((</>), takeDirectory)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

import File (File(..))
import FileStorage
import Relative.Common
import Relative.Instances ()

-- RelativeFS instances for Text
instance MonadFileSave RelativeFS Text where
  saveFile content file = RelativeFS $ do
    rootDir <- ask
    let relativePath = rootDir </> resolvePath file
    liftIO $ do
      createDirectoryIfMissing True (takeDirectory relativePath)
      TIO.writeFile relativePath content

instance MonadFileLoad RelativeFS Text where
  loadFile file = RelativeFS $ do
    rootDir <- ask
    let relativePath = rootDir </> resolvePath file
    liftIO $ TIO.readFile relativePath


instance MonadFileStorage RelativeFS Text