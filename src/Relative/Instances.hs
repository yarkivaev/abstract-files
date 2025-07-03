{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Relative.Instances () where

import Control.Monad.Reader (ask)
import Control.Monad.IO.Class (liftIO)
import System.Directory (removeFile)
import System.FilePath ((</>))
import Data.Text (Text)
import qualified Data.Text as T

import File (File(..), Path(..), Segment(..))
import FileSystem
import Relative.Common

-- MonadFileDelete instance (shared between Text and ByteString)
instance MonadFileDelete RelativeFS where
  deleteFile file = RelativeFS $ do
    rootDir <- ask
    let relativePath = rootDir </> resolvePath file
    liftIO $ removeFile relativePath

-- MonadFileShow instance (shared between Text and ByteString)
instance MonadFileShow RelativeFS where
  showPath (Path segments) = RelativeFS $ do
    let segmentTexts = map (\(Segment s) -> s) segments
    return $ T.intercalate "/" segmentTexts
  
  showFile file = RelativeFS $ do
    let segmentTexts = map (\(Segment s) -> s) (let Path segs = filePath file in segs)
        pathText = T.intercalate "/" segmentTexts
    return $ if T.null pathText then fileName file else pathText <> "/" <> fileName file