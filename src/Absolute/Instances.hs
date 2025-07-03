{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Absolute.Instances () where

import System.Directory (removeFile)
import System.FilePath ((</>))
import Data.Text (Text)
import qualified Data.Text as T

import File (File(..), Path(..), Segment(..))
import FileSystem
import Absolute.Common

-- MonadFileDelete instance (shared between Text and ByteString)
instance MonadFileDelete AbsoluteFS where
  deleteFile file = AbsoluteFS $ do
    let absolutePath = "/" </> resolvePath file
    removeFile absolutePath

-- MonadFileShow instance (shared between Text and ByteString)
instance MonadFileShow AbsoluteFS where
  showPath (Path segments) = AbsoluteFS $ do
    let segmentTexts = map (\(Segment s) -> s) segments
    return $ "/" <> T.intercalate "/" segmentTexts
  
  showFile file = AbsoluteFS $ do
    let segmentTexts = map (\(Segment s) -> s) (let Path segs = filePath file in segs)
        pathText = "/" <> T.intercalate "/" segmentTexts
    return $ if pathText == "/" then "/" <> fileName file else pathText <> "/" <> fileName file