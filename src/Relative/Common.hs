{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Relative.Common
  ( RelativeFS(..)
  , runRelativeFS
  , resolvePath
  ) where

import Control.Monad.Reader (ReaderT, runReaderT)
import Control.Monad.IO.Class (MonadIO)
import System.FilePath ((</>))
import Data.Text (Text)
import qualified Data.Text as T

import File (File(..), Path(..), Segment(..))

-- RelativeFS implementation
newtype RelativeFS a = RelativeFS { unRelativeFS :: ReaderT FilePath IO a }
  deriving (Functor, Applicative, Monad, MonadIO)

-- Helper function to resolve File to FilePath
resolvePath :: File -> FilePath
resolvePath (File (Path segments) fname) = 
  let segmentPaths = map (\(Segment s) -> T.unpack s) segments
      pathPart = foldr (</>) "" segmentPaths
  in pathPart </> T.unpack fname

-- Convenience function to run RelativeFS with a root directory
runRelativeFS :: FilePath -> RelativeFS a -> IO a
runRelativeFS rootDir (RelativeFS action) = runReaderT action rootDir