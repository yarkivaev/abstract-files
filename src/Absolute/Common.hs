{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Absolute.Common
  ( AbsoluteFS(..)
  , resolvePath
  ) where

import Control.Monad.IO.Class (MonadIO)
import System.FilePath ((</>))
import Data.Text (Text)
import qualified Data.Text as T

import File (File(..), Path(..), Segment(..))

-- AbsoluteFS implementation
newtype AbsoluteFS a = AbsoluteFS { runAbsoluteFS :: IO a }
  deriving (Functor, Applicative, Monad, MonadIO)

-- Helper function to resolve File to FilePath
resolvePath :: File -> FilePath
resolvePath (File (Path segments) fname) = 
  let segmentPaths = map (\(Segment s) -> T.unpack s) segments
      pathPart = foldr (</>) "" segmentPaths
  in pathPart </> T.unpack fname