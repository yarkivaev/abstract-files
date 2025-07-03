{-# LANGUAGE OverloadedStrings #-}

module Relative.FileSystem
  ( -- * Core types and functions
    RelativeFS(..)
  , runRelativeFS
    -- * Text instances (imported for side effects)
  , module Relative.Text
    -- * ByteString instances (imported for side effects)
  , module Relative.ByteString
  ) where

import Relative.Common (RelativeFS(..), runRelativeFS)
import Relative.Instances ()
import Relative.Text
import Relative.ByteString