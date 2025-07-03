{-# LANGUAGE OverloadedStrings #-}

module Absolute.FileSystem
  ( -- * Core types and functions
    AbsoluteFS(..)
  , resolvePath
    -- * Text instances (imported for side effects)
  , module Absolute.Text
    -- * ByteString instances (imported for side effects)  
  , module Absolute.ByteString
  ) where

import Absolute.Common (AbsoluteFS(..), resolvePath)
import Absolute.Instances ()
import Absolute.Text
import Absolute.ByteString