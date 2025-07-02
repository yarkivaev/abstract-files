module Local
  ( -- * Re-exports from RelativeLocal
    module RelativeLocal
    -- * Re-exports from AbsoluteLocal  
  , module AbsoluteLocal
    -- * Combined operations (backward compatibility)
  , localFileOps
  , localSaveOps
  , localLoadOps
  , localDeleteOps
  ) where

import qualified Data.ByteString as BS
import Control.Monad.IO.Class (MonadIO)

import File
import RelativeLocal
import AbsoluteLocal

-- Backward compatibility: use relative operations as default
localFileOps :: MonadIO m => FileOps m BS.ByteString
localFileOps = relativeFileOps

localSaveOps :: MonadIO m => SaveOps m BS.ByteString
localSaveOps = relativeSaveOps

localLoadOps :: MonadIO m => LoadOps m BS.ByteString
localLoadOps = relativeLoadOps

localDeleteOps :: MonadIO m => DeleteOps m
localDeleteOps = relativeDeleteOps