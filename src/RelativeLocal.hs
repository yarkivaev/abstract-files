module RelativeLocal
  ( relativeFileOps
  , relativeSaveOps
  , relativeLoadOps
  , relativeDeleteOps
  ) where

import qualified Data.ByteString as BS
import System.FilePath ((</>), takeDirectory, joinPath)
import System.Directory (createDirectoryIfMissing, getCurrentDirectory, removeFile)
import Control.Monad.IO.Class (MonadIO, liftIO)

import File

-- Helper function to convert File to relative FilePath
getRelativeFilePath :: File -> IO FilePath
getRelativeFilePath (File (Path dirs) (FileName fileName)) = 
  (</> joinPath (map show dirs ++ [show fileName])) <$> getCurrentDirectory

-- Individual capability implementations for relative paths
relativeSaveOps :: MonadIO m => SaveOps m BS.ByteString
relativeSaveOps = SaveOps
  { saveFile = \content file -> liftIO $ do
      path <- getRelativeFilePath file
      createDirectoryIfMissing True (takeDirectory path)
      BS.writeFile path content
  }

relativeLoadOps :: MonadIO m => LoadOps m BS.ByteString
relativeLoadOps = LoadOps
  { loadFile = \file -> liftIO $ getRelativeFilePath file >>= BS.readFile
  }

relativeDeleteOps :: MonadIO m => DeleteOps m
relativeDeleteOps = DeleteOps
  { deleteFile = \file -> liftIO $ getRelativeFilePath file >>= removeFile
  }

-- Combined file operations for relative paths in local filesystem
relativeFileOps :: MonadIO m => FileOps m BS.ByteString
relativeFileOps = defaultFileOps
  { saveOps = relativeSaveOps
  , loadOps = relativeLoadOps
  , deleteOps = relativeDeleteOps
  }