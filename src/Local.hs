module Local
  ( localFileOps
  , localSaveOps
  , localLoadOps
  ) where

import qualified Data.ByteString as BS
import System.FilePath ((</>), takeDirectory, joinPath)
import System.Directory (createDirectoryIfMissing, getCurrentDirectory, removeFile)
import Control.Monad.IO.Class (MonadIO, liftIO)

import File

-- Helper function to convert File to FilePath
getFilePath :: File -> IO FilePath
getFilePath (File (Folder dirs) (FileName name)) = 
  (</> joinPath (dirs ++ [name])) <$> getCurrentDirectory

-- Individual capability implementations
localSaveOps :: MonadIO m => SaveOps m BS.ByteString
localSaveOps = SaveOps
  { saveFile = \content file -> liftIO $ do
      path <- getFilePath file
      createDirectoryIfMissing True (takeDirectory path)
      BS.writeFile path content
  }

localLoadOps :: MonadIO m => LoadOps m BS.ByteString
localLoadOps = LoadOps
  { loadFile = \file -> liftIO $ getFilePath file >>= BS.readFile
  }

-- Combined file operations for local filesystem
localFileOps :: MonadIO m => FileOps m BS.ByteString
localFileOps = defaultFileOps
  { saveOps = localSaveOps
  , loadOps = localLoadOps
  }

