module AbsoluteLocal
  ( absoluteFileOps
  , absoluteSaveOps
  , absoluteLoadOps
  , absoluteDeleteOps
  ) where

import qualified Data.ByteString as BS
import System.FilePath (takeDirectory, joinPath, (</>))
import System.Directory (createDirectoryIfMissing, removeFile)
import Control.Monad.IO.Class (MonadIO, liftIO)

import File

-- Helper function to convert File to absolute FilePath
getAbsoluteFilePath :: File -> FilePath
getAbsoluteFilePath file = 
  let Path dirs = filePath file
      name = fileName file
  in "/" </> joinPath (map show dirs ++ [name])

-- Individual capability implementations for absolute paths
absoluteSaveOps :: MonadIO m => SaveOps m BS.ByteString
absoluteSaveOps = SaveOps
  { saveFile = \content file -> liftIO $ do
      let path = getAbsoluteFilePath file
      createDirectoryIfMissing True (takeDirectory path)
      BS.writeFile path content
  }

absoluteLoadOps :: MonadIO m => LoadOps m BS.ByteString
absoluteLoadOps = LoadOps
  { loadFile = \file -> liftIO $ BS.readFile (getAbsoluteFilePath file)
  }

absoluteDeleteOps :: MonadIO m => DeleteOps m
absoluteDeleteOps = DeleteOps
  { deleteFile = \file -> liftIO $ removeFile (getAbsoluteFilePath file)
  }

-- Combined file operations for absolute paths in local filesystem
absoluteFileOps :: MonadIO m => FileOps m BS.ByteString
absoluteFileOps = defaultFileOps
  { saveOps = absoluteSaveOps
  , loadOps = absoluteLoadOps
  , deleteOps = absoluteDeleteOps
  }