{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Local () where

import qualified Data.ByteString as BS
import System.FilePath ((</>), takeDirectory, joinPath)
import System.Directory (createDirectoryIfMissing, getCurrentDirectory)
import Control.Monad.IO.Class (MonadIO, liftIO)

import File

instance MonadIO m => SaveFile m BS.ByteString where
  save content file = liftIO $ do
    path <- getFilePath file
    createDirectoryIfMissing True (takeDirectory path)
    BS.writeFile path content

instance MonadIO m => LoadFile m BS.ByteString where
  load file = liftIO $ getFilePath file >>= BS.readFile

-- Helper function to convert File to FilePath
getFilePath :: File -> IO FilePath
getFilePath (RelativeFile (Folder dirs) (FileName name)) = 
  (</> joinPath (dirs ++ [name])) <$> getCurrentDirectory
getFilePath (AbsoluteFile (Folder dirs) (FileName name)) = 
  return $ joinPath (dirs ++ [name])

