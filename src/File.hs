module File
  ( FileName(..)
  , Folder(..)
  , File(..)
  , SaveOps(..)
  , LoadOps(..)
  , DeleteOps(..)
  , FileOps(..)
  , defaultFileOps
  ) where

newtype FileName = FileName String

newtype Folder = Folder [String]

data File = File Folder FileName

-- Capability records
data SaveOps m content = SaveOps
  { saveFile :: content -> File -> m ()
  }

data LoadOps m content = LoadOps
  { loadFile :: File -> m content
  }

data DeleteOps m = DeleteOps
  { deleteFile :: File -> m ()
  }

-- Combined capability record
data FileOps m content = FileOps
  { saveOps   :: SaveOps m content
  , loadOps   :: LoadOps m content
  , deleteOps :: DeleteOps m
  }

-- Default implementation with error messages
defaultFileOps :: FileOps m content
defaultFileOps = FileOps
  { saveOps = SaveOps
      { saveFile = \_ _ -> error "Save operation not implemented"
      }
  , loadOps = LoadOps
      { loadFile = \_ -> error "Load operation not implemented"
      }
  , deleteOps = DeleteOps
      { deleteFile = \_ -> error "Delete operation not implemented"
      }
  }

