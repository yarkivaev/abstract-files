# abstract-files

A Haskell library providing an abstract file system interface with pluggable backends.

## Installation

```yaml
# stack.yaml
extra-deps:
- git: https://github.com/yarkivaev/abstract-files.git
  commit: main

# package.yaml
dependencies:
- abstract-files
```

## Usage

```haskell
{-# LANGUAGE OverloadedStrings #-}

import File
import RelativeLocal (relativeFileOps)
import AbsoluteLocal (absoluteFileOps)
import qualified Data.ByteString.Char8 as BS

main :: IO ()
main = do
  -- Relative path operations (relative to current directory)
  let relFile = File (Folder ["data"]) (FileName "relative.txt")
  let relOps = relativeFileOps
  saveFile (saveOps relOps) ("Relative path!" :: BS.ByteString) relFile
  
  -- Absolute path operations
  let absFile = File (Folder ["/tmp", "myapp"]) (FileName "absolute.txt")
  let absOps = absoluteFileOps
  saveFile (saveOps absOps) ("Absolute path!" :: BS.ByteString) absFile
  
  -- Load and display
  content1 <- loadFile (loadOps relOps) relFile
  content2 <- loadFile (loadOps absOps) absFile
  BS.putStr content1
  BS.putStr content2
```

## Core Types

```haskell
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

-- Combined operations
data FileOps m content = FileOps
  { saveOps   :: SaveOps m content
  , loadOps   :: LoadOps m content
  , deleteOps :: DeleteOps m
  }
```

## Building

```bash
stack build    # Build library
stack test     # Run tests
stack ghci     # Enter REPL
```

## License

MIT