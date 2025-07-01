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
import Local (localFileOps)
import qualified Data.ByteString.Char8 as BS

main :: IO ()
main = do
  let file = File (Folder ["data"]) (FileName "example.txt")
  let ops = localFileOps
  saveFile (saveOps ops) ("Hello, World!" :: BS.ByteString) file
  content <- loadFile (loadOps ops) file
  BS.putStrLn content
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