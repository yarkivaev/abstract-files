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
import Local ()
import qualified Data.ByteString.Char8 as BS

main :: IO ()
main = do
  let file = RelativeFile (Folder ["data"]) (FileName "example.txt")
  save ("Hello, World!" :: BS.ByteString) file
  content <- load file
  BS.putStrLn content
```

## Core Types

```haskell
data File = RelativeFile Folder FileName 
          | AbsoluteFile Folder FileName

class Monad m => SaveFile m content where
  save :: content -> File -> m ()

class Monad m => LoadFile m content where
  load :: File -> m content
```

## Building

```bash
stack build    # Build library
stack test     # Run tests
stack ghci     # Enter REPL
```

## License

MIT