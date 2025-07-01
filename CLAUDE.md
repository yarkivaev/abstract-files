# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

This is a Haskell library project that provides an abstract file system interface. The library defines type classes for saving and loading files with different implementations (currently working on a local file system implementation).

## Build and Development Commands

### Essential Commands
- **Build**: `stack build`
- **Run tests**: `stack test`
- **Run executable**: `stack run`
- **Start REPL**: `stack ghci`
- **Clean build**: `stack clean`
- **Build documentation**: `stack haddock`

### Development Workflow
- **Build and run tests**: `stack build --test`
- **Build with file watching**: `stack build --file-watch`
- **Run a specific test**: `stack test --test-arguments="-m <test-pattern>"`

## Architecture and Code Structure

### Core Abstractions
The library centers around type classes in `src/File.hs`:
- **SaveFile**: Type class for saving content to files
- **LoadFile**: Type class for loading content from files
- **File**: ADT representing either relative or absolute file paths
- **Folder**: List of directory segments
- **FileName**: String representing a file name

### Implementation Pattern
Each file system implementation should:
1. Create instances of `SaveFile` and `LoadFile` type classes
2. Handle both `RelativeFile` and `AbsoluteFile` cases
3. Provide appropriate error handling in the monad context

### Current State
- Core abstractions are defined but have syntax errors (extra comma in exports)
- Local file system implementation started in `src/local/Local.hs` but incomplete
- No tests implemented yet
- Strict compiler warnings enabled for code quality

## Important Notes
- The project uses Stack with LTS 23.26 (recent stable Haskell)
- All compiler warnings are treated seriously (see extensive `-W` flags in package.yaml)
- The library is designed to be extensible with different file system backends