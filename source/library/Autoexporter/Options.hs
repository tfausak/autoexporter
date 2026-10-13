{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE TypeApplications #-}

-- | The option parser for the preprocessor.
module Autoexporter.Options
  ( -- * Parser
    parseOptions,

    -- * Types
    Options (..),
    Depth (..),
  )
where

import qualified Options.Applicative as Optparse

data Options = Options
  { _inputPath :: FilePath,
    _outputPath :: FilePath,
    _depth :: Depth,
    _includes :: [FilePath]
  }

-- | This type describes how to search for modules to export. A shallow search
-- only considers files in one directory. A deep search considers all files in
-- the directory tree.
data Depth
  = DepthShallow
  | DepthDeep
  deriving (Eq, Show)

parseOptions :: IO Options
parseOptions = Optparse.execParser optionsParserInfo

optionsParserInfo :: Optparse.ParserInfo Options
optionsParserInfo = Optparse.info optionsParser mempty

optionsParser :: Optparse.Parser Options
optionsParser = do
  -- Ideally we'd use pattern-matching here, but this is not available inside
  -- Applicative do-blocks.
  paths <- positionalArgs
  depth <- Optparse.flag DepthShallow DepthDeep (Optparse.long "deep")
  includes <- Optparse.many (Optparse.strOption (Optparse.long "include"))
  pure
    Options
      { _inputPath = fst paths,
        _outputPath = snd paths,
        _depth = depth,
        _includes = includes
      }

-- | Parse the input and output paths from the positional arguments.
--
-- We expect three positional arguments from GHC: the path to the original
-- source file, the path to the actual input file, and the path to the output
-- file. The source and input files could be different if another preprocessor
-- is involved. Since we don't consider the file's contents, we can ignore the
-- input file.
positionalArgs :: Optparse.Parser (FilePath, FilePath)
positionalArgs = do
  inputSourcePath <- Optparse.strArgument (Optparse.metavar "INPUT")
  _inputActualPath <- Optparse.strArgument @String (Optparse.metavar "(UNUSED)")
  outputPath <- Optparse.strArgument (Optparse.metavar "OUTPUT")
  pure (inputSourcePath, outputPath)
