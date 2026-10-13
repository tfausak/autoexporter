-- | This package isn't really meant to be used as a library. It's typically
-- used as a GHC preprocessor, like so:
--
-- > {-# OPTIONS_GHC -F -pgmF autoexporter #-}
--
-- For more information, please see the README on GitHub:
-- <https://github.com/tfausak/autoexporter#readme>.
module Autoexporter
  ( autoexporter,
  )
where

import qualified Autoexporter.Options as Options
import qualified Control.Exception as Exception
import qualified Data.List as List
import qualified Data.Maybe as Maybe
import qualified Distribution.ModuleName as Cabal
import qualified Distribution.Text as Cabal
import qualified System.Directory as Directory
import qualified System.FilePath as FilePath

autoexporter :: IO ()
autoexporter = do
  -- Start by getting the command line arguments. Refer to the
  -- Autoexporter.Options module for details.
  Options.Options
    { Options._inputPath = input,
      Options._outputPath = output,
      Options._depth = depth,
      Options._includes = includes
    } <-
    Options.parseOptions

  -- Next we convert the original source file path into a module name. If we
  -- aren't able to do this then something weird is going on and we should
  -- crash.
  moduleName <- case toModuleName input of
    Just moduleName -> pure moduleName
    Nothing -> Exception.throwIO (InvalidModuleName input)

  -- Then we want to find all of the relevant modules to re-export. Note that
  -- we simply ignore non-Haskell files and files that don't form valid module
  -- names. Also we sort the module names so that the output is deterministic.
  entries <- listDirectory depth (FilePath.dropExtension input)
  let moduleNames = getModuleNames (includes ++ entries)

  -- Finally we render the module and write it to the output file.
  let content = renderModule moduleName moduleNames
  writeFile output content

-- | This function attempts to convert an arbitrary file path into a valid
-- Haskell module name. Any extensions are ignored.
--
-- >>> toModuleName "invalid/module.name"
-- Nothing
-- >>> toModuleName "valid/Module.name"
-- Just (ModuleName ["Module"])
-- >>> toModuleName "Qualified/Module.name"
-- Just (ModuleName ["Qualified","Module"])
toModuleName :: FilePath -> Maybe Cabal.ModuleName
toModuleName =
  Maybe.listToMaybe
    . Maybe.mapMaybe (Cabal.simpleParse . List.intercalate ".")
    . List.tails
    . FilePath.splitDirectories
    . FilePath.dropExtensions

-- | This exception type is thrown when we can't create a valid module name
-- from the source file path.
newtype InvalidModuleName
  = InvalidModuleName FilePath
  deriving (Eq, Show)

instance Exception.Exception InvalidModuleName

-- | Lists all of the entries in the given directory. Note that unlike
-- 'Directory.listDirectory' the results of calling this function will include
-- the original directory name.
listDirectory :: Options.Depth -> FilePath -> IO [FilePath]
listDirectory depth = case depth of
  Options.DepthShallow -> listDirectoryShallow
  Options.DepthDeep -> listDirectoryDeep

listDirectoryShallow :: FilePath -> IO [FilePath]
listDirectoryShallow directory = do
  entries <- Directory.listDirectory directory
  pure (fmap (FilePath.combine directory) entries)

listDirectoryDeep :: FilePath -> IO [FilePath]
listDirectoryDeep directory = do
  entries <- listDirectoryShallow directory
  let listEntry entry = do
        isDirectory <- Directory.doesDirectoryExist entry
        if isDirectory then listDirectoryDeep entry else pure [entry]
  fmap concat (mapM listEntry entries)

-- | Given a list of file paths, returns a sorted list of module names from the
-- entries that were Haskell files.
getModuleNames :: [FilePath] -> [Cabal.ModuleName]
getModuleNames =
  List.sort . Maybe.mapMaybe toModuleName . filter isHaskellFile

-- | This predicate tells you if the given file path is a Haskell source file.
isHaskellFile :: FilePath -> Bool
isHaskellFile = flip elem haskellExtensions . FilePath.takeExtensions

-- | These are the extensions that we consider to be Haskell source files.
haskellExtensions :: [String]
haskellExtensions = [".hs", ".lhs"]

-- | Given a module name and a list of module names to re-export, renders a
-- module with all the appropriate imports and exports.
renderModule :: Cabal.ModuleName -> [Cabal.ModuleName] -> String
renderModule moduleName moduleNames =
  unlines
    [ "{-# OPTIONS_GHC -fno-warn-dodgy-exports -fno-warn-unused-imports #-}",
      "module " <> Cabal.display moduleName <> " (",
      List.intercalate "\n" (fmap renderExport moduleNames),
      ") where",
      List.intercalate "\n" (fmap renderImport moduleNames)
    ]

renderExport :: Cabal.ModuleName -> String
renderExport moduleName = "module " <> Cabal.display moduleName <> ","

renderImport :: Cabal.ModuleName -> String
renderImport moduleName = "import " <> Cabal.display moduleName
