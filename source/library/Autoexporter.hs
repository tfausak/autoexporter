module Autoexporter where

import qualified Data.List as List
import qualified Data.Maybe as Maybe
import qualified Data.Traversable as Traversable
import qualified Distribution.ModuleName as ModuleName
import qualified Distribution.Text as Text
import qualified System.Directory as Directory
import qualified System.Environment as Environment
import qualified System.FilePath as FilePath

data ExportScope = ExportScope'Shallow
                 | ExportScope'Deep

main :: IO ()
main = do
  args <- Environment.getArgs
  mainWithArgs args

mainWithArgs :: [String] -> IO ()
mainWithArgs args =
  case args of
    [name, inputFile, outputFile, "--deep"] ->
      autoexport ExportScope'Deep name inputFile outputFile
    [name, inputFile, outputFile] ->
      autoexport ExportScope'Shallow name inputFile outputFile
    _ -> fail ("unexpected arguments: " ++ show args)

autoexport :: ExportScope -> String -> FilePath -> FilePath -> IO ()
autoexport exportScope name inputFile outputFile = do
  let moduleName = makeModuleName name
  let directory = FilePath.dropExtension inputFile
  files <- findFiles exportScope directory
  let output = makeOutput moduleName directory files
  writeFile outputFile output

findFiles :: ExportScope -> FilePath -> IO [FilePath]
findFiles ExportScope'Shallow dir = filter isHaskellFile <$> findRootFiles dir
findFiles ExportScope'Deep dir = do
  rootItems <- findRootFiles dir
  childItems <- Traversable.for rootItems $ \item -> do
    let path = dir FilePath.</> item
    dirExists <- Directory.doesDirectoryExist path
    if dirExists
      then fmap (item FilePath.</>) <$> findFiles ExportScope'Deep path
      else pure []
  pure $ mconcat (filter isHaskellFile rootItems : childItems)

findRootFiles :: FilePath -> IO [FilePath]
findRootFiles dir = filter isRootItem <$> Directory.getDirectoryContents dir

makeModuleName :: FilePath -> String
makeModuleName name =
  let path = FilePath.dropExtension name
      parts = FilePath.splitDirectories path
      rest = takeWhileEnd isModuleName parts
  in List.intercalate "." rest

takeWhileEnd :: (a -> Bool) -> [a] -> [a]
takeWhileEnd p xs = reverse (takeWhile p (reverse xs))

isModuleName :: String -> Bool
isModuleName x = Maybe.isJust (parseModuleName x)

parseModuleName :: String -> Maybe ModuleName.ModuleName
parseModuleName = Text.simpleParse

makeOutput :: String -> FilePath -> [FilePath] -> String
makeOutput moduleName directory files =
  let haskellFiles = filter isHaskellFile files
      paths = map (directory FilePath.</>) haskellFiles
      modules = List.sort (map makeModuleName paths)
  in renderModule moduleName modules

isHaskellFile :: FilePath -> Bool
isHaskellFile x = List.isSuffixOf ".hs" x || List.isSuffixOf ".lhs" x

isRootItem :: FilePath -> Bool
isRootItem x = x `notElem` [".", ".."]

renderModule :: String -> [String] -> String
renderModule name modules =
  unlines'
    [ unwords ["module", name, "("]
    , unlines' (map renderExport modules)
    , ") where"
    , unlines' (map renderImport modules)
    ]

unlines' :: [String] -> String
unlines' = List.intercalate "\n"

renderExport :: String -> String
renderExport x = "  module " ++ x ++ ","

renderImport :: String -> String
renderImport x = "import " ++ x
