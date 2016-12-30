module Autoexporter where

import qualified Data.List as List
import qualified Data.Maybe as Maybe
import qualified Distribution.ModuleName as ModuleName
import qualified Distribution.Text as Text
import qualified System.Directory as Directory
import qualified System.Environment as Environment
import qualified System.FilePath as FilePath

main :: IO ()
main = do
  args <- Environment.getArgs
  mainWithArgs args

mainWithArgs :: [String] -> IO ()
mainWithArgs args =
  case args of
    [name, inputFile, outputFile] -> autoexport name inputFile outputFile
    _ -> fail ("unexpected arguments: " ++ show args)

autoexport :: String -> FilePath -> FilePath -> IO ()
autoexport name inputFile outputFile = do
  let moduleName = makeModuleName name
  let directory = FilePath.dropExtension inputFile
  files <- Directory.getDirectoryContents directory
  let output = makeOutput moduleName directory files
  writeFile outputFile output

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
