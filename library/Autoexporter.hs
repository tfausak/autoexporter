module Autoexporter where

import qualified Data.List as List
import qualified System.Directory as Directory
import qualified System.Environment as Environment
import qualified System.FilePath as FilePath

main :: IO ()
main = do
    args <- Environment.getArgs
    case args of
        [name, inputFile, outputFile] -> do
            let moduleName = makeModuleName name

            let directory = FilePath.dropExtension inputFile
            files <- Directory.getDirectoryContents directory
            let haskellFiles = filter (\ f -> List.isSuffixOf ".hs" f || List.isSuffixOf ".lhs" f) files
            let paths = map (directory FilePath.</>) haskellFiles
            let modules = List.sort (map makeModuleName paths)
            let exports = init (unlines (map (\ x -> "    module " ++ x ++ ",") modules))
            let imports = init (unlines (map ("import " ++) modules))

            let output = unlines
                    [ unwords ["module", moduleName, "("]
                    , exports
                    , ") where"
                    , ""
                    , imports
                    ]
            writeFile outputFile output
        _ -> error ("unexpected arguments: " ++ show args)
  where
    makeModuleName :: FilePath -> String
    makeModuleName name =
        let path = FilePath.dropExtension name
            parts = FilePath.splitDirectories path
            -- TODO: Figure out a better way to get the actual module name.
            rest = tail (dropWhile (/= "library") parts)
        in  List.intercalate "." rest
