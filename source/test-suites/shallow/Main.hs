module Main (main) where

import Test.Tasty (TestTree, defaultMain, testGroup)
import TestTree

tests :: TestTree
tests = testGroup "import TestTree" [includesShallowLeaf]

main :: IO ()
main = defaultMain tests
