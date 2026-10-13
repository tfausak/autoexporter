module Main (main) where

import Test.Tasty (TestTree, defaultMain, testGroup)
import TestTree

tests :: TestTree
tests = testGroup "import TestTree" [includesShallowLeaf, includesDeepLeaf1]

main :: IO ()
main = defaultMain tests
