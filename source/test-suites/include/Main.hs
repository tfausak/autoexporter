module Main (main) where

import NonRootTestTree.AutoexportedSubtree
import Test.Tasty (TestTree, defaultMain, testGroup)
import TestTree

defaultTestTreeTests :: TestTree
defaultTestTreeTests =
  testGroup "import TestTree" [includesShallowLeaf, includesDeepLeaf1]

nonRootTestTreeTests :: TestTree
nonRootTestTreeTests =
  testGroup
    "import NonRootTestTree.AutoexportedSubtree"
    [includesNonRootShallowLeaf, includesNonRootDeepLeaf1]

tests :: TestTree
tests = testGroup "using --include" [defaultTestTreeTests, nonRootTestTreeTests]

main :: IO ()
main = defaultMain tests
