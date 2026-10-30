module TestTree.DeepLeaves.DeepLeaf2 (includesDeepLeaf2) where

import Test.Tasty (TestTree)
import Test.Tasty.HUnit (testCase)

includesDeepLeaf2 :: TestTree
includesDeepLeaf2 = testCase "includes DeepLeaf2" (pure ())
