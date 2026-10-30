module TestTree.DeepLeaves.DeepLeaf1 (includesDeepLeaf1) where

import Test.Tasty (TestTree)
import Test.Tasty.HUnit (testCase)

includesDeepLeaf1 :: TestTree
includesDeepLeaf1 = testCase "includes DeepLeaf1" (pure ())
