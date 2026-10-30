module TestTree.ShallowLeaf (includesShallowLeaf) where

import Test.Tasty (TestTree)
import Test.Tasty.HUnit (testCase)

includesShallowLeaf :: TestTree
includesShallowLeaf = testCase "includes ShallowLeaf" (pure ())
