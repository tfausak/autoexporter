module NonRootTestTree.AutoexportedSubtree.ShallowLeaf
  ( includesNonRootShallowLeaf,
  )
where

import Test.Tasty (TestTree)
import Test.Tasty.HUnit (testCase)

includesNonRootShallowLeaf :: TestTree
includesNonRootShallowLeaf = testCase "includes ShallowLeaf" (pure ())
