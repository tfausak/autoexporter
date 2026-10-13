module NonRootTestTree.AutoexportedSubtree.DeepLeaves.DeepLeaf2
  ( includesNonRootDeepLeaf2,
  )
where

import Test.Tasty (TestTree)
import Test.Tasty.HUnit (testCase)

includesNonRootDeepLeaf2 :: TestTree
includesNonRootDeepLeaf2 = testCase "includes DeepLeaf2" (pure ())
