module NonRootTestTree.AutoexportedSubtree.DeepLeaves.DeepLeaf1
  ( includesNonRootDeepLeaf1,
  )
where

import Test.Tasty (TestTree)
import Test.Tasty.HUnit (testCase)

includesNonRootDeepLeaf1 :: TestTree
includesNonRootDeepLeaf1 = testCase "includes DeepLeaf1" (pure ())
