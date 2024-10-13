{-# LANGUAGE ScopedTypeVariables #-}

module Main (main) where

import Lib (Tree (Leaf, Node), bitReversePermutation, constructTree, depthOfTree, flattenTree, invert, invertHuman)
import Test.Hspec
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck

-- Generator for random trees of a given depth with random permutations of leaves
genTree :: forall a. (Num a, Enum a) => Int -> Gen (Tree a)
genTree depth = do
  let n = 2 ^ depth
  xs <- shuffle [0 .. n - 1]
  let (tree, _) = constructTree depth xs :: (Tree a, [a])
  return tree

instance (Arbitrary a, Num a, Enum a) => Arbitrary (Tree a) where
  arbitrary = do
    depth <- choose (1, 5)
    genTree depth

main :: IO ()
main = hspec $ do
  describe "Invert Function for Bit-Reversal Trees" $ do
    prop "correctly performs bit-reversal permutation on tree leaves" $
      \(tree :: Tree Int) ->
        let xs_flat = flattenTree tree
            invertedTree = invert tree
            ys_flat = flattenTree invertedTree
            ys_expected = bitReversePermutation xs_flat
            depth = depthOfTree tree
         in verbose
              $ counterexample
                ( "Depth: "
                    ++ show depth
                    ++ "\n"
                    -- ++ "Original tree: "
                    -- ++ prettyPrintTree tree
                    -- ++ "\n"
                    ++ "Original flattened: "
                    ++ show xs_flat
                    ++ "\n"
                    ++ "Expected flattened: "
                    ++ show ys_expected
                    ++ "\n"
                    ++ "Inverted flattened: "
                    ++ show ys_flat
                )
              $ ys_flat `shouldBe` ys_expected