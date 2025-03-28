{-# LANGUAGE ScopedTypeVariables #-}

module Main (main) where

import Lib (Tree (Leaf, Node), bitReversePermutation, depthOfTree, flattenTree, invert)
import Test.Hspec
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck

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