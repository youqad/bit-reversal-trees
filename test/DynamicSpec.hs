{-# LANGUAGE ScopedTypeVariables #-}

module DynamicSpec (main, Tree (Leaf, Node), testInvert) where

import Lib (Tree (Leaf, Node), bitReversePermutation, depthOfTree, flattenTree, invertHuman)
import Test.Hspec
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck

testInvert :: (Tree Int -> Tree Int) -> IO ()
testInvert invert = hspec $ do
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
                    ++ "Original tree, flattened: "
                    ++ show xs_flat
                    ++ "\n"
                    ++ "Expected tree, flattened: "
                    ++ show ys_expected
                    ++ "\n"
                    ++ "Your inverted tree, flattened: "
                    ++ show ys_flat
                )
              $ ys_flat `shouldBe` ys_expected

main :: IO ()
main = do
  putStrLn "Run `testInvert invert` with an `invert :: Tree Int -> Tree Int` function to test your implementation."
