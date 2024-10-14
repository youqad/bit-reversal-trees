{-# LANGUAGE ScopedTypeVariables, RankNTypes #-}

module Lib
    ( Tree(..)
    , constructTree
    , prettyPrintTree
    , depthOfTree
    , flattenTree
    , reverseBits
    , bitReversePermutation
    , invertHuman
    , invert
    ) where

import qualified Data.Tree as DT
import qualified Data.Bits as B
import Text.ParserCombinators.ReadP
import Control.Applicative ((<|>))
import Test.QuickCheck

data Tree a = Leaf a | Node (Tree a) (Tree a) deriving (Eq)

instance (Show a) => Show (Tree a) where
  show (Leaf x) = "Leaf " ++ show x
  show (Node left right) = "Node (" ++ show left ++ ") (" ++ show right ++ ")"

instance (Read a) => Read (Tree a) where
    readsPrec _ = readP_to_S parseTree
      where
        parseTree = parseLeaf <|> parseNode
        parseLeaf = do
          string "Leaf "
          x <- readS_to_P reads
          return (Leaf x)
        parseNode = do
          string "Node"
          skipSpaces
          char '('
          left <- parseTree
          char ')'
          skipSpaces
          char '('
          right <- parseTree
          char ')'
          return (Node left right)

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

--------------------------------------------------------------------------------
-- Tree utilities
--------------------------------------------------------------------------------

toDataTree :: (Show a) => Tree a -> DT.Tree String
toDataTree (Leaf n) = DT.Node (show n) []
toDataTree (Node left right) = DT.Node "Node" [toDataTree left, toDataTree right]

prettyPrintTree :: (Show a) => Tree a -> String
prettyPrintTree = DT.drawTree . toDataTree

depthOfTree :: Tree a -> Int
depthOfTree (Leaf _) = 0
depthOfTree (Node left right) = 1 + max (depthOfTree left) (depthOfTree right)

constructTree :: Int -> [a] -> (Tree a, [a])
constructTree 0 (x : xs) = (Leaf x, xs)
constructTree n xs =
  let (leftTree, xs') = constructTree (n - 1) xs
      (rightTree, xs'') = constructTree (n - 1) xs'
   in (Node leftTree rightTree, xs'')

flattenTree :: Tree a -> [a]
flattenTree (Leaf x) = [x]
flattenTree (Node left right) = flattenTree left ++ flattenTree right



--------------------------------------------------------------------------------
-- Ground-truth bit-reversal permutation implementation
--------------------------------------------------------------------------------

-- Reverse the bits of an integer
reverseBits :: Int -> Int -> Int
reverseBits numBits n =
  foldl (\acc i -> ((n `B.shiftR` i) B..&. 1) B..|. (acc `B.shiftL` 1)) 0 [0..(numBits - 1)]

-- Perform a bit-reversal permutation on a list based on indices
bitReversePermutation :: [a] -> [a]
bitReversePermutation xs = map getElement reversedIndices
  where
    n = length xs
    numBits = ceiling (logBase 2 (fromIntegral n))
    indices = [0 .. n - 1]
    reversedIndices = map (reverseBits numBits) indices
    getElement idx = if idx < n then xs !! idx else error "Index out of bounds"

--------------------------------------------------------------------------------
-- Implementations of `invert`
--------------------------------------------------------------------------------

-- My own implementation, as a reference (passes all tests)
invertHuman :: Tree a -> Tree a
invertHuman (Node l@(Node _ _) r@(Node _ _)) = 
  let Node ll lr = invertHuman l
      Node rl rr = invertHuman r
  in Node (invertHuman (Node (invertHuman ll) (invertHuman rl))) 
          (invertHuman (Node (invertHuman lr) (invertHuman rr)))
invertHuman t = t


-- With one extra bit of state, we can adapt one of o1-preview's implementations (`invertO1`): https://chatgpt.com/share/670c8a2e-38b8-800a-9d8b-9594b5cf0c76
-- to create a working solution (`invertHumanBasedOnO1`) (that passes all the tests)
invertO1 :: Tree a -> Tree a
invertO1 (Leaf x) = Leaf x
invertO1 (Node l r) = merge (invertO1 l) (invertO1 r)
  where
    merge (Leaf x) (Leaf y) = Node (Leaf x) (Leaf y)
    merge (Node a b) (Node c d) = Node (merge a c) (merge b d)
  
invertHumanBasedOnO1 :: Tree a -> Tree a
invertHumanBasedOnO1 = invert' False
  where
    invert' :: Bool -> Tree a -> Tree a
    invert' _ (Leaf x) = Leaf x
    invert' False (Node l r) = invert' True $ Node (invert' False l) (invert' False r)
    invert' True (Node (Leaf x) (Leaf y)) = Node (Leaf x) (Leaf y)
    invert' True (Node (Node a b) (Node c d)) = Node (invert' True $ Node a c) (invert' True $ Node b d)


-- o1-mini implementation, which passes all tests 
-- BUT does not satisfy the syntactic requirement "no helper functions"
invertO1MiniWithHelperFns :: Tree a -> Tree a
invertO1MiniWithHelperFns tree = rebuildTree d permutedLeaves
  where
    -- Calculate the depth of the tree
    depth :: Tree a -> Int
    depth (Leaf _) = 0
    depth (Node l _) = 1 + depth l

    -- Collect all leaves in left-to-right order
    collectLeaves :: Tree a -> [a]
    collectLeaves (Leaf x) = [x]
    collectLeaves (Node l r) = collectLeaves l ++ collectLeaves r

    -- Compute bit-reversed index
    bitReversedIndex :: Int -> Int -> Int
    bitReversedIndex bits x = foldl (\acc i -> acc * 2 + ((x `div` (2 ^ i)) `mod` 2)) 0 [0..bits -1]

    -- Permute leaves based on bit-reversed indices
    permuteLeaves :: Int -> [a] -> [a]
    permuteLeaves bits leaves = map (\i -> leaves !! bitReversedIndex bits i) [0..length leaves - 1]

    -- Rebuild the tree from a list of leaves
    rebuildTree :: Int -> [a] -> Tree a
    rebuildTree 0 (x:_) = Leaf x
    rebuildTree d xs = Node (rebuildTree (d -1) left) (rebuildTree (d -1) right)
      where
        half = length xs `div` 2
        left = take half xs
        right = drop half xs

    -- Execute steps within invert
    d = depth tree
    leaves = collectLeaves tree
    permutedLeaves = permuteLeaves d leaves

-- Placeholder for the invert function
invert :: Tree a -> Tree a
invert = invertO1MiniWithHelperFns