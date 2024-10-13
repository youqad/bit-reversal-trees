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
import Data.Bits
import Text.ParserCombinators.ReadP
import Control.Applicative ((<|>))

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
  foldl (\acc i -> ((n `shiftR` i) .&. 1) .|. (acc `shiftL` 1)) 0 [0..(numBits - 1)]

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


-- With one extra bit of state, we can adapt one of o1's implementations (`invertO1`)
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


-- -- o1's implementation
-- invert :: Tree a -> Tree a
-- invert = invert False
--   where
--     -- Helper function using one bit of state to track inversion
--     invert :: Bool -> Tree a -> Tree a
--     invert b (Leaf x) = Leaf x
--     invert b (Node l r) =
--         case (invert b l, invert b r) of
--             (Leaf x', Leaf y') -> Node (Leaf x') (Leaf y')
--             (Node a b', Node c d) -> Node (Node a c) (Node b' d)
--             _ -> Node l r










