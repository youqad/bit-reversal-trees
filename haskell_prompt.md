```haskell
-- Type `Tree` of (perfect) binary trees
data Tree a = Leaf a | Node (Tree a) (Tree a) 
    deriving (Eq, Show)

{- Your goal is to implement an `invert` function that performs a bit-reversal
permutation on a `Tree`. Here's what we mean by that:

1. Each leaf in the binary tree has a path leading to it, which can be 
   represented as a string of bits: `False` (or `0`) for left, `True` (or `1`) for right.
2. The bit-reversal permutation swaps a leaf at path `p` with the leaf at path `reverse p`.
3. For example, a leaf at path `[False, False, True]` (left, left, right) would be swapped
   with the leaf at path `[True, False, False]` (right, left, left).

Implementation requirements:
1. The `invert :: Tree a -> Tree a` function that you will implement must be a standalone, pure, recursive function that does NOT rely on any helper function.
2. Only use the `Tree` type defined above.
3. Only use recursion (no loops).
4. Maintain purity (no side effects or mutability).

This is a hard problem, so think deeply, and make sure to carefully double-check your solution against the tests.
But I can guarantee you that it is solvable within the given constraints. Keep trying and don't give up!
-}

-- Implement the `invert` function:
invert :: Tree a -> Tree a
invert tree = undefined  -- Replace 'undefined' with your implementation

-- Tests:
testTree1 :: Tree Int
testTree1 = Node (Node (Node (Leaf 0) (Leaf 1)) (Node (Leaf 2) (Leaf 3))) 
                 (Node (Node (Leaf 4) (Leaf 5)) (Node (Leaf 6) (Leaf 7)))

expectedTree1 :: Tree Int
expectedTree1 = Node (Node (Node (Leaf 0) (Leaf 4)) (Node (Leaf 2) (Leaf 6))) 
                     (Node (Node (Leaf 1) (Leaf 5)) (Node (Leaf 3) (Leaf 7)))

testTree2 :: Tree Int
testTree2 = Node (Node (Node (Leaf 1) (Leaf 3)) (Node (Leaf 5) (Leaf 7)))
                 (Node (Node (Leaf 0) (Leaf 2)) (Node (Leaf 4) (Leaf 6)))

expectedTree2 :: Tree Int
expectedTree2 = Node (Node (Node (Leaf 1) (Leaf 0)) (Node (Leaf 5) (Leaf 4)))
                     (Node (Node (Leaf 3) (Leaf 2)) (Node (Leaf 7) (Leaf 6)))

testTree3 :: Tree Int
testTree3 = Node (Node (Node (Node (Leaf 10) (Leaf 14)) (Node (Leaf 4) (Leaf 6)))
                       (Node (Node (Leaf 9) (Leaf 1)) (Node (Leaf 0) (Leaf 7))))
                 (Node (Node (Node (Leaf 11) (Leaf 15)) (Node (Leaf 2) (Leaf 5)))
                       (Node (Node (Leaf 13) (Leaf 12)) (Node (Leaf 8) (Leaf 3))))

expectedTree3 :: Tree Int
expectedTree3 = Node (Node (Node (Node (Leaf 10) (Leaf 11)) (Node (Leaf 9) (Leaf 13)))
                           (Node (Node (Leaf 4) (Leaf 2)) (Node (Leaf 0) (Leaf 8))))
                     (Node (Node (Node (Leaf 14) (Leaf 15)) (Node (Leaf 1) (Leaf 12)))
                           (Node (Node (Leaf 6) (Leaf 5)) (Node (Leaf 7) (Leaf 3))))

main :: IO ()
main = do
    let tests = [
            ("Test 1 (Depth 3)", testTree1, expectedTree1),
            ("Test 2 (Depth 3)", testTree2, expectedTree2),
            ("Test 3 (Depth 4)", testTree3, expectedTree3)
        ]
    
    mapM_ (\(testName, input, expected) -> do
        putStrLn testName
        putStrLn "Input:"
        print input
        putStrLn "Expected output:"
        print expected
        putStrLn "Actual output:"
        print (invert input)) tests
```