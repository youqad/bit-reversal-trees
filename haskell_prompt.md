```haskell
-- Type `Tree` of perfect binary trees
data Tree a = Leaf a | Node (Tree a) (Tree a) 

{- You are an expert Haskell competitive programmer. Your goal is to implement an `invert :: Tree a -> Tree a` function that performs a bit-reversal permutation on a `Tree`. Here is what we mean by that:

1. Each leaf in the binary tree has a path leading to it, which can be represented as a sequence of bits: `False` (or `0`) for left, `True` (or `1`) for right.
2. The bit-reversal permutation swaps a leaf at path `p` with the leaf at path `reverse p`. For example, a leaf at path `[False, False, True]` (left, left, right) would be swapped with the leaf at path `[True, False, False]` (right, left, left).

MANDATORY SYNTACTIC REQUIREMENTS:
1. The `invert` function must be a standalone and pure function ONLY relying on an inner function `invertHelper :: Bool -> Tree a -> Tree a` that is itself a self-contained single pure recursive function.
2. Only use recursion (no loops).
3. Maintain purity (no side effects or mutability).

The `Bool` parameter is an extra boolean that you can use however you want: the goal is that `invertHelper True tree` should return the bit-reversed tree.

This is a very difficult problem, so think step-by-step before implementing your solution and carefully review it to make sure it meets all the requirements. Test your implementation against the test cases to verify its correctness. I guarantee you that it is solvable within the constraints.
-}

-- Implement the `invert` function as follows:
invert :: Tree a -> Tree a
invert tree = invertHelper tree True
  where
    invertHelper :: Bool -> Tree a -> Tree a
    invertHelper flag tree = undefined  -- Replace 'undefined' with your implementation

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

main = do
    mapM_ (\(input, expected) -> do
        putStrLn "Expected output:"
        print expected
        putStrLn "Actual output:"
        print (invert input)
    ) [(testTree1, expectedTree1),
       (testTree2, expectedTree2),
       (testTree3, expectedTree3)]
```