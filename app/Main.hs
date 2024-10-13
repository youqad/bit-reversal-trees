{-# LANGUAGE FlexibleInstances #-}

module Main
  ( main,
  )
where

import Lib (Tree (Leaf, Node), invertHuman, prettyPrintTree)

-- Manual tests:
testTree1 :: Tree Int
testTree1 =
  Node
    (Node (Node (Leaf 0) (Leaf 1)) (Node (Leaf 2) (Leaf 3)))
    (Node (Node (Leaf 4) (Leaf 5)) (Node (Leaf 6) (Leaf 7)))

expectedTree1 :: Tree Int
expectedTree1 =
  Node
    (Node (Node (Leaf 0) (Leaf 4)) (Node (Leaf 2) (Leaf 6)))
    (Node (Node (Leaf 1) (Leaf 5)) (Node (Leaf 3) (Leaf 7)))

testTree2 :: Tree Int
testTree2 =
  Node
    (Node (Node (Leaf 1) (Leaf 3)) (Node (Leaf 5) (Leaf 7)))
    (Node (Node (Leaf 0) (Leaf 2)) (Node (Leaf 4) (Leaf 6)))

expectedTree2 :: Tree Int
expectedTree2 =
  Node
    (Node (Node (Leaf 1) (Leaf 0)) (Node (Leaf 5) (Leaf 4)))
    (Node (Node (Leaf 3) (Leaf 2)) (Node (Leaf 7) (Leaf 6)))

testTree3 :: Tree Int
testTree3 =
  Node
    ( Node
        (Node (Node (Leaf 10) (Leaf 14)) (Node (Leaf 4) (Leaf 6)))
        (Node (Node (Leaf 9) (Leaf 1)) (Node (Leaf 0) (Leaf 7)))
    )
    ( Node
        (Node (Node (Leaf 11) (Leaf 15)) (Node (Leaf 2) (Leaf 5)))
        (Node (Node (Leaf 13) (Leaf 12)) (Node (Leaf 8) (Leaf 3)))
    )

expectedTree3 :: Tree Int
expectedTree3 =
  Node
    ( Node
        (Node (Node (Leaf 10) (Leaf 11)) (Node (Leaf 9) (Leaf 13)))
        (Node (Node (Leaf 4) (Leaf 2)) (Node (Leaf 0) (Leaf 8)))
    )
    ( Node
        (Node (Node (Leaf 14) (Leaf 15)) (Node (Leaf 1) (Leaf 12)))
        (Node (Node (Leaf 6) (Leaf 5)) (Node (Leaf 7) (Leaf 3)))
    )

main :: IO ()
main = do
  let tests =
        [ ("Test 1 (Depth 3)", testTree1, expectedTree1),
          ("Test 2 (Depth 3)", testTree2, expectedTree2),
          ("Test 3 (Depth 4)", testTree3, expectedTree3)
        ]

  mapM_
    ( \(testName, input, expected) -> do
        putStrLn testName
        putStrLn "Input:"
        putStrLn $ prettyPrintTree input
        putStrLn "\nExpected output:"
        putStrLn $ prettyPrintTree expected
        putStrLn "\nActual output:"
        putStrLn $ prettyPrintTree (invertHuman input)
    )
    tests
