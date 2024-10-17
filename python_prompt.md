```python
# Type `Tree`: perfect binary trees
@dataclass
class Tree:
    pass

@dataclass
class Leaf(Tree):
    value: int

@dataclass
class Node(Tree):
    left: Tree
    right: Tree

"""
You are an expert Python competitive programmer. Your goal is to implement an `invert(tree: Tree) -> Tree` function that performs a bit-reversal permutation on a `Tree`. Here is what we mean by that:

1. Each leaf in the binary tree has a path leading to it, which can be represented as a sequence of bits: `False` (or `0`) for left, `True` (or `1`) for right.
2. The bit-reversal permutation swaps a leaf at path `p` with the leaf at path `reverse(p)`. For example, a leaf at path `[False, False, True]` (left, left, right) would be swapped with the leaf at path `[True, False, False]` (right, left, left).

**MANDATORY SYNTACTIC REQUIREMENTS:**
1. The `invert` function must be a standalone, pure, and recursive function.
2. It must NOT rely on any helper function, with one possible exception: you can use a single helper inner function of type `def invert_helper(flag: bool, tree: Tree) -> Tree` if needed, but it is not necessary; there exist solutions that do not rely on any helper function.
3. Only use recursion (no loops).
4. Maintain purity (no side effects or mutability).

Remember, this is a challenging problem, so think step-by-step before implementing your solution and carefully review it to make sure it meets all the requirements. Test your implementation against the test cases to verify its correctness. I guarantee you that it is solvable within the constraints, so take your time to think it through carefully.
"""

# Implement the `invert` function:
def invert(tree: Tree) -> Tree:
    # Replace 'pass' with your implementation
    pass

# Tests:
test_tree1 = Node(
    Node(
        Node(Leaf(0), Leaf(1)),
        Node(Leaf(2), Leaf(3))
    ),
    Node(
        Node(Leaf(4), Leaf(5)),
        Node(Leaf(6), Leaf(7))
    )
)

expected_tree1 = Node(
    Node(
        Node(Leaf(0), Leaf(4)),
        Node(Leaf(2), Leaf(6))
    ),
    Node(
        Node(Leaf(1), Leaf(5)),
        Node(Leaf(3), Leaf(7))
    )
)

test_tree2 = Node(
    Node(
        Node(Leaf(1), Leaf(3)),
        Node(Leaf(5), Leaf(7))
    ),
    Node(
        Node(Leaf(0), Leaf(2)),
        Node(Leaf(4), Leaf(6))
    )
)

expected_tree2 = Node(
    Node(
        Node(Leaf(1), Leaf(0)),
        Node(Leaf(5), Leaf(4))
    ),
    Node(
        Node(Leaf(3), Leaf(2)),
        Node(Leaf(7), Leaf(6))
    )
)

test_tree3 = Node(
    Node(
        Node(
            Node(Leaf(10), Leaf(14)),
            Node(Leaf(4), Leaf(6))
        ),
        Node(
            Node(Leaf(9), Leaf(1)),
            Node(Leaf(0), Leaf(7))
        )
    ),
    Node(
        Node(
            Node(Leaf(11), Leaf(15)),
            Node(Leaf(2), Leaf(5))
        ),
        Node(
            Node(Leaf(13), Leaf(12)),
            Node(Leaf(8), Leaf(3))
        )
    )
)

expected_tree3 = Node(
    Node(
        Node(
            Node(Leaf(10), Leaf(11)),
            Node(Leaf(9), Leaf(13))
        ),
        Node(
            Node(Leaf(4), Leaf(2)),
            Node(Leaf(0), Leaf(8))
        )
    ),
    Node(
        Node(
            Node(Leaf(14), Leaf(15)),
            Node(Leaf(1), Leaf(12))
        ),
        Node(
            Node(Leaf(6), Leaf(5)),
            Node(Leaf(7), Leaf(3))
        )
    )
)

for input_tree, expected_tree in [
   (test_tree1, expected_tree1),
   (test_tree2, expected_tree2),
   (test_tree3, expected_tree3),
]:
   print("Expected output:")
   print(expected_tree)
   print("Actual output:")
   print(invert(input_tree))
```