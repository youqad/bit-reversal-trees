import unittest
from typing import Union
from hypothesis import given, strategies as st
from hypothesis.strategies import composite
from hypothesis import settings, HealthCheck
from dataclasses import dataclass
import sys
import os

@dataclass(eq=True, repr=True)
class Tree:
    pass

@dataclass(eq=True, repr=True)
class Leaf(Tree):
    value: int

@dataclass(eq=True, repr=True)
class Node(Tree):
    left: Tree
    right: Tree

# placeholder: will be replaced dynamically in the test suite.
def invert(tree: Tree) -> Tree:
    pass

####### Helper functions for the test #######
def flatten_tree(tree: Tree) -> list:
    if isinstance(tree, Leaf):
        return [tree.value]
    elif isinstance(tree, Node):
        return flatten_tree(tree.left) + flatten_tree(tree.right)

def reverse_bits(n: int, num_bits: int) -> int:
    reversed_bits = 0
    for _ in range(num_bits):
        reversed_bits = (reversed_bits << 1) | (n & 1)
        n >>= 1
    return reversed_bits

def bit_reverse_permutation(values: list) -> list:
    n = len(values)
    num_bits = (n - 1).bit_length()
    indices = [reverse_bits(i, num_bits) for i in range(n)]
    return [values[i] for i in indices]

####### Generation of perfect binary trees #######
# @composite
# def perfect_binary_trees(draw, depth):
#     if depth == 0:
#         value = draw(st.integers())
#         return Leaf(value)
#     else:
#         left = draw(perfect_binary_trees(depth=depth - 1))
#         right = draw(perfect_binary_trees(depth=depth - 1))
#         return Node(left, right)

@composite
def perfect_binary_trees(draw, max_depth=4):
    depth = draw(st.integers(min_value=1, max_value=max_depth))
    num_leaves = 2 ** depth
    values = list(range(num_leaves))
    shuffled_values = draw(st.permutations(values))
    tree, remaining_values = build_tree(depth, shuffled_values)
    assert len(remaining_values) == 0  # make sure all values have been used
    return tree

def build_tree(depth, values):
    if depth == 0:
        value = values[0]
        return Leaf(value), values[1:]
    else:
        left_subtree, values = build_tree(depth - 1, values)
        right_subtree, values = build_tree(depth - 1, values)
        return Node(left_subtree, right_subtree), values

####### Test #######
# class TestInvertFunction(unittest.TestCase):
#     invert = staticmethod(invert)  # will be replaced by the generated invert function.

#     @given(perfect_binary_trees(depth=5))
#     def test_invert_function(self, tree):
#         PER_TEST_TIMEOUT = 10  # seconds
#         result_queue = Queue()

#         def run_test(result_queue):
#             try:
#                 invert_func = TestInvertFunction.invert
#                 original_flat = flatten_tree(tree)
#                 inverted_tree = invert_func(tree)
#                 inverted_flat = flatten_tree(inverted_tree)
#                 expected_flat = bit_reverse_permutation(original_flat)

#                 if inverted_flat == expected_flat:
#                     result_queue.put(None)  # Test passed
#                 else:
#                     msg = (
#                         f"Original flattened: {original_flat}\n"
#                         f"Expected flattened: {expected_flat}\n"
#                         f"Inverted flattened: {inverted_flat}"
#                     )
#                     result_queue.put(AssertionError(msg))
#             except Exception as e:
#                 result_queue.put(e)

#         p = Process(target=run_test, args=(result_queue,))
#         p.start()
#         p.join(PER_TEST_TIMEOUT)

#         if p.is_alive():
#             p.terminate()
#             p.join()
#             self.fail(f"Test timed out after {PER_TEST_TIMEOUT} seconds.")
#         else:
#             exception = result_queue.get()
#             if exception is not None:
#                 raise exception


class TestInvertFunction(unittest.TestCase):
    invert = None # will be set by the test suite.

    @classmethod
    def set_invert_function(cls, invert_function):
        cls.invert = staticmethod(invert_function)

    @settings(
        deadline=10000,  # 10 seconds per test case
        suppress_health_check=[HealthCheck.too_slow],
        database=None,
    )
    @given(perfect_binary_trees(max_depth=4))
    def test_invert_function(self, tree):
        invert_func = TestInvertFunction.invert
        original_flat = flatten_tree(tree)
        inverted_tree = invert_func(tree)
        inverted_flat = flatten_tree(inverted_tree)
        expected_flat = bit_reverse_permutation(original_flat)

        self.assertEqual(
            inverted_flat,
            expected_flat,
            msg=(
                "Failed:\n"
                f"Original tree, flattened: {original_flat}\n"
                f"Expected tree, flattened: {expected_flat}\n"
                f"Your inverted tree, flattened: {inverted_flat}"
            ),
        )

# test result class to capture exception messages
class CustomTestResult(unittest.TextTestResult):
    def __init__(self, *args, **kwargs):
        super().__init__(*args, **kwargs)
        self.failures_msgs = []
        self.errors_msgs = []

    def addFailure(self, test, err):
        super().addFailure(test, err)
        exc_type, exc_value, tb = err
        self.failures_msgs.append(str(exc_value))

    def addError(self, test, err):
        super().addError(test, err)
        exc_type, exc_value, tb = err
        self.errors_msgs.append(str(exc_value))


if __name__ == "__main__":
    unittest.main()