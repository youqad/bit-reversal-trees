#!/usr/bin/env python3

import sys
import unittest
from hypothesis import given, settings, Verbosity, HealthCheck
from hypothesis_tests_python import (
    Tree, Leaf, Node, perfect_binary_trees,
    flatten_tree, bit_reverse_permutation
)
from termcolor import colored

class TestCustomInvert(unittest.TestCase):
    """Test class to test custom invert functions."""
    invert = None

    @classmethod
    def set_invert_function(cls, invert_function):
        cls.invert = staticmethod(invert_function)

    @settings(
        deadline=10000,
        suppress_health_check=[HealthCheck.too_slow],
        database=None,
        verbosity=Verbosity.verbose,
        max_examples=100
    )
    @given(perfect_binary_trees(max_depth=4))
    def test_invert_function(self, tree):
        """Test the invert function with randomly generated perfect binary trees."""
        invert_func = self.invert
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

def test_implementation(invert_code: str) -> tuple[bool, str]:
    """Test a Python implementation using Hypothesis."""
    try:
        namespace = {
            "__builtins__": __builtins__,
            "Tree": Tree,
            "Leaf": Leaf,
            "Node": Node,
        }
        exec(invert_code, namespace, namespace)
        
        invert_function = namespace.get("invert")
        if not invert_function:
            return False, "No 'invert' function found in the code"
        
        TestCustomInvert.set_invert_function(invert_function)
        suite = unittest.TestLoader().loadTestsFromTestCase(TestCustomInvert)
        runner = unittest.TextTestRunner(verbosity=2)
        result = runner.run(suite)
        
        return result.wasSuccessful(), "All tests passed! 🎉" if result.wasSuccessful() else "\n".join(str(err[1]) for err in result.failures + result.errors)
        
    except Exception as e:
        return False, f"Error during testing: {str(e)}"

def main():
    if len(sys.argv) > 1:
        with open(sys.argv[1]) as f:
            implementation = f.read()
    else:
        print(colored("Paste your Python implementation of invert (Ctrl+D when done):", "cyan"))
        implementation = sys.stdin.read()
    
    print(colored("\nRunning Hypothesis property-based tests...", "cyan"))
    print(colored("Note: While you'll see '1 test', Hypothesis will generate and run", "cyan"))
    print(colored("100 different test cases with randomly generated trees.", "cyan"))
    print(colored("The statistics at the end will show details about the test run.", "cyan"))
    
    success, output = test_implementation(implementation)
    if success:
        print(colored("\nOutput:", "green"))
        print(colored(output, "green"))
    else:
        print(colored("\nOutput:", "red"))
        print(colored(output, "red"))
    sys.exit(0 if success else 1)

if __name__ == "__main__":
    main()