from solve_challenge import run_tests

def test_run_tests_correct_implementation():
    correct_invert_code = '''
def invert(tree: Tree) -> Tree:
    if isinstance(tree, Node) and isinstance(tree.left, Node) and isinstance(tree.right, Node):
        left_inverted = invert(tree.left)
        right_inverted = invert(tree.right)
        left_left_inverted = invert(left_inverted.left)
        left_right_inverted = invert(left_inverted.right)
        right_left_inverted = invert(right_inverted.left)
        right_right_inverted = invert(right_inverted.right)
        return Node(invert(Node(left_left_inverted, right_left_inverted)), invert(Node(left_right_inverted, right_right_inverted)))
    return tree
'''

    result, output = run_tests(correct_invert_code, program_synthesis_language="python")
    print(f"Result: {result}")
    print(f"Output:\n{output}")
    assert result == True, f"Expected True, got {result}. Output: {output}"
    assert "All tests passed!" in output, f"Expected 'All tests passed!' in output, got: {output}"

def test_run_tests_incorrect_implementation():
    incorrect_invert_code = '''
def invert(tree: Tree) -> Tree:
    if isinstance(tree, Leaf):
        return tree
    else:
        return Node(invert(tree.right), invert(tree.left))
'''
    result, output = run_tests(incorrect_invert_code, program_synthesis_language="python")
    print(f"Result: {result}")
    print(f"Output:\n{output}")
    assert result == False, f"Expected False, got {result}. Output: {output}"
    assert "Failed:" in output, f"Expected 'Failed:' in output, got: {output}"

def test_run_tests_syntax_error():
    syntax_error_code = '''
def invert(tree: Tree) -> Tree
    if isinstance(tree, Leaf):
        return tree
    # Missing colon after function definition
'''
    result, output = run_tests(syntax_error_code, program_synthesis_language="python")
    print(f"Result: {result}")
    print(f"Output:\n{output}")
    assert result == False, f"Expected False, got {result}. Output: {output}"
    assert "Error during testing" in output, f"Expected 'Error during testing' in output, got: {output}"

def test_run_tests_timeout():
    infinite_recursion_code = '''
def invert(tree: Tree) -> Tree:
    # Infinite recursion without base case
    return invert(tree)
'''
    result, output = run_tests(infinite_recursion_code, program_synthesis_language="python")
    print(f"Result: {result}")
    print(f"Output:\n{output}")
    assert result == False, f"Expected False, got {result}. Output: {output}"
    assert ("Test timed out after" in output or "maximum recursion depth exceeded" in output), f"Expected timeout or recursion depth exceeded message in output, got: {output}"
