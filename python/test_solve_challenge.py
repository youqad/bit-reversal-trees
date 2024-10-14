import pytest
import pexpect
from solve_challenge import run_tests, create_ghci_process

@pytest.fixture(scope="module")
def ghci_process():
    ghci = create_ghci_process()
    yield ghci
    print("Closing GHCi process...")
    ghci.sendline(":q")
    ghci.close()

def test_run_tests_incorrect_implementation(ghci_process):
    incorrect_invert_code = '''
invert :: Tree a -> Tree a
invert (Leaf a) = Leaf a
invert (Node l r) = Node (invert r) (invert l)
'''
    result, output = run_tests(incorrect_invert_code, ghci_process)
    print(f"Result: {result}")
    print(f"Output: {output}")
    assert result == False
    assert "Failed:" in output or "error:" in output

def test_run_tests_correct_implementation(ghci_process):
    correct_invert_code = """
invert :: Tree a -> Tree a
invert (Node l@(Node _ _) r@(Node _ _)) = 
  let Node ll lr = invert l
      Node rl rr = invert r
  in Node (invert (Node (invert ll) (invert rl))) 
          (invert (Node (invert lr) (invert rr)))
invert t = t
"""
    result, output = run_tests(correct_invert_code, ghci_process)
    print(f"Result: {result}")
    print(f"Output: {output}")
    assert result == True, f"Expected True, got {result}. Output: {output}"
    assert "+++ OK, passed" in output, f"Expected '+++ OK, passed' in output, got: {output}"


# def test_run_tests_syntax_error(ghci_process):
#     # Missing parentheses
#     syntax_error_code = """
# invert :: Tree a -> Tree a
# invert (Leaf a) = Leaf a
# invert (Node l r) = Node (invert r (invert l)
# """
#     result, output = run_tests(syntax_error_code, ghci_process)

#     assert result == False
#     assert "parse error" in output.lower() or "syntax error" in output.lower()


# def test_run_tests_timeout(ghci_process):
#     infinite_loop_code = """
# invert :: Tree a -> Tree a
# invert t = invert t
# """
#     result, output = run_tests(infinite_loop_code, ghci_process)

#     assert result == False
#     assert "timeout" in output.lower() or "timed out" in output.lower()
