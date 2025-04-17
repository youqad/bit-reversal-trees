# Victor Taelin's Bit-Reversal Trees Challenge: Haskell Solutions and Python LLM Search

<p align="center">
  <a href="https://www.haskell.org/ghcup/"><img src="https://img.shields.io/badge/Haskell-5e5086?style=flat-square&logo=haskell&logoColor=white" alt="Haskell"></a>&nbsp;
  <a href="https://www.python.org/downloads/"><img src="https://img.shields.io/badge/python-3670A0?style=flat-square&logo=python&logoColor=ffdd54" alt="Python"></a>&nbsp;
  <a href="https://platform.openai.com/docs/api-reference"><img src="https://img.shields.io/badge/OpenAI-74aa9c?style=flat-square&logo=openai&logoColor=white" alt="OpenAI"></a>&nbsp;
  <a href="https://x.ai/"><img src="https://img.shields.io/badge/Grok-000000?style=flat-square&logo=x&logoColor=white" alt="xAI"></a>&nbsp;
  <a href="https://docs.anthropic.com/en/api/getting-started"><img src="https://img.shields.io/badge/Anthropic-c99d78?style=flat-square&logo=Anthropic&logoColor=black&color=c99d78&" alt="Anthropic"></a>&nbsp;
  <a href="https://deepseek.com/"><img src="https://img.shields.io/badge/DeepSeek-4D6BFE?style=flat-square&logo=data:image/svg%2bxml;base64,PHN2ZyBoZWlnaHQ9IjFlbSIgc3R5bGU9ImZsZXg6bm9uZTtsaW5lLWhlaWdodDoxIiB2aWV3Qm94PSIwIDAgMjQgMjQiIHdpZHRoPSIxZW0iIHhtbG5zPSJodHRwOi8vd3d3LnczLm9yZy8yMDAwL3N2ZyI+PHRpdGxlPkRlZXBTZWVrPC90aXRsZT48cGF0aCBkPSJNMjMuNzQ4IDQuNDgyYy0uMjU0LS4xMjQtLjM2NC4xMTMtLjUxMi4yMzQtLjA1MS4wMzktLjA5NC4wOS0uMTM3LjEzNi0uMzcyLjM5Ny0uODA2LjY1Ny0xLjM3My42MjYtLjgyOS0uMDQ2LTEuNTM3LjIxNC0yLjE2My44NDgtLjEzMy0uNzgyLS41NzUtMS4yNDgtMS4yNDctMS41NDgtLjM1Mi0uMTU2LS43MDgtLjMxMS0uOTU1LS42NS0uMTcyLS4yNDEtLjIxOS0uNTEtLjMwNS0uNzc0LS4wNTUtLjE2LS4xMS0uMzIzLS4yOTMtLjM1LS4yLS4wMzEtLjI3OC4xMzYtLjM1Ni4yNzYtLjMxMy41NzItLjQzNCAxLjIwMi0uNDIyIDEuODQuMDI3IDEuNDM2LjYzMyAyLjU4IDEuODM4IDMuMzkzLjEzNy4wOTMuMTcyLjE4Ny4xMjkuMzIzLS4wODIuMjgtLjE4LjU1Mi0uMjY2LjgzMy0uMDU1LjE3OS0uMTM3LjIxNy0uMzI5LjE0YTUuNTI2IDUuNTI2IDAgMDEtMS43MzYtMS4xOGMtLjg1Ny0uODI4LTEuNjMxLTEuNzQyLTIuNTk3LTIuNDU4YTExLjM2NSAxMS4zNjUgMCAwMC0uNjg5LS40NzFjLS45ODUtLjk1Ny4xMy0xLjc0My4zODgtMS44MzYuMjctLjA5OC4wOTMtLjQzMi0uNzc5LS40MjgtLjg3Mi4wMDQtMS42Ny4yOTUtMi42ODcuNjg0YTMuMDU1IDMuMDU1IDAgMDEtLjQ2NS4xMzcgOS41OTcgOS41OTcgMCAwMC0yLjg4My0uMTAyYy0xLjg4NS4yMS0zLjM5IDEuMTAyLTQuNDk3IDIuNjIzQy4wODIgOC42MDYtLjIzMSAxMC42ODQuMTUyIDEyLjg1Yy40MDMgMi4yODQgMS41NjkgNC4xNzUgMy4zNiA1LjY1MyAxLjg1OCAxLjUzMyAzLjk5NyAyLjI4NCA2LjQzOCAyLjE0IDEuNDgyLS4wODUgMy4xMzMtLjI4NCA0Ljk5NC0xLjg2LjQ3LjIzNC45NjIuMzI3IDEuNzguMzk3LjYzLjA1OSAxLjIzNi0uMDMgMS43MDUtLjEyOC43MzUtLjE1Ni42ODQtLjgzNy40MTktLjk2MS0yLjE1NS0xLjAwNC0xLjY4Mi0uNTk1LTIuMTEzLS45MjYgMS4wOTYtMS4yOTYgMi43NDYtMi42NDIgMy4zOTItNy4wMDMuMDUtLjM0Ny4wMDctLjU2NSAwLS44NDUtLjAwNC0uMTcuMDM1LS4yMzcuMjMtLjI1NmE0LjE3MyA0LjE3MyAwIDAwMS41NDUtLjQ3NWMxLjM5Ni0uNzYzIDEuOTYtMi4wMTUgMi4wOTMtMy41MTcuMDItLjIzLS4wMDQtLjQ2Ny0uMjQ3LS41ODh6TTExLjU4MSAxOGMtMi4wODktMS42NDItMy4xMDItMi4xODMtMy41Mi0yLjE2LS4zOTIuMDI0LS4zMjEuNDcxLS4yMzUuNzYzLjA5LjI4OC4yMDcuNDg2LjM3MS43MzkuMTE0LjE2Ny4xOTIuNDE2LS4xMTMuNjAzLS42NzMuNDE2LTEuODQyLS4xNC0xLjg5Ny0uMTY3LTEuMzYxLS44MDItMi41LTEuODYtMy4zMDEtMy4zMDctLjc3NC0xLjM5My0xLjIyNC0yLjg4Ny0xLjI5OC00LjQ4Mi0uMDItLjM4Ni4wOTMtLjUyMi40NzctLjU5MmE0LjY5NiA0LjY5NiAwIDAxMS41MjktLjAzOWMyLjEzMi4zMTIgMy45NDYgMS4yNjUgNS40NjggMi43NzQuODY4Ljg2IDEuNTI1IDEuODg3IDIuMjAyIDIuODkxLjcyIDEuMDY2IDEuNDk0IDIuMDgyIDIuNDggMi45MTQuMzQ4LjI5Mi42MjUuNTE0Ljg5MS42NzctLjgwMi4wOS0yLjE0LjExLTMuMDU0LS42MTR6bTEtNi40NGEuMzA2LjMwNiAwIDAxLjQxNS0uMjg3LjMwMi4zMDIgMCAwMS4yLjI4OC4zMDYuMzA2IDAgMDEtLjMxLjMwNy4zMDMuMzAzIDAgMDEtLjMwNC0uMzA4em0zLjExIDEuNTk2Yy0uMi4wODEtLjM5OS4xNTEtLjU5LjE2YTEuMjQ1IDEuMjQ1IDAgMDEtLjc5OC0uMjU0Yy0uMjc0LS4yMy0uNDctLjM1OC0uNTUyLS43NThhMS43MyAxLjczIDAgMDEuMDE2LS41ODhjLjA3LS4zMjctLjAwOC0uNTM3LS4yMzktLjcyNy0uMTg3LS4xNTYtLjQyNi0uMTk5LS42ODgtLjE5OWEuNTU5LjU1OSAwIDAxLS4yNTQtLjA3OGMtLjExLS4wNTQtLjItLjE5LS4xMTQtLjM1OC4wMjgtLjA1NC4xNi0uMTg2LjE5Mi0uMjEuMzU2LS4yMDIuNzY3LS4xMzYgMS4xNDYuMDE2LjM1Mi4xNDQuNjE4LjQwOCAxLjAwMS43ODIuMzkxLjQ1MS40NjIuNTc2LjY4NS45MTQuMTc2LjI2NS4zMzYuNTM3LjQ0NS44NDguMDY3LjE5NS0uMDE5LjM1NC0uMjUuNDUyeiIgZmlsbD0iI2ZmZiI+PC9wYXRoPjwvc3ZnPg==&logoColor=white" alt="DeepSeek"></a>&nbsp;
  <a href="https://ai.google.dev/"><img src="https://img.shields.io/badge/Gemini-4285F4?style=flat-square&logo=google&logoColor=white" alt="Gemini"></a>&nbsp;
  <a href="https://wandb.ai/site"><img src="https://img.shields.io/badge/W%26B-000000?style=flat-square&logo=weightsandbiases&logoColor=white" alt="W&B"></a>&nbsp;
  <a href="https://x.com/VictorTaelin/status/1844886809005687270"><img src="https://img.shields.io/badge/Post-%23000000.svg?style=flat-square&logo=X&logoColor=white" alt="X"></a>
</p>

This project implements and tests solutions to the bit-reversal permutation on binary trees, as proposed in [Victor Taelin's challenge](https://x.com/VictorTaelin/status/1844886809005687270). It includes Haskell implementations and a Python-based Large Language Model (LLM) search for valid Haskell solutions.

## The Challenge

Victor's challenge involves coming up with a prompt to show that a LLM is capable of implementing a solution to the problem of inverting a binary tree with specific requirements:
1. It must invert the keys using a "bit-reversal permutation"
2. It must be a dependency-free, pure recursive function
3. The function may use one extra bit of state if needed, i.e. it must have the type signature `Tree -> Tree` or `Bool -> Tree -> Tree` (direct recursion with maximum 1 bit of state)

<p align="center">
  <a href="https://x.com/VictorTaelin/status/1844886809005687270">
    <img src="victor_tweet.png" alt="Victor's tweet" width="500" height="auto">
  </a>
</p>

### Initial Prompt

My Haskell prompt, adapted from [Victor's prompt](https://gist.github.com/VictorTaelin/45440a737e47b872d7505c6cda27b6aa) and kept under 1k tokens, is in [`haskell_prompt.md`](haskell_prompt.md). The Python prompt is in [`python_prompt.md`](python_prompt.md).
Note that there are two versions of the prompt, for each language: one that allows the use of a helper function using one extra bit of state, and one that does not mention any such helper function ([`haskell_prompt_no_helper.md`](haskell_prompt_no_helper.md) and [`python_prompt_no_helper.md`](python_prompt_no_helper.md)).

> [!NOTE]
> For full details about the challenge, see: 
> - [Victor's tweet](https://x.com/VictorTaelin/status/1844886809005687270)
> - [Victor's gist](https://gist.github.com/VictorTaelin/45440a737e47b872d7505c6cda27b6aa)

> [!IMPORTANT]
> APRIL 2025 UPDATE: A valid solution has now automatically been generated by our LLM search in a way that satifies Victor's challenge, with OpenAI's o3. See below, in the "Solutions" section.

## Project Structure

```
.
├── .env.example
├── README.md
├── Victor_challenge.md
├── haskell_prompt.md
├── python
│   ├── requirements.txt
│   ├── solutions0.txt
│   ├── solve_challenge.py
│   ├── test_solve_challenge.py
│   └── utils.py
├── Setup.hs
├── package.yaml
├── stack.yaml
├── app
│   └── Main.hs
├── src
│   └── Lib.hs
└── test
    ├── DynamicSpec.hs
    └── Spec.hs
```

where:

- [`src/Lib.hs`](src/Lib.hs): Contains Haskell implementations of the bit-reversal tree inversion (see [`haskell_prompt.md`](haskell_prompt.md) for the prompt used to generate the AI ones)
- [`test/Spec.hs`](test/Spec.hs): QuickCheck tests for the Haskell implementations in `src/Lib.hs`
- [`test/DynamicSpec.hs`](test/DynamicSpec.hs): QuickCheck tests for the Haskell implementations generated during the LLM search
- [`python/solve_challenge.py`](python/solve_challenge.py): Python script to search for valid Haskell solutions using LLMs
- [`python/utils.py`](python/utils.py): Utility functions for the Python LLM search
- [`python/test_solve_challenge.py`](python/test_solve_challenge.py): Pytest file for testing the Python-GHCi integration for the LLM search
- [`python/test_custom_invert.py`](python/test_custom_invert.py): Pytest file for testing custom Python implementations of the `invert` function
- [`python/hypothesis_tests_python.py`](python/hypothesis_tests_python.py): Hypothesis tests for the Python implementations
- [`python/test_solve_challenge_hypothesis.py`](python/test_solve_challenge_hypothesis.py), [`python/test_solve_challenge_quickcheck.py`](python/test_solve_challenge_quickcheck.py): Hypothesis and QuickCheck tests (to be run with `python -m pytest`) to make sure the LLM search is working as expected in [solve_challenge.py](python/solve_challenge.py).

## Getting Started

### Prerequisites

1. Haskell with Stack (if you want to search over Haskell programs)
2. Python 3.7+
3. OpenAI API key (for GPT models)
4. Anthropic API key (for Claude models)
5. DeepSeek API key (for DeepSeek models)
6. Weights & Biases API key (optional, for LLM logging)

### Step-by-Step Guide

#### Haskell Setup

1. Clone the repository:
   ```sh
   git clone https://github.com/youqad/bit-reversal-trees.git
   cd bit-reversal-trees
   ```

2. Set up the Haskell environment:
   Make sure you have [Stack](https://docs.haskellstack.org/en/stable/install_and_upgrade/) installed on your system.
   ```sh
   stack setup
   stack build
   ```

3. To run the Haskell QuickCheck tests:
   ```sh
   stack test
   ```


#### Python Setup
4. Set up the Python environment:
   ```sh
   cd python
   python -m venv venv
   source venv/bin/activate
   pip install -r requirements.txt
   ```

5. Set up your environment variables:
   Copy the [`.env.example`](.env.example) file to `.env` and fill in the required values:
   ```sh
   OPENAI_API_KEY=your_api_key_here
   OPENAI_ORGANIZATION=your_organization_id
   OPENAI_PROJECT=your_project_name
   ANTHROPIC_API_KEY=your_anthropic_api_key_here
   DEEPSEEK_API_KEY=your_deepseek_api_key_here
   GEMINI_API_KEY=your_google_api_key_here
   WANDB_API_KEY=your_wandb_api_key
   GENERATOR_MODEL_NAME=anthropic/claude-3-5-sonnet-20241022 # or gpt-4.1, or o3, or o4-mini, or gpt-4o, or gpt-4.5-preview, or o1-mini, or o1-preview, or o1, or o1-pro, or o3-mini, or deepseek/deepseek-reasoner, or xai/grok-3-beta, or gemini/gemini-2.5-pro-exp-03-25
   VERIFIER_MODEL_NAME=gpt-4.1-mini # or o4-mini, or xai/grok-3-mini-beta, or deepseek/deepseek-chat, for example
   NUM_INITIAL_SOLUTIONS=10 # Number of suggested solutions to the initial prompt (each of them giving rise to a conversation)
   MAX_ROUNDS=8 # Number of rounds/turns per conversation, in the LLM search
   PROGRAM_SYNTHESIS_LANGUAGE=haskell # or python
   ```

## LLM-based Program Synthesis search

The [`python/solve_challenge.py`](python/solve_challenge.py) script searches over either Haskell or Python programs using a very simple/naive LLM-based search approach (because we're not supposed to give any hint to the LLM, other than the 1k tokens of the initial prompt and [access to a function interpreter](https://gist.github.com/VictorTaelin/45440a737e47b872d7505c6cda27b6aa?permalink_comment_id=5232410#gistcomment-5232410)). It follows these steps:

1. A generator LLM (specified by `GENERATOR_MODEL_NAME`) produces an initial implementation of the `invert` function (as a solution to [the initial prompt](https://github.com/youqad/bit-reversal-trees?tab=readme-ov-file#initial-prompt)) in the language specified by `PROGRAM_SYNTHESIS_LANGUAGE` (Haskell or Python), starting a trajectory/conversation (i.e. a sequence of messages). We can use the `NUM_INITIAL_SOLUTIONS` environment variable to specify the number of conversations to run (in parallel if possible, or sequentially otherwise).
2. The initial implementation goes through:
   - a syntactic verification by a verifier LLM (`VERIFIER_MODEL_NAME`), which checks if the implementation satisfies the syntactic constraints of the challenge
   - a property-based testing (QuickCheck in [`test/DynamicSpec.hs`](test/DynamicSpec.hs) for Haskell, Hypothesis in [`python/hypothesis_tests_python.py`](python/hypothesis_tests_python.py) for Python), checking if the implementation satisfies the desired functional requirement
3. Failed implementations enter a refinement loop where:
   - ❌ if the implementation is deemed syntactically incorrect by the verifier LLM (`VERIFIER_MODEL_NAME`): the message "The proposed invert function does not satisfy the syntactic requirements. Please revise your implementation." is sent back to the generator LLM (`GENERATOR_MODEL_NAME`) (no hints are given) and the conversation continues 
   - ❌ if the implementation is deemed syntactically correct but fails on one test case (i.e. it does not satisfy the functional desired requirements): the counter-example is sent back to the generator LLM (`GENERATOR_MODEL_NAME`) and the conversation continues
   - ❌ if the implementation times out: the message "Your code caused a timeout during testing. Please review your implementation." is sent back to the generator LLM (`GENERATOR_MODEL_NAME`) and the conversation continues
   - ✅ if the implementation is deemed syntactically correct and passes all the test cases: the conversation stops and the solution is logged.
4. Rinse and repeat: the process repeats until either:
     - a valid solution is found
     - or the maximum number of rounds (`MAX_ROUNDS`) is reached for the current conversation

Valid solutions are logged to `solutions.txt`, `solutions_calls.jsonl`, and Weights & Biases for later analysis.

> [!NOTE] 
> Configure the search by setting environment variables in `.env` (see [`.env.example`](.env.example) for an example).

### Haskell LLM-based Program Synthesis search

6. Run the LLM search:
   ```sh
   export PROGRAM_SYNTHESIS_LANGUAGE=haskell
   python solve_challenge.py
   ```

7. To test Haskell's QuickCheck tests:
   ```sh
   python -m pytest test_solve_challenge_quickcheck.py
   ```

### Python LLM-based Program Synthesis search

8. Run the Python LLM search:
   ```sh
   export PROGRAM_SYNTHESIS_LANGUAGE=python
   python solve_challenge.py
   ```

9. To test Python's Hypothesis tests:
   ```sh
   python -m pytest test_solve_challenge_hypothesis.py
   ```

## Solutions

### Python

#### Solution with our system

🎉 APRIL 2025: The [o3 model has now successfully generated](https://gist.github.com/youqad/02a36419cbc4725601bffc05f14b3947) a Python implementation that passes all tests and adheres to the constraints (using the function with one bit of state, and no other helper function)!

```python
def invert(tree: Tree) -> Tree:
    """
    Return a new perfect‑binary tree whose leaves are arranged according to
    the bit‑reversal permutation of their paths   (left = 0, right = 1).

    Mandatory constraints – all satisfied:
        1.  Only the inner   invert_helper(tree, flag)   is used.
        2.  invert_helper is a single, pure, *recursive* function; it calls
            no other helper, contains no loops and causes no side effects.
        3.  The public call is   invert_helper(tree, True) .
    """

    def invert_helper(t: Tree, flag: bool) -> Tree:
        # -------- leaf ---------------------------------------------------- #
        if isinstance(t, Leaf):
            return t

        # ------------------------------------------------------------------ #
        #  flag == True   : full bit‑reversal of  the  subtree  't'.
        #  flag == False  : *weave* two already bit‑reversed sub‑trees so
        #                   that their leaves are perfectly interleaved
        #                   (LSB that has just become the MSB).
        # ------------------------------------------------------------------ #
        if flag:                                            # bit‑reversal
            left_rev  = invert_helper(t.left,  True)        # reverse both halves
            right_rev = invert_helper(t.right, True)
            # After each half has been bit‑reversed we only have to
            # weave them together; the weaving is performed by a single
            # recursive call in the   flag == False   mode.
            return invert_helper(Node(left_rev, right_rev), False)

        # --------------------- weaving mode ------------------------------- #
        #  Children are leaves → nothing more to weave; just join them.
        if isinstance(t.left, Leaf):                        # depth == 1
            return Node(t.left, t.right)

        #  Non‑trivial case – recurse pairwise on both sides.
        return Node(
            invert_helper(Node(t.left.left,  t.right.left),  False),
            invert_helper(Node(t.left.right, t.right.right), False)
        )

    # single call required by the statement
    return invert_helper(tree, True)
```

📦 The solution has been saved [on WandB weave with Call ID: 0196425c-0650-7df2-8d57-05d272f6d111](https://wandb.ai/ox/bit-reversal-trees/weave/traces?filter=%7B%22opVersionRefs%22:%5B%22weave:///ox/bit-reversal-trees/op/chat_completion_request:*%22%5D%7D&filters=%7B%22items%22:%5B%7B%22id%22:0,%22field%22:%22id%22,%22operator%22:%22(string):+equals%22,%22value%22:%220196425c-0650-7df2-8d57-05d272f6d111%22%7D%5D,%22logicOperator%22:%22and%22%7D&peekPath=/ox/bit-reversal-trees/calls/0196425c-0650-7df2-8d57-05d272f6d111?descendentCallId%3D0196425c-0650-7df2-8d57-05efe40cb079%26hideTraceTree%3D0)

It's also been automatically stored here: https://wandb.ai/ox/bit-reversal-trees/weave/objects/0196425c-0650-7df2-8d57-05d272f6d111/versions/KOTE3EQkUWYSdy046vDxHGbma9A8gkuxXUHNpZ3tHH0

#### On the Web Chat

🎉 With the full o1 model: a Python implementation, that _did_ work (with the [python_prompt.md](python_prompt.md) prompt, i.e. with the "extra bit of state" helper function), can be found [here](https://chatgpt.com/share/675735e2-ae68-800a-9394-6e150f809a69):

```python
def invert(tree: Tree) -> Tree:
    def invert_helper(t: Tree, f: bool) -> Tree:
        # f = True: "invert mode"
        # f = False: "interleave mode"
        if isinstance(t, Leaf):
            # In both modes, a leaf just returns itself
            return Leaf(t.value)
        else:
            if f:
                # Invert mode:
                # 1. Recursively invert children
                L = invert_helper(t.left, True)
                R = invert_helper(t.right, True)
                # 2. After inverting both halves, interleave them by calling in interleave mode
                return invert_helper(Node(L, R), False)
            else:
                # Interleave mode:
                # Here, t = Node(X, Y) where X and Y are subtrees already processed in invert mode
                X, Y = t.left, t.right
                if isinstance(X, Leaf) and isinstance(Y, Leaf):
                    # If both are leaves, just pair them
                    return Node(X, Y)
                else:
                    # Otherwise, both must be Nodes due to perfect binary structure
                    # Recursively interleave corresponding sub-subtrees
                    return Node(
                        invert_helper(Node(X.left, Y.left), False),
                        invert_helper(Node(X.right, Y.right), False)
                    )
    return invert_helper(tree, True)
```

Some other examples of "almost solutions" that ended up passing all tests but not satisfying the syntactic constraints (i.e. the verifier LLM didn't flag them as incorrect by mistake), for various generators, can be found in the [python/some_almost_solutions_found](python/some_almost_solutions_found) directory.

### Haskell

The [`src/Lib.hs`](src/Lib.hs) file contains several Haskell implementations of the bit-reversal tree inversion function, including:

- [`invertHuman`](https://github.com/youqad/bit-reversal-trees/blob/51d1793a35c72f9adec6980cf9f08c677d2727bf/src/Lib.hs#L111-L118): My own reference implementation, written by hand, which satisfies all the constraints
- [`invertO1`](https://github.com/youqad/bit-reversal-trees/blob/51d1793a35c72f9adec6980cf9f08c677d2727bf/src/Lib.hs#L121-L128): [One of o1-preview's solutions](https://chatgpt.com/share/670c8a2e-38b8-800a-9d8b-9594b5cf0c76), which passes all the tests but does not satisfy the constraint of not using any helper function
  - This function (or very close variants of it) was found repeatedly by o1-preview with our LLM search (see [here](https://gist.github.com/youqad/7bfe0e5d3f57d30c28a7c73a212b464a#file-o1-preview-01928bc9-3a15-75a1-9cb3-498358b2c0be-json-L115) and [here](https://gist.github.com/youqad/7bfe0e5d3f57d30c28a7c73a212b464a#file-o1-preview-01928a22-ff39-7540-87f3-a3ff1817c756-json-L83) for example), and was flagged as not satisfying the syntactic requirements by the gpt4o-mini verifier most of the times.
- [`invertHumanBasedOnO1`](https://github.com/youqad/bit-reversal-trees/blob/51d1793a35c72f9adec6980cf9f08c677d2727bf/src/Lib.hs#L130-L138): An adaptation of o1-preview's implementation by hand to satisfy the constraint of not using any helper function
- `invertO1MiniWithHelperFns1`, `invertO1MiniWithHelperFns2`, `invertO1MiniWithHelperFns3`: Implementations by o1-mini (cf. [here](https://gist.github.com/youqad/e8870ec417dd37606d06a47412ce5e3b#file-o1-mini-01928789-1b47-74a3-8a72-0cda64404bae-json-L51), [here](https://gist.github.com/youqad/e8870ec417dd37606d06a47412ce5e3b#file-o1-mini-019288df-e441-74c2-8ef2-ccd493c22f23-json-L27) and [here](https://gist.github.com/youqad/e8870ec417dd37606d06a47412ce5e3b#file-o1-mini-019288a0-7ce1-7ab1-bb79-aba2f3dfd148-json-L43)), which pass all the tests but do not satisfy the constraint of not using any helper function
- `invertClaude`: Claude 3.5 Sonnet's implementation, which passes all tests but does not satisfy the syntactic constraint of not using any helper function.


## Testing Custom Implementations

You can test your own implementations of the `invert` function in both Python (with the [`python/test_custom_invert.py`](python/test_custom_invert.py) script) and Haskell (with `stack test`).

### Python Implementation

For Python, your implementation should use the provided `Tree`, `Node`, and `Leaf` classes (from [`python/hypothesis_tests_python.py`](python/hypothesis_tests_python.py)) and have the type signature `def invert(tree: Tree) -> Tree`.

Example Python implementation:
```python
def invert(tree: Tree) -> Tree:
    if isinstance(tree, Node) and isinstance(tree.left, Node) and isinstance(tree.right, Node):
        left_inverted = invert(tree.left)
        right_inverted = invert(tree.right)
        left_left_inverted = invert(left_inverted.left)
        left_right_inverted = invert(left_inverted.right)
        right_left_inverted = invert(right_inverted.left)
        right_right_inverted = invert(right_inverted.right)
        return Node(invert(Node(left_left_inverted, right_left_inverted)), 
                   invert(Node(left_right_inverted, right_right_inverted)))
    return tree
```

In the [`python`](python) directory, to test your Python implementation with the [`python/test_custom_invert.py`](python/test_custom_invert.py) script, either:
- Save it to a file (e.g. `custom_implementation.py`) and run:
   ```bash
   python test_custom_invert.py custom_implementation.py
   ```

- Or paste it directly in the terminal:
   ```bash
   python test_custom_invert.py
   # Then paste your code and press Ctrl+D when done
   ```

### Haskell Implementation

For Haskell, your implementation should use the provided `Tree` data type (from [`src/Lib.hs`](src/Lib.hs)) and have the type signature `invert :: Tree a -> Tree a`.

Example Haskell implementation:
```haskell
invert :: Tree a -> Tree a
invert (Node l@(Node _ _) r@(Node _ _)) = 
    let Node ll lr = invert l
        Node rl rr = invert r
    in Node (invert (Node (invert ll) (invert rl))) 
            (invert (Node (invert lr) (invert rr)))
invert t = t
```

To test your Haskell implementation, replace the `invert` function in `src/Lib.hs` with your implementation and run:
```bash
stack test
```

Both testing methods use property-based testing (Hypothesis for Python, QuickCheck for Haskell) to check whether your implementation correctly performs the bit-reversal permutation on the tree leaves.

## License

This project is open source and available under the [MIT License](LICENSE).
