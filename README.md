# Victor Taelin's Bit-Reversal Trees Challenge: Haskell Solutions and Python LLM Search

<p align="center">
  <a href="https://www.haskell.org/ghcup/"><img src="https://img.shields.io/badge/Haskell-5e5086?style=flat-square&logo=haskell&logoColor=white" alt="Haskell"></a>&nbsp;
  <a href="https://www.python.org/downloads/"><img src="https://img.shields.io/badge/python-3670A0?style=flat-square&logo=python&logoColor=ffdd54" alt="Python"></a>&nbsp;
  <a href="https://platform.openai.com/docs/api-reference"><img src="https://img.shields.io/badge/OpenAI-74aa9c?style=flat-square&logo=openai&logoColor=white" alt="OpenAI"></a>&nbsp;
  <a href="https://docs.anthropic.com/en/api/getting-started"><img src="https://img.shields.io/badge/Anthropic-c99d78?style=flat-square&logo=Anthropic&logoColor=black&color=c99d78&" alt="Anthropic"></a>&nbsp;
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

My Haskell prompt, adapted from [Victor's prompt](https://gist.github.com/VictorTaelin/45440a737e47b872d7505c6cda27b6aa) and kept under 1k tokens, is in [`haskell_prompt.md`](haskell_prompt.md).

> [!NOTE]
> For full details about the challenge, see: 
> - [Victor's tweet](https://x.com/VictorTaelin/status/1844886809005687270)
> - [Victor's gist](https://gist.github.com/VictorTaelin/45440a737e47b872d7505c6cda27b6aa)

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

## Getting Started

### Prerequisites

1. Haskell with Stack
2. Python 3.7+
3. OpenAI API key (for GPT models)
4. Anthropic API key (for Claude models)
5. Weights & Biases API key (optional, for LLM logging)

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
   WANDB_API_KEY=your_wandb_api_key
   GENERATOR_MODEL_NAME=claude-3-sonnet-20240229 # or gpt-4o, or o1-mini, or o1-preview
   VERIFIER_MODEL_NAME=gpt-4o-mini # for example
   NUM_INITIAL_SOLUTIONS=10 # Number of suggested solutions to the initial prompt (each of them giving rise to a conversation)
   MAX_ROUNDS=8 # Number of rounds/turns per conversation, in the LLM search
   ```

## Haskell LLM-based Program Synthesis search

6. Run the LLM search:
   ```sh
   export PROGRAM_SYNTHESIS_LANGUAGE=haskell
   python solve_challenge.py
   ```

7. To test Haskell's QuickCheck tests:
   ```sh
   python -m pytest test_solve_challenge_quickcheck.py
   ```

## Python LLM-based Program Synthesis search

8. Run the Python LLM search:
   ```sh
   export PROGRAM_SYNTHESIS_LANGUAGE=python
   python solve_challenge.py
   ```

9. To test Python's Hypothesis tests:
   ```sh
   python -m pytest test_solve_challenge_hypothesis.py
   ```

## Haskell Implementations

The [`src/Lib.hs`](src/Lib.hs) file contains several Haskell implementations of the bit-reversal tree inversion function, including:

- [`invertHuman`](https://github.com/youqad/bit-reversal-trees/blob/51d1793a35c72f9adec6980cf9f08c677d2727bf/src/Lib.hs#L111-L118): My own reference implementation, written by hand, which satisfies all the constraints
- [`invertO1`](https://github.com/youqad/bit-reversal-trees/blob/51d1793a35c72f9adec6980cf9f08c677d2727bf/src/Lib.hs#L121-L128): [One of o1-preview's solutions](https://chatgpt.com/share/670c8a2e-38b8-800a-9d8b-9594b5cf0c76), which passes all the tests but does not satisfy the constraint of not using any helper function
  - This function (or very close variants of it) was found repeatedly by o1-preview with our LLM search (see [here](https://gist.github.com/youqad/7bfe0e5d3f57d30c28a7c73a212b464a#file-o1-preview-01928bc9-3a15-75a1-9cb3-498358b2c0be-json-L115) and [here](https://gist.github.com/youqad/7bfe0e5d3f57d30c28a7c73a212b464a#file-o1-preview-01928a22-ff39-7540-87f3-a3ff1817c756-json-L83) for example), and was flagged as not satisfying the syntactic requirements by the gpt4o-mini verifier most of the times.
- [`invertHumanBasedOnO1`](https://github.com/youqad/bit-reversal-trees/blob/51d1793a35c72f9adec6980cf9f08c677d2727bf/src/Lib.hs#L130-L138): An adaptation of o1-preview's implementation by hand to satisfy the constraint of not using any helper function
- `invertO1MiniWithHelperFns1`, `invertO1MiniWithHelperFns2`, `invertO1MiniWithHelperFns3`: Implementations by o1-mini (cf. [here](https://gist.github.com/youqad/e8870ec417dd37606d06a47412ce5e3b#file-o1-mini-01928789-1b47-74a3-8a72-0cda64404bae-json-L51), [here](https://gist.github.com/youqad/e8870ec417dd37606d06a47412ce5e3b#file-o1-mini-019288df-e441-74c2-8ef2-ccd493c22f23-json-L27) and [here](https://gist.github.com/youqad/e8870ec417dd37606d06a47412ce5e3b#file-o1-mini-019288a0-7ce1-7ab1-bb79-aba2f3dfd148-json-L43)), which pass all the tests but do not satisfy the constraint of not using any helper function
- `invertClaude`: Claude 3.5 Sonnet's implementation, which passes all tests but does not satisfy the syntactic constraint of not using any helper function.

## Python LLM Search

The [`python/solve_challenge.py`](python/solve_challenge.py) script uses OpenAI's GPT models to search for valid Haskell implementations of the bit-reversal tree inversion function. It follows these steps:

1. Generate initial solutions to the prompt [`haskell_prompt.md`](haskell_prompt.md) using the generator LLM (`GENERATOR_MODEL_NAME` in `.env`)

2. For each one of them, verify the syntactic correctness using the verifier LLM (`VERIFIER_MODEL_NAME` in `.env`)
3. For the syntactically correct ones, run the suggested solutions through the Haskell QuickCheck tests in [`test/DynamicSpec.hs`](test/DynamicSpec.hs)

4. Rinse and repeat: iterate and the refine solutions based on the test results




## License

This project is open source and available under the [MIT License](LICENSE).
