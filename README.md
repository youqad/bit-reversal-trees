# Victor Taelin's Bit-Reversal Trees Challenge: Haskell Solutions and Python LLM Search

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
3. OpenAI API key (for LLM search)
4. Weights & Biases API key (optional, for LLM logging)

### Step-by-Step Guide

#### Haskell

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


#### Python
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
   WANDB_API_KEY=your_wandb_api_key
   GENERATOR_MODEL_NAME=gpt-4o
   VERIFIER_MODEL_NAME=gpt-4o-mini
   NUM_INITIAL_SOLUTIONS=10 # Number of suggested solutions to the initial prompt (each of them giving rise to a conversation)
   MAX_ROUNDS=8 # Number of rounds/turns per conversation, in the LLM search
   ```

6. Run the Python LLM search:
   ```sh
   python solve_challenge.py
   ```

7. To run the Python tests:
   ```sh
   python -m pytest test_solve_challenge.py
   ```

## Haskell Implementations

The [`src/Lib.hs`](src/Lib.hs) file contains several Haskell implementations of the bit-reversal tree inversion function, including:

- `invertHuman`: My own reference implementation, written by hand, which satisfies all the constraints
- `invertO1`: [One of o1-preview's solutions](https://chatgpt.com/share/670c8a2e-38b8-800a-9d8b-9594b5cf0c76), which passes all the tests but does not satisfy the constraint of not using any helper function
- `invertHumanBasedOnO1`: An adaptation of o1-preview's implementation by hand to satisfy the constraint of not using any helper function
- `invertO1MiniWithHelperFns`: [One of o1-mini's solutions](https://gist.github.com/youqad/e8870ec417dd37606d06a47412ce5e3b#file-o1-mini-01928789-1b47-74a3-8a72-0cda64404bae-json-L51), which passes all the tests but does not satisfy the constraint of not using any helper function

## Python LLM Search

The [`python/solve_challenge.py`](python/solve_challenge.py) script uses OpenAI's GPT models to search for valid Haskell implementations of the bit-reversal tree inversion function. It follows these steps:

1. Generate initial solutions to the prompt [`haskell_prompt.md`](haskell_prompt.md) using the generator LLM (`GENERATOR_MODEL_NAME` in `.env`)

2. For each one of them, verify the syntactic correctness using the verifier LLM (`VERIFIER_MODEL_NAME` in `.env`)
3. For the syntactically correct ones, run the suggested solutions through the Haskell QuickCheck tests in [`test/DynamicSpec.hs`](test/DynamicSpec.hs)

4. Rinse and repeat: iterate and the refine solutions based on the test results




## License

This project is open source and available under the [MIT License](LICENSE).
