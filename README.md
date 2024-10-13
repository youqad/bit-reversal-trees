# Bit-Reversal Trees

This project implements and tests the bit reversal permutation on binary trees, as proposed in [Victor Taelin's challenge](https://x.com/VictorTaelin/status/1844886809005687270).

## The Challenge

Victor's challenge involves coming up with a prompt to show that a LLM is capable of implementing a solution to the problem of inverting a binary tree with specific requirements:
1. It must invert the keys using a "bit-reversal permutation"
2. It must be a dependency-free, pure recursive function
3. The function must have the type signature `Bit -> Tree -> Tree` (direct recursion with max 1 bit state)

My Haskell prompt is in [`haskell_prompt.md`](haskell_prompt.md).

For full details of the challenge, see: 
- [Victor's tweet](https://x.com/VictorTaelin/status/1844886809005687270)
- [Victor's gist](https://gist.github.com/VictorTaelin/45440a737e47b872d7505c6cda27b6aa)

## Getting Started

### Prerequisites

Make sure you have [Stack](https://docs.haskellstack.org/en/stable/install_and_upgrade/) installed on your system.

### Building the Project

To build the project, run:

```bash
stack build
```

To run the tests, run:

```bash
stack test
```

## License

The code in this repo is open source and available under the [MIT License](LICENSE).
