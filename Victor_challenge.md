# THE PROBLEM

Source: https://gist.github.com/VictorTaelin/45440a737e47b872d7505c6cda27b6aa

🌲 Invert a binary tree! 🌲

Except with 3 catches:
1. It must invert the *keys* ("bit-reversal permutation")
2. It must be a dependency-free, pure recursive function
3. It must have type `Bit -> Tree -> Tree` (i.e., a direct recursion with max 1 bit state)


# WHY THIS PROBLEM?

- It is somehow NOT on the internet. (AFAIK)
- Humans can solve it. (I've done it in ~1h.)
- It requires reasoning. (My head hurts!)
- The solution is simple. (7 lines of code!)
- Obvious pre-requisite to automate CS research.
- Honestly, it would make me believe I'll be automated.

# THE CHALLENGE

I claim no AI will EVER solve this problem.
If you prove me wrong, HOC will grant you $10k!

# RULES

- You must give it an approved prompt, nothing else.
- It must output a correct solution, passing all tests.
- You can use *any* software or AI model.
- You can let it "think" for as long as you want.
- You can propose a new prompt, as long as:
    - It imposes equivalent restrictions.
    - It clearly doesn't help the AI.
    - Up to 1K tokens, all included.
- Common sense applies.

# APPROVED PROMPTS

## Agda Version

```agda
{-# OPTIONS --no-termination-check #-}

-- Let Tree be a Perfect Binary Tree:

data Nat : Set where
  Z : Nat
  S : Nat → Nat
{-# BUILTIN NATURAL Nat #-}

data Bit : Set where
  O : Bit
  I : Bit

data Tree (A : Set) : Nat → Set where
  N : ∀ {d} → Tree A d → Tree A d → Tree A (S d)
  L : Nat → Tree A Z

-- Your goal is to implement an 'invert' function that performs a bit-reversal
-- permutation on a Tree, respecting the following limitations:
-- 1. You can NOT define or use any function other than 'invert'.
-- 2. You can NOT use any type not defined above (Nat, Bit and Tree).
-- 3. You can NOT use loops (but you can call 'invert' recursively).
-- 4. You can NOT use mutability. It must be a pure Agda function.
-- 5. You can use 1 bit of state (as an extra argument).
-- 6. You can use pattern-matching and constructors freely.
-- 
-- Example:
-- input  = (N(N(N(L 0)(L 1))(N(L 2)(L 3)))(N(N(L 4)(L 5))(N(L 6)(L 7))))
-- output = (N(N(N(L 0)(L 4))(N(L 2)(L 6)))(N(N(L 1)(L 5))(N(L 3)(L 7))))
-- Because that's the bit-reversal permutation of the original tree.
-- 
-- Now, complete the program below, with a valid implementation of 'invert':
invert : ∀ {A d} → Bit → Tree A d → Tree A d
```

## TypeScript Version

```typescript
type Nat     = number;
type Bit     = false | true;
type Tree<A> = [Tree<A>, Tree<A>] | Nat;

// Your goal is to implement an 'invert' function that performs a bit-reversal
// permutation on a Tree, respecting the following limitations:
// 1. You can NOT define or use any function other than 'invert'.
// 2. You can NOT use any type not defined above (Nat, Bit and Tree).
// 3. You can NOT use loops (but you can call 'invert' recursively).
// 4. You can NOT use mutability. It must be a pure function.
// 5. You can NOT use primitive JS operators or functions.
// 6. You can use 1 bit of state (as an extra argument).
// 7. You can only use the operations allowed below.
// 
// Operations allowed:
// - Destructing (`const [a,b] = value`)
// - Variables (`const x = value`)
// - Branching (`if (x) { ... } else { ... }`)
// - Recursion (`invert(_, _)')
// - `Array.isArray`
// 
// All other operations are not allowed.
// 
// Example:
// input  = [[[[0,1],[2,3]],[[4,5],[6,7]]]]
// output = [[[[0,4],[2,6]],[[1,5],[3,7]]]]
// Because that's the bit-reversal permutation of the original tree.
// 
// Now, complete the program below, with a valid implementation of 'invert':
function invert<A>(bit: Bit, tree: Tree<A>): Tree<A> {
  ...
}

// A test:
const tree: Tree<Nat> = [[[[0,1],[2,3]],[[4,5],[6,7]]],[[[8,9],[10,11]],[[12,13],[14,15]]]];
console.log(JSON.stringify(invert(true, tree)));
```

# CONCLUSION

✨ If it can't invert a tree, it won't solve P=NP. ✨
