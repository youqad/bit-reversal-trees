import os
import json
import subprocess
import concurrent.futures
import copy
import sys
sys.path.append(os.path.dirname(os.path.dirname(os.path.abspath(__file__))))
sys.path.insert(0, os.path.abspath("../../"))
sys.path.insert(0, os.path.abspath("../"))
from utils import *
import pexpect
import weave
from termcolor import colored
from tqdm import tqdm
from pprint import pprint
from dotenv import load_dotenv, find_dotenv
load_dotenv(find_dotenv())

weave.init('bit-reversal-trees')

def create_ghci_process():
    ghci = pexpect.spawn('stack ghci --ghci-options=-ignore-dot-ghci ../test/DynamicSpec.hs', encoding='utf-8')
    ghci.sendline(':set -ignore-dot-ghci')
    ghci.sendline(':set -XOverloadedStrings')
    ghci.sendline(':set -XScopedTypeVariables')
    ghci.sendline(':set -XTemplateHaskell')
    ghci.sendline(':set +m')
    ghci.sendline(':set prompt "\ESC[1;34m\STX%s\n\ESC[0;34m\STXλ> \ESC[m\STX"')
    ghci.expect_exact('λ> ')
    return ghci


def run_tests(invert_code, ghci):
    """
    Run Hspec tests and return the result by sending the invert code directly into GHCi.
    """
    try:
        ghci.expect_exact('λ> ')
        # Use :{ and :} to enter multi-line mode in GHCi
        ghci.sendline(':{')
        ghci.sendline('let ' + invert_code.replace('\n', '\n    '))
        ghci.sendline(':}')
        ghci.expect_exact('λ> ')

        # Run the tests
        ghci.sendline('testInvert invert')
        index = ghci.expect(['All examples passed', 'Failures:', pexpect.EOF, pexpect.TIMEOUT], timeout=120)
        output = ghci.before + ghci.after

        if 'Failures:' in output:
            start = output.index('Invert Function for Bit-Reversal Trees')
            end = output.index('Failures:') + len('Failures:')
            relevant_output = output[start:end].strip()
        else:
            relevant_output = "All examples passed"

        if 'All examples passed' in relevant_output:
            print(colored("✅ All tests passed!", "green"))
            return True, relevant_output
        else:
            print(colored("❌ Tests failed.", "red"))
            return False, relevant_output

    except Exception as e:
        print(colored("❌ Error running tests.", "red"))
        return False, str(e)

def main():
    with open(HASKELL_PROMPT_FILE, "r") as f:
        initial_prompt = f.read()

    initial_message = {
        "role": "system",
        "content": initial_prompt,
    }

    response = chat_completion_request(
        [initial_message],
        model=GENERATOR_MODEL_NAME,
        n=NUM_INITIAL_SOLUTIONS
    )

    conversations = []
    for idx, choice in enumerate(response.choices):
        assistant_content = choice.message.content
        messages = [copy.deepcopy(initial_message)]
        messages.append({
            "role": "assistant",
            "content": assistant_content,
        })
        conversations.append({
            "messages": messages,
            "round_num": 1,
            "idx": idx
        })

    ghci = create_ghci_process()

    solutions = []
    with tqdm(total=len(conversations), desc="Processing conversations") as pbar:
        for conv in conversations:
            full_conv_or_solution, success = process_conversation(conv, ghci)
            if success:
                print(colored(f"Conversation {conv['idx']}: Found a valid implementation!", "green"))
                print(colored(full_conv_or_solution, "light_green"))
                solutions.append(full_conv_or_solution)
            pbar.update(1)

    print(colored(f"Found {len(solutions)} solutions!", "green"))
    if solutions:
        with open("solutions.txt", "a+") as f:
            for solution in solutions:
                f.write(solution + "\n\n")
        print(colored("Solutions have been saved to 'solutions.txt' 🚀", "light_green"))
    else:
        print(colored("No solution found. ❌", "magenta"))
    ghci.sendline(':q')  # Quit GHCi
    ghci.expect(pexpect.EOF)
    ghci.terminate()


def process_conversation(conversation, ghci):
    messages = conversation["messages"]
    idx = conversation["idx"]

    first_assistant_message = messages[-1]
    feedback, success = verify_response(first_assistant_message.content, ghci)
    if success:
        return feedback, success
    messages.append({"role": "user", "content": feedback})

    pbar = tqdm(total=MAX_ROUNDS, desc=f"Conversation {idx}", leave=False)

    while conversation["round_num"] <= MAX_ROUNDS:
        print(colored(f"\n=== Conversation {idx} - Round {conversation['round_num'] + 1} ===\n", "cyan"))

        response = chat_completion_request(messages, model=GENERATOR_MODEL_NAME)

        assistant_message = response.choices[0].message
        assistant_content = assistant_message.content

        feedback, success = verify_response(assistant_content, ghci)

        if success:
            return feedback, success

        messages.append({"role": "user", "content": feedback})

        conversation["round_num"] += 1
        pbar.update(1)
    else:
        print(colored(f"🚫 Conversation {idx}: Reached maximum number of rounds without finding a valid implementation.", "red"))
    pbar.close()

    return conversation, False

verifier_function_calling_list = [
    {
        "name": "extract_invert_function",
        "description": "Extract the invert function and verify syntactic requirements.",
        "parameters": {
            "type": "object",
            "properties": {
                "invert_function_code": {
                    "type": "string",
                    "description": "The Haskell code for the `invert :: Tree a -> Tree a` function.",
                },
                "satisfies_requirements": {
                    "type": "boolean",
                    "description": "Whether the proposed invert function satisfies the syntactic requirements, i.e.:\n1. The `invert` function must be a standalone, pure, and recursive function. It must NOT rely on any helper function. The ONLY exception about using a helper function is if it uses only one extra bit of state (i.e. `invert` relies on a helper function `invert' :: Bool -> Tree a -> Tree a`).\n2. It only uses recursion (no loops).\n3. It maintains purity (no side effects or mutability).",
                },
            },
            "required": ["invert_function_code", "satisfies_requirements"],
        },
    },
]
verifier_function_calling_schema = {"name": "extract_invert_function"}

def verify_response(assistant_content, ghci):
    success = False
    feedback = None

    verifier_messages = [
        {
            "role": "system",
            "content": "You are a Haskell code verifier. Extract the `invert :: Tree a -> Tree a` function from the assistant's response, and check if it satisfies the syntactic requirements.",
        },
        {
            "role": "user",
            "content": assistant_content,
        },
    ]

    response_verifier = chat_completion_request(
        verifier_messages,
        model=VERIFIER_MODEL_NAME,
        functions=verifier_function_calling_list,
        function_call=verifier_function_calling_schema
    )

    assistant_verifier_message = response_verifier.choices[0].message
    result_invert_function = execute_function_call(assistant_verifier_message, "extract_invert_function")

    if not result_invert_function:
        print(colored("❌ Verifier did not return a function call. Skipping this response.", "red"))
        feedback = "The verifier was unable to find the `invert :: Tree a -> Tree a` function in your response. Please provide a clear implementation of the invert function."
    else:
        invert_code = result_invert_function.get("invert_function_code")
        satisfies_requirements = result_invert_function.get("satisfies_requirements")

        if not satisfies_requirements:
            feedback = (
                "The proposed invert function does not satisfy the syntactic requirements. "
                "Please revise your implementation."
            )
            print(colored("❌ Syntactic check failed.", "red"))
            print(colored(f"Assistant's code:\n{invert_code}", "yellow"))
        else:
            tests_passed, test_output = run_tests(invert_code, ghci)

            if tests_passed:
                print(colored("🎉 Successfully found a valid implementation!", "green"))
                print(colored("Assistant's code:", "cyan"))
                print(colored(invert_code, "yellow"))
                success = True
                feedback = invert_code
            else:
                feedback = (
                    "Your invert function is incorrect. "
                    "Here is the test output:\n"
                    + test_output
                )
                print(colored("Assistant's code:", "cyan"))
                print(colored(invert_code, "yellow"))

    return feedback, success



if __name__ == "__main__":
    main()
