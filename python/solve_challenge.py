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
import warnings

warnings.filterwarnings("ignore", category=DeprecationWarning, module="wandb")
warnings.filterwarnings("ignore", category=DeprecationWarning, module="weave")

load_dotenv(find_dotenv())

weave.init('bit-reversal-trees')

def create_ghci_process():
    print("Starting GHCi process...")
    # Set logfile to sys.stdout to print GHCi output to the console
    ghci = pexpect.spawn(
        'stack ghci --ghci-options=-ignore-dot-ghci ../test/DynamicSpec.hs',
        encoding='utf-8',
        logfile=sys.stdout
    )
    ghci.expect('GHCi, version.*', timeout=60)
    print("GHCi started. Setting up environment...")
    ghci.sendline(':set -ignore-dot-ghci')
    ghci.sendline(':set -XOverloadedStrings')
    ghci.sendline(':set -XScopedTypeVariables')
    ghci.sendline(':set -XTemplateHaskell')
    ghci.sendline(':set +m')
    ghci.sendline(':set prompt "ghci_prompt> "')
    ghci.expect_exact('ghci_prompt> ', timeout=30)
    
    print("Verifying testInvert availability...")
    ghci.sendline(':t testInvert')
    index = ghci.expect(['testInvert ::', 'Not in scope'], timeout=30)
    if index == 1:
        raise Exception("testInvert function is not available in the GHCi environment")
    
    ghci.expect_exact('ghci_prompt> ', timeout=30)
    print("GHCi process ready.")
    return ghci

def run_tests(invert_code, ghci):
    """
    Run Hspec tests and return the result by sending the invert code directly into GHCi.
    """
    try:
        print("Sending invert code to GHCi...")
        ghci.sendline(':{')
        ghci.sendline('let ' + invert_code.strip().replace('\n', '\n    '))
        ghci.sendline(':}')
        print("Waiting for GHCi prompt after sending code...")
        ghci.expect_exact('ghci_prompt> ', timeout=30)

        print("Running testInvert...")
        ghci.sendline('testInvert invert')

        print("Waiting for test results...")
        ghci.expect_exact('ghci_prompt> ', timeout=120)

        output = ghci.before

        if '+++ OK, passed' in output:
            print(colored("✅ All tests passed!", "green"))
            return True, output
        elif 'Failures:' in output or 'error:' in output:
            print(colored("❌ Tests failed.", "red"))
            return False, extract_failed_info(output)
        else:
            print(colored("❌ Unknown test result.", "red"))
            return False, output

    except pexpect.TIMEOUT:
        print(colored("❌ Timeout while running tests.", "red"))
        print(f"Last output: {ghci.before}")
        return False, "Timeout while running tests"
    except Exception as e:
        print(colored(f"❌ Error running tests: {str(e)}", "red"))
        print(f"Last output: {ghci.before}")
        return False, str(e)

def extract_failed_info(output):
    """
    Extract the relevant failure information from the test output.
    """
    lines = output.split('\n')
    start_index = None
    end_index = None
    
    for i, line in enumerate(lines):
        if line.strip().startswith("Failed:"):
            start_index = i
        elif start_index and line.strip() == "":
            end_index = i
            break
    
    if start_index and end_index:
        failed_info = '\n'.join(lines[start_index:end_index])
        return failed_info.replace("Inverted flattened:", "Your inverted flattened:")
    else:
        return "Failed to extract detailed failure information."

def main():
    with open(HASKELL_PROMPT_FILE, "r") as f:
        initial_prompt = f.read()

    initial_role = "user" if GENERATOR_MODEL_NAME.startswith("o1") else "system"

    initial_message = {
        "role": initial_role,
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
        messages = [initial_message]
        messages.append({
            "role": "assistant",
            "content": assistant_content,
        })
        conversations.append({
            "messages": messages,
            "round_num": 1,
            "idx": idx + 1
        })

    ghci = create_ghci_process()

    solutions = []
    with tqdm(total=len(conversations), desc="Processing conversations") as pbar:
        for conv in conversations:
            full_conv_or_solution, success = process_conversation(conv, ghci)
            if success:
                print(colored(f"✨ Conversation {conv['idx']}: Found a valid implementation! 🎉 ✨", "green"))
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
    
    # Gracefully terminate the GHCi process
    try:
        ghci.sendline(':q')
        ghci.expect(pexpect.EOF, timeout=5)
    except pexpect.TIMEOUT:
        print(colored("GHCi didn't terminate as expected. Forcing termination.", "yellow"))
    finally:
        ghci.close(force=True)


def process_conversation(conversation, ghci):
    """
    Process a single conversation, including multiple rounds of verification.
    """
    idx = conversation["idx"]

    print(colored(f"\n=== Conversation {idx} - Round {conversation['round_num']} ===\n", "cyan"))
    
    messages = conversation["messages"]

    first_assistant_message = messages[-1]
    feedback, success = verify_response(first_assistant_message["content"], ghci)
    if success:
        return feedback, success
    messages.append({"role": "user", "content": feedback})

    conversation["round_num"] += 1

    pbar = tqdm(total=MAX_ROUNDS, desc=f"Conversation {idx}", leave=False)

    while conversation["round_num"] <= MAX_ROUNDS:
        print(colored(f"\n=== Conversation {idx} - Round {conversation['round_num']} ===\n", "cyan"))

        response = chat_completion_request(messages, model=GENERATOR_MODEL_NAME)

        assistant_message = response.choices[0].message
        assistant_content = assistant_message.content
        messages.append({"role": "assistant", "content": assistant_content})

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
        "description": "Extract the `invert :: Tree a -> Tree a` function (and ONLY this function) and verify if it satisfies the syntactic requirements.",
        "parameters": {
            "type": "object",
            "properties": {
                "invert_function_code": {
                    "type": "string",
                    "description": "The Haskell code for the `invert :: Tree a -> Tree a` function, and ONLY this function.",
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
            "content": "You are a Haskell code verifier. Extract the `invert :: Tree a -> Tree a` function from the user's message, and check if it satisfies the syntactic requirements.",
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
