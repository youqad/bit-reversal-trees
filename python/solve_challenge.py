import os
import json
import sys

sys.path.append(os.path.dirname(os.path.dirname(os.path.abspath(__file__))))
sys.path.insert(0, os.path.abspath("../../"))
sys.path.insert(0, os.path.abspath("../"))
from utils import (
    chat_completion_request,
    execute_function_call,
    get_verifier_schemas_and_messages,
    create_ghci_process,
    extract_failed_info,
    get_call_dict,
    send_desktop_notification,
    GENERATOR_MODEL_NAME,
    VERIFIER_MODEL_NAME,
    NUM_INITIAL_SOLUTIONS,
    HASKELL_PROMPT_FILE,
    PYTHON_PROMPT_FILE,
    PROGRAM_SYNTHESIS_LANGUAGE,
    MAX_CONSECUTIVE_TIMEOUTS,
    MAX_ROUNDS,
)
import pexpect
import weave
from termcolor import colored
from tqdm import tqdm
from dotenv import load_dotenv, find_dotenv
import warnings
import tempfile
import traceback
from hypothesis_tests_python import (
    Tree,
    Leaf,
    Node,
    flatten_tree,
    reverse_bits,
    bit_reverse_permutation,
    perfect_binary_trees,
    # build_tree,
    TestInvertFunction,
)
import unittest
import io

warnings.filterwarnings("ignore", category=DeprecationWarning, module="wandb")
warnings.filterwarnings("ignore", category=DeprecationWarning, module="weave")

load_dotenv(find_dotenv())

weave_client = weave.init("bit-reversal-trees")

consecutive_timeouts = 0


def run_tests(
    invert_code, ghci=None, program_synthesis_language=PROGRAM_SYNTHESIS_LANGUAGE
):
    """
    Haskell: Run Hspec tests and return the result by sending the invert code directly into GHCi.
    Python: Run the tests using hypothesis and return the result.
    """
    global consecutive_timeouts
    if program_synthesis_language == "haskell":
        try:
            print("Sending invert code to GHCi...")
            ghci.sendline(":{")
            ghci.sendline("let " + invert_code.strip().replace("\n", "\n    "))
            ghci.sendline(":}")
            print("Waiting for GHCi prompt after sending code...")
            while True:
                index = ghci.expect(
                    ["ghci_prompt> ", r"Display all \d+ possibilities\? \(y or n\)"],
                    timeout=30,
                )
                if index == 0:
                    # GHCi prompt received, proceed
                    break
                elif index == 1:
                    # GHCi is asking to display all possibilities, send 'n' to proceed
                    ghci.sendline("n")
            # ghci.expect_exact("ghci_prompt> ", timeout=30)

            print("Running testInvert...")
            ghci.sendline("testInvert invert")

            print("Waiting for test results...")
            ghci.expect_exact("ghci_prompt> ", timeout=120)

            output = ghci.before

            if "+++ OK, passed" in output:
                print(colored("✅ All tests passed!", "green"))
                consecutive_timeouts = 0
                return True, output
            elif "Failures:" in output or "error:" in output:
                print(colored("❌ Tests failed.", "red"))
                consecutive_timeouts = 0
                return False, extract_failed_info(
                    output, program_synthesis_language=PROGRAM_SYNTHESIS_LANGUAGE
                )
            else:
                print(colored("❌ Unknown test result.", "red"))
                consecutive_timeouts = 0
                return False, output

        except pexpect.TIMEOUT:
            print(colored("❌ Timeout while running tests.", "red"))
            print(f"Last output: {ghci.before}")
            consecutive_timeouts += 1
            return False, "Test timed out."
        except Exception as e:
            print(colored(f"❌ Error running tests: {str(e)}", "red"))
            print(f"Last output: {ghci.before}")
            consecutive_timeouts = 0
            return False, str(e)
    elif program_synthesis_language == "python":
        try:
            # local namespace for exec
            namespace = {
                "__builtins__": __builtins__,
                "Tree": Tree,
                "Leaf": Leaf,
                "Node": Node,
            }
            exec(invert_code, namespace, namespace)

            invert_function = namespace.get("invert", None)
            print(f"Invert function: {invert_function}")
            if invert_function is None:
                consecutive_timeouts = 0
                return False, "`invert` function not found in the provided code."

            # Replace the placeholder invert function with the generated one
            TestInvertFunction.set_invert_function(invert_function)

            loader = unittest.TestLoader()
            suite = loader.loadTestsFromTestCase(TestInvertFunction)
            runner = unittest.TextTestRunner()
            result = runner.run(suite)

            if result.wasSuccessful():
                consecutive_timeouts = 0
                print(colored("✅ All tests passed! 🎉", "green"))
                return True, "All tests passed!"
            else:
                consecutive_timeouts = 0
                # collect the failures
                failures = []
                for failure in result.failures + result.errors:
                    test_case, traceback_str = failure
                    failures.append(traceback_str)
                print(colored("❌ Tests failed.\n", "red"))
                # print(colored("\n".join(failures), "red"))
                # print("\n")
                return False, "\n".join(failures)

        except Exception as e:
            consecutive_timeouts = 0
            print(colored(f"❌ Error during testing: {str(e)}", "red"))
            return False, f"Error during testing: {str(e)}"
    else:
        raise ValueError(
            f"Unsupported program synthesis language: {program_synthesis_language}"
        )


def main():
    if PROGRAM_SYNTHESIS_LANGUAGE == "haskell":
        PROMPT_FILE = HASKELL_PROMPT_FILE
    elif PROGRAM_SYNTHESIS_LANGUAGE == "python":
        PROMPT_FILE = PYTHON_PROMPT_FILE
    else:
        raise ValueError(
            f"Unsupported PROGRAM_SYNTHESIS_LANGUAGE: {PROGRAM_SYNTHESIS_LANGUAGE}"
        )

    with open(PROMPT_FILE, "r") as f:
        initial_prompt = f.read()

    initial_role = "user" if GENERATOR_MODEL_NAME.startswith("o1") else "system"

    initial_message = {
        "role": initial_role,
        "content": initial_prompt,
    }

    conversations = []

    if (
        GENERATOR_MODEL_NAME.startswith("o1")
        or GENERATOR_MODEL_NAME.startswith("claude")
        or GENERATOR_MODEL_NAME.startswith("anthropic")
    ):
        # For o1 models and Claude, make separate requests (n>1 is not supported)
        for idx in range(NUM_INITIAL_SOLUTIONS):
            response, call = chat_completion_request.call(
                [initial_message], model=GENERATOR_MODEL_NAME, n=1, temperature=1
            )
            response_choice = response.choices[0]
            assistant_content = response_choice.message.content
            messages = [
                initial_message,
                {
                    "role": "assistant",
                    "content": assistant_content,
                },
            ]
            call_ids_dict = {
                "call_id": call.id,
                "trace_id": call.trace_id,
                "parent_id": call.parent_id,
            }
            conversations.append(
                {
                    "messages": messages,
                    "round_num": 1,
                    "idx": idx + 1,
                    "call_ids_dict": call_ids_dict,
                    "call": call,
                    "response_choice": response_choice,
                }
            )
    else:
        response, call = chat_completion_request.call(
            [initial_message],
            model=GENERATOR_MODEL_NAME,
            n=NUM_INITIAL_SOLUTIONS,
            temperature=1,
        )
        for idx, choice in enumerate(response.choices):
            assistant_content = choice.message.content
            messages = [
                initial_message,
                {
                    "role": "assistant",
                    "content": assistant_content,
                },
            ]
            call_ids_dict = {
                "call_id": call.id,
                "trace_id": call.trace_id,
                "parent_id": call.parent_id,
            }
            conversations.append(
                {
                    "messages": messages,
                    "round_num": 1,
                    "idx": idx + 1,
                    "call_ids_dict": call_ids_dict,
                    "call": call,
                    "response_choice": choice,
                }
            )

    if PROGRAM_SYNTHESIS_LANGUAGE == "haskell":
        ghci = create_ghci_process()
    else:
        ghci = None

    solutions = []
    with tqdm(total=len(conversations), desc="Processing conversations") as pbar:
        for conv in conversations:
            full_conv_or_solution, success, ghci = process_conversation(conv, ghci=ghci)
            if success:
                print(
                    colored(
                        f"✨ Conversation {conv['idx']}: Found a valid implementation! 🎉 ✨",
                        "green",
                    )
                )
                print(colored(full_conv_or_solution, "light_green"))
                solutions.append(
                    (
                        full_conv_or_solution,
                        conv["call_ids_dict"]["call_id"],
                        conv["call_ids_dict"]["trace_id"],
                        conv["call_ids_dict"]["parent_id"],
                    )
                )
            pbar.update(1)

    print(colored(f"Found {len(solutions)} solutions!", "green"))
    if solutions:
        with open("solutions_final.txt", "a+") as f:
            for solution, call_id, trace_id, parent_id in solutions:
                f.write(f"Call ID: {call_id}\n")
                f.write(f"Trace ID: {trace_id}\n")
                f.write(f"Parent ID: {parent_id}\n")
                f.write(solution + "\n\n")
        print(
            colored(
                "Solutions have been saved to 'solutions_final.txt' 🚀", "light_green"
            )
        )
    else:
        print(colored("No solution found. ❌", "magenta"))

    # Gracefully terminate the GHCi process
    if ghci is not None:
        try:
            ghci.sendline(":q")
            ghci.expect(pexpect.EOF, timeout=5)
        except pexpect.TIMEOUT:
            print(
                colored(
                    "GHCi didn't terminate as expected. Forcing termination.", "yellow"
                )
            )
        finally:
            ghci.close(force=True)

    # Send desktop notification
    send_desktop_notification(f"Search complete: {len(solutions)} solutions found!")


def process_conversation(conversation, ghci=None):
    """
    Process a single conversation, including multiple rounds of verification.
    If a test times out, it will restart the GHCi process.
    Return a triple: the solution if successful or the final conversation if not, whether the conversation was successful (boolean), and the GHCi process.
    """
    global consecutive_timeouts
    idx = conversation["idx"]

    print(
        colored(
            f"\n=== Conversation {idx} - Round {conversation['round_num']} ===\n",
            "cyan",
        )
    )

    messages = conversation["messages"]
    first_assistant_message = messages[-1]
    call_ids_dict = conversation["call_ids_dict"]
    response_choice = conversation["response_choice"]
    feedback, success, ghci = verify_response(
        first_assistant_message["content"],
        call_ids_dict,
        conversation["call"],
        response_choice,
        ghci=ghci,
    )
    if success:
        write_solution(feedback, call_ids_dict, conversation["call"], response_choice)
        return feedback, success, ghci

    messages.append({"role": "user", "content": feedback})

    conversation["round_num"] += 1

    pbar = tqdm(total=MAX_ROUNDS, desc=f"Conversation {idx}", leave=False)

    while conversation["round_num"] <= MAX_ROUNDS:
        print(
            colored(
                f"\n=== Conversation {idx} - Round {conversation['round_num']} ===\n",
                "cyan",
            )
        )

        response, call = chat_completion_request.call(
            messages, model=GENERATOR_MODEL_NAME, temperature=1
        )
        if response is None or call is None:
            raise Exception("chat_completion_request.call() returned None")
        response_choice = response.choices[0]
        assistant_message = response_choice.message
        assistant_content = assistant_message.content
        messages.append({"role": "assistant", "content": assistant_content})
        conversation["messages"] = messages
        conversation["call"] = call
        conversation["response_choice"] = response_choice

        call_ids_dict = {
            "call_id": call.id,
            "trace_id": call.trace_id,
            "parent_id": call.parent_id,
        }
        conversation["call_ids_dict"] = call_ids_dict

        feedback, success, ghci = verify_response(
            assistant_content, call_ids_dict, call, response_choice, ghci=ghci
        )

        if success:
            write_solution(feedback, call_ids_dict, call, response_choice)
            return feedback, success, ghci

        messages.append({"role": "user", "content": feedback})

        conversation["round_num"] += 1
        pbar.update(1)
    else:
        print(
            colored(
                f"🚫 Conversation {idx}: Reached maximum number of rounds without finding a valid implementation.",
                "red",
            )
        )
    pbar.close()

    return conversation, False, ghci


def write_solution(solution, call_ids_dict, call, choice):
    """
    Write a solution to the 'solutions.txt' file along with the Call ID, Trace ID, and Parent ID.
    Also log the call to 'solutions_calls.jsonl'.
    """
    with open("solutions.txt", "a+") as f:
        f.write(f"Call ID: {call_ids_dict['call_id']}\n")
        f.write(f"Trace ID: {call_ids_dict['trace_id']}\n")
        f.write(f"Parent ID: {call_ids_dict['parent_id']}\n")
        f.write(solution + "\n\n")
    print(
        colored(
            f"Solution has been saved to 'solutions.txt' with Call ID: {call_ids_dict['call_id']} 🚀",
            "light_green",
        )
    )
    call_dict = get_call_dict(call, choice, weave_client)
    call_dict["solution"] = solution
    with open("solutions_calls.jsonl", "a+") as f:
        json.dump(call_dict, f)
        f.write("\n")
    print(
        colored(
            f"Call details logged to 'solutions_calls.jsonl' for Call ID: {call_ids_dict['call_id']} 📝",
            "light_green",
        )
    )


def verify_response(assistant_content, call_ids_dict, call, response_choice, ghci=None):
    """
    Verify the response by extracting the invert function from `assistant_content` and running tests.
    If a test times out, it will restart the GHCi process in Haskell mode.
    Return a tuple: the feedback message (string), a boolean indicating success (boolean), and the GHCi process.
    """
    global consecutive_timeouts
    success = False
    feedback = None

    verifier_function_schemas, verifier_messages = get_verifier_schemas_and_messages(
        assistant_content, program_synthesis_language=PROGRAM_SYNTHESIS_LANGUAGE
    )

    response_verifier = chat_completion_request(
        verifier_messages,
        model=VERIFIER_MODEL_NAME,
        functions=verifier_function_schemas,
        function_call={"name": "extract_invert_function"},
    )

    assistant_verifier_message = response_verifier.choices[0].message
    result_invert_function = execute_function_call(
        assistant_verifier_message, "extract_invert_function"
    )

    if not result_invert_function:
        print(
            colored(
                "❌ Verifier did not return a function call. Skipping this response.",
                "red",
            )
        )
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
            tests_passed, test_output = run_tests(invert_code, ghci=ghci)

            if tests_passed:
                print(colored("🎉 Successfully found a valid implementation!", "green"))
                print(colored("Assistant's code:", "cyan"))
                print(colored(invert_code, "yellow"))
                success = True
                feedback = invert_code
                call_dict = get_call_dict(call, response_choice, weave_client)
                weave.publish(
                    {
                        "call_id": call_ids_dict["call_id"],
                        "trace_id": call_ids_dict["trace_id"],
                        "parent_id": call_ids_dict["parent_id"],
                        "solution": feedback,
                        "call": call_dict,
                        "success": True,
                    },
                    call_ids_dict["call_id"],
                )
            else:
                if consecutive_timeouts >= MAX_CONSECUTIVE_TIMEOUTS:
                    print(
                        colored(
                            f"⚠️ Maximum consecutive timeouts reached: {consecutive_timeouts} ⚠️",
                            "magenta",
                        )
                    )
                    consecutive_timeouts = 0
                    if ghci is not None:
                        print(colored("Restarting GHCi...", "magenta"))
                        try:
                            ghci.close(force=True)
                        except pexpect.exceptions.ExceptionPexpect as e:
                            print(colored(f"Error closing GHCi: {e}", "yellow"))
                            print("Attempting to create a new GHCi process anyway...")
                        try:
                            ghci = create_ghci_process()
                        except Exception as e:
                            print(
                                colored(f"Error creating new GHCi process: {e}", "red")
                            )
                            print("Continuing with the existing GHCi process...")
                elif "Test timed out" in test_output:
                    print(colored("❌ Timeout while running tests.", "red"))
                    print(f"Test output: {test_output}")
                    feedback = (
                        "Your code caused a timeout during testing. "
                        "Please review your implementation."
                    )
                feedback = (
                    "Your invert function is incorrect. "
                    "Here is the test output:\n" + test_output
                )
                print(colored("Assistant's code:", "cyan"))
                print(colored(invert_code, "yellow"))

    return feedback, success, ghci


if __name__ == "__main__":
    main()
