import os
import json
import subprocess
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
from weave.trace.serialize import to_json

warnings.filterwarnings("ignore", category=DeprecationWarning, module="wandb")
warnings.filterwarnings("ignore", category=DeprecationWarning, module="weave")

load_dotenv(find_dotenv())

weave_client = weave.init("bit-reversal-trees")

verifier_function_schemas = [
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
                    "description": "Whether the proposed `invert :: Tree a -> Tree a` function satisfies the syntactic requirements, i.e.:\n1. The `invert :: Tree a -> Tree a` function must be a standalone, pure, and recursive function.\n2. The `invert :: Tree a -> Tree a` function can either rely on no helper function at all, or rely on one single helper inner function that takes an extra boolean argument as input (i.e. an inner helper function of type `Bool -> Tree a -> Tree a`), if needed.\n3. The `invert :: Tree a -> Tree a` function only uses recursion (no loops).\n4. The `invert :: Tree a -> Tree a` function maintains purity (no side effects or mutability).",
                },
            },
            "required": ["invert_function_code", "satisfies_requirements"],
        },
    },
]

MAX_CONSECUTIVE_TIMEOUTS = 2
consecutive_timeouts = 0


def create_ghci_process():
    print("Starting GHCi process...")
    # Set logfile to sys.stdout to print GHCi output to the console
    ghci = pexpect.spawn(
        "stack ghci --ghci-options=-ignore-dot-ghci ../test/DynamicSpec.hs",
        encoding="utf-8",
        logfile=sys.stdout,
    )
    ghci.expect("GHCi, version.*", timeout=60)
    print("GHCi started. Setting up environment...")
    ghci.sendline(":set -ignore-dot-ghci")
    ghci.sendline(":set -XOverloadedStrings")
    ghci.sendline(":set -XScopedTypeVariables")
    ghci.sendline(":set -XTemplateHaskell")
    ghci.sendline(":set +m")
    ghci.sendline(':set prompt "ghci_prompt> "')
    ghci.expect_exact("ghci_prompt> ", timeout=30)

    print("Verifying testInvert availability...")
    ghci.sendline(":t testInvert")
    index = ghci.expect(["testInvert ::", "Not in scope"], timeout=30)
    if index == 1:
        raise Exception("testInvert function is not available in the GHCi environment")

    ghci.expect_exact("ghci_prompt> ", timeout=30)
    print("GHCi process ready.")
    return ghci


def run_tests(invert_code, ghci):
    global consecutive_timeouts
    """
    Run Hspec tests and return the result by sending the invert code directly into GHCi.
    """
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
            return False, extract_failed_info(output)
        else:
            print(colored("❌ Unknown test result.", "red"))
            consecutive_timeouts = 0
            return False, output

    except pexpect.TIMEOUT:
        print(colored("❌ Timeout while running tests.", "red"))
        print(f"Last output: {ghci.before}")
        consecutive_timeouts += 1
        return False, "Timeout while running tests"
    except Exception as e:
        print(colored(f"❌ Error running tests: {str(e)}", "red"))
        print(f"Last output: {ghci.before}")
        consecutive_timeouts = 0
        return False, str(e)


def extract_failed_info(output):
    """
    Extract the relevant failure information from the test output.
    """
    lines = output.split("\n")
    start_index = None
    end_index = None

    for i, line in enumerate(lines):
        if line.strip().startswith("Failed:"):
            start_index = i
        elif start_index and line.strip() == "":
            end_index = i
            break

    if start_index and end_index:
        failed_info = "\n".join(lines[start_index:end_index])
        return failed_info.replace("Inverted flattened:", "Your inverted flattened:")
    else:
        return "Failed to extract detailed failure information."

def get_call_dict(call, response_choice):
    global weave_client
    project_id = getattr(call, "project_id", None)

    def serialize_value(value):
        return to_json(value, project_id, weave_client)

    # output_class = getattr(call, "output")
    # output_dict = output_class.__dict__
    # choices = output_dict.get("choices", [])

    # choices = response.choices
    # choice = choices[choice_index]
    # message_dict = serialize_value(getattr(choice, "message", {}))
    # serialized_choice = {
    #     "finish_reason": getattr(choice, "finish_reason", None),
    #     "index": getattr(choice, "index", None),
    #     "message": {
    #         "content": message_dict.get("content", None),
    #         "role": message_dict.get("role", None),
    #         "tool_calls": message_dict.get("tool_calls", None),
    #         "function_call": message_dict.get("function_call", None),
    #     }
    # }
    # choices.append(serialized_choice)

    serialized_output = serialize_value(getattr(call, "output", {}))
    # serialized_output["choices"] = choices
    
    call_dict = {
        "id": getattr(call, "id", None),
        "project_id": project_id,
        "op_name": getattr(call, "op_name", None),
        "display_name": getattr(call, "display_name", None),
        "trace_id": getattr(call, "trace_id", None),
        "parent_id": getattr(call, "parent_id", None),
        "started_at": getattr(call, "started_at", None),
        "attributes": serialize_value(getattr(call, "attributes", {})),
        "inputs": serialize_value(getattr(call, "inputs", {})),
        "ended_at": getattr(call, "ended_at", None),
        "exception": serialize_value(getattr(call, "exception", None)),
        "output": serialized_output,
        "summary": serialize_value(getattr(call, "summary", None)),
        "wb_user_id": getattr(call, "wb_user_id", None),
        "wb_run_id": getattr(call, "wb_run_id", None),
        "deleted_at": getattr(call, "deleted_at", None),
        "response_choice": response_choice.model_dump(),
    }
    return call_dict


def main():
    with open(HASKELL_PROMPT_FILE, "r") as f:
        initial_prompt = f.read()

    initial_role = "user" if GENERATOR_MODEL_NAME.startswith("o1") else "system"

    initial_message = {
        "role": initial_role,
        "content": initial_prompt,
    }

    conversations = []

    if GENERATOR_MODEL_NAME.startswith("o1") or GENERATOR_MODEL_NAME.startswith(
        "claude"
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

    ghci = create_ghci_process()

    solutions = []
    with tqdm(total=len(conversations), desc="Processing conversations") as pbar:
        for conv in conversations:
            full_conv_or_solution, success, ghci = process_conversation(conv, ghci)
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
    try:
        ghci.sendline(":q")
        ghci.expect(pexpect.EOF, timeout=5)
    except pexpect.TIMEOUT:
        print(
            colored("GHCi didn't terminate as expected. Forcing termination.", "yellow")
        )
    finally:
        ghci.close(force=True)


def process_conversation(conversation, ghci):
    """
    Process a single conversation, including multiple rounds of verification.
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
    feedback, success = verify_response(
        first_assistant_message["content"], ghci, call_ids_dict, conversation["call"], response_choice
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

        if consecutive_timeouts >= MAX_CONSECUTIVE_TIMEOUTS:
            print(
                colored(
                    f"⚠️ Reached {MAX_CONSECUTIVE_TIMEOUTS} consecutive timeouts. Restarting GHCi...",
                    "magenta",
                )
            )
            consecutive_timeouts = 0
            try:
                ghci.close(force=True)
            except pexpect.exceptions.ExceptionPexpect as e:
                print(colored(f"Error closing GHCi: {e}", "yellow"))
                print("Attempting to create a new GHCi process anyway...")
            try:
                ghci = create_ghci_process()
            except Exception as e:
                print(colored(f"Error creating new GHCi process: {e}", "red"))
                print("Continuing with the existing GHCi process...")

        response, call = chat_completion_request.call(
            messages, model=GENERATOR_MODEL_NAME, temperature=1
        )
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

        feedback, success = verify_response(
            assistant_content, ghci, call_ids_dict, call, response_choice
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
    call_dict = get_call_dict(call, choice)
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


def verify_response(assistant_content, ghci, call_ids_dict, call, response_choice):
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
            tests_passed, test_output = run_tests(invert_code, ghci)

            if tests_passed:
                print(colored("🎉 Successfully found a valid implementation!", "green"))
                print(colored("Assistant's code:", "cyan"))
                print(colored(invert_code, "yellow"))
                success = True
                feedback = invert_code
                call_dict = get_call_dict(call, response_choice)
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
                feedback = (
                    "Your invert function is incorrect. "
                    "Here is the test output:\n" + test_output
                )
                print(colored("Assistant's code:", "cyan"))
                print(colored(invert_code, "yellow"))

    return feedback, success


if __name__ == "__main__":
    main()
