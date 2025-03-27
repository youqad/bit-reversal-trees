from openai import OpenAI
from openai._types import NOT_GIVEN
from anthropic import Anthropic
from tenacity import retry, stop_after_attempt, wait_random_exponential
from dotenv import load_dotenv, find_dotenv
import os
import json
import litellm
import weave
import platform
import pync
from datetime import datetime
from typing import Optional, Dict, Any
import sys
import pexpect

litellm.drop_params = True
litellm.modify_params = True
# litellm.set_verbose=True

load_dotenv(find_dotenv(), override=True)

openai_client = OpenAI(
    api_key=os.getenv("OPENAI_API_KEY"),
    organization=os.getenv("OPENAI_ORGANIZATION"),
    project=os.getenv("OPENAI_PROJECT"),
)

anthropic_client = Anthropic(api_key=os.getenv("ANTHROPIC_API_KEY"))

MAX_ROUNDS = int(os.getenv("MAX_ROUNDS", 5))
NUM_INITIAL_SOLUTIONS = int(os.getenv("NUM_INITIAL_SOLUTIONS", 4))
GENERATOR_MODEL_NAME = os.getenv("GENERATOR_MODEL_NAME", "gpt-4o")
VERIFIER_MODEL_NAME = os.getenv("VERIFIER_MODEL_NAME", "gpt-4o-mini")
QUICKCHECK_TEST_FILE = "../test/Spec.hs"
HASKELL_PROMPT_FILE = "../haskell_prompt.md"
PYTHON_PROMPT_FILE = "../python_prompt.md"
LIB_FILE = "../src/Lib.hs"
PROGRAM_SYNTHESIS_LANGUAGE = os.getenv("PROGRAM_SYNTHESIS_LANGUAGE", "haskell")
MAX_CONSECUTIVE_TIMEOUTS = int(os.getenv("MAX_CONSECUTIVE_TIMEOUTS", 2))

@weave.op()
@retry(wait=wait_random_exponential(multiplier=1, max=40), stop=stop_after_attempt(5))
def chat_completion_request(messages, functions=None, function_call=None, model=GENERATOR_MODEL_NAME, temperature=0, n=1):
    try:
        if functions is not None and function_call is not None:
            response = litellm.completion(
                model=model,
                messages=messages,
                functions=functions,
                function_call=function_call,
                temperature=temperature,
                n=n
            )
        else:
            response = litellm.completion(
                model=model,
                messages=messages,
                temperature=temperature,
                n=n
            )
        return response
    except Exception as e:
        print("🚫 Unable to generate ChatCompletion response")
        print(f"Exception: {e}")
        raise e


def get_call_dict(call, response_choice, weave_client):
    project_id = getattr(call, "project_id", None)

    def serialize_value(value: Any) -> Any:
        if isinstance(value, datetime):
            return value.isoformat()
        if isinstance(value, dict):
            return {k: serialize_value(v) for k, v in value.items()}
        return weave.to_json(value, project_id, weave_client)

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
        "started_at": serialize_value(getattr(call, "started_at", None)),
        "attributes": serialize_value(getattr(call, "attributes", {})),
        "inputs": serialize_value(getattr(call, "inputs", {})),
        "ended_at": serialize_value(getattr(call, "ended_at", None)),
        "exception": getattr(call, "exception", None),
        "output": serialized_output,
        "summary": serialize_value(getattr(call, "summary", None)),
        "wb_user_id": getattr(call, "wb_user_id", None),
        "wb_run_id": getattr(call, "wb_run_id", None),
        "deleted_at": serialize_value(getattr(call, "deleted_at", None)),
        "response_choice": response_choice.model_dump(),
    }
    return call_dict

def execute_function_call(message, function_name):
    if not message.function_call:
        return None
    
    if message.function_call.name == function_name:
        try:
            return json.loads(message.function_call.arguments)
        except json.JSONDecodeError:
            raise ValueError(f"Invalid JSON in function arguments: {message.function_call.arguments}")
    return None


def get_verifier_schemas_and_messages(assistant_content, program_synthesis_language=PROGRAM_SYNTHESIS_LANGUAGE):
    if program_synthesis_language == "haskell":
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
        verifier_messages = [
            {
                "role": "system",
                "content": "You are a Haskell code verifier. Extract the `invert :: Tree a -> Tree a` function from the user's message (and ONLY this function), and check if it satisfies the syntactic requirements.",
            },
            {
                "role": "user",
                "content": assistant_content,
            },
        ]
    elif program_synthesis_language == "python":
        verifier_function_schemas = [
            {
            "name": "extract_invert_function",
            "description": "Extract the `invert(tree: Tree) -> Tree` function (and ONLY this function) and verify if it satisfies the syntactic requirements.",
            "parameters": {
                "type": "object",
                "properties": {
                    "invert_function_code": {
                        "type": "string",
                        "description": "The Python code for the `invert(tree: Tree) -> Tree` function, and ONLY this function.",
                    },
                    "satisfies_requirements": {
                        "type": "boolean",
                        "description": "Whether the proposed `invert(tree: Tree) -> Tree` function satisfies the syntactic requirements, i.e.:\n1. The `invert(tree: Tree) -> Tree` function must be a standalone, pure, and recursive function.\n2. The `invert(tree: Tree) -> Tree` function can either rely on no helper function at all, or rely on one single helper inner function that takes an extra boolean argument as input (i.e. an inner helper function of type `(flag: bool, tree: Tree) -> Tree`), if needed.\n3. The `invert(tree: Tree) -> Tree` function only uses recursion (no loops).\n4. The `invert(tree: Tree) -> Tree` function maintains purity (no side effects or mutability).",
                    },
                },
                    "required": ["invert_function_code", "satisfies_requirements"],
                },
            },
        ]
        verifier_messages = [
            {
                "role": "system",
                "content": "You are a Python code verifier. Extract the `invert(tree: Tree) -> Tree` function from the user's message (and ONLY this function), and check if it satisfies the syntactic requirements.",
            },
            {
                "role": "user",
                "content": assistant_content,
            },
        ]
    else:
        raise ValueError(f"Unsupported program synthesis language: {program_synthesis_language}")
    return verifier_function_schemas, verifier_messages


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


def extract_failed_info(output, program_synthesis_language=PROGRAM_SYNTHESIS_LANGUAGE):
    """
    Extract the relevant failure information from the test output.
    """
    if program_synthesis_language == "haskell":
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
            return failed_info
        else:
            return "Failed to extract detailed failure information."
    elif program_synthesis_language == "python":
        pass
    else:
        raise ValueError(f"Unsupported program synthesis language: {program_synthesis_language}")


def send_desktop_notification(message):
    if platform.system() == "Darwin":
        pync.notify(message, title="Haskell Challenge Solver")
    # elif platform.system() == "Linux":
    #     notify2.init("Haskell Challenge Solver")
    #     notification = notify2.Notification("Haskell Challenge Solver", message)
    #     notification.show()
    else:
        print("Desktop notifications are not supported on this platform.")