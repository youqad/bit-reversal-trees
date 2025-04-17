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
from weave.trace.serialization.serialize import to_json
from types import SimpleNamespace
from termcolor import colored
import unittest  # Add this import

litellm.drop_params = True
litellm.modify_params = True
# litellm.set_verbose=True

load_dotenv(find_dotenv(), override=True)

# openai_client = OpenAI(
#     api_key=os.getenv("OPENAI_API_KEY"),
#     organization=os.getenv("OPENAI_ORGANIZATION"),
#     project=os.getenv("OPENAI_PROJECT"),
# )

# anthropic_client = Anthropic(api_key=os.getenv("ANTHROPIC_API_KEY"))

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
def chat_completion_request(
    messages,
    functions=None,
    function_call=None,
    model=GENERATOR_MODEL_NAME,
    temperature=0,
    n=1,
):
    if model == "o1-pro":
        # special handling for o1-pro using litellm.responses
        if not messages:
            raise ValueError("Messages list cannot be empty for o1-pro")
        input_text = messages[-1].get("content", "")
        if not input_text:
            raise ValueError("Last message content is empty or missing for o1-pro")
        # we are assuming n=1 for o1-pro based on the response structure.
        if n > 1:
            print(
                colored(
                    f"Warning: 'n={n}' requested for 'o1-pro', but litellm.responses might only return one response. Proceeding with n=1 logic.",
                    "yellow",
                )
            )

        try:
            api_response = litellm.responses(
                model=model,
                input=input_text,
                temperature=temperature,  # Pass temperature if supported
            )

            # Response Normalization:
            # extract content from the specific structure of ResponsesAPIResponse
            try:
                # path: response.output[1].content[0].text
                content_text = api_response.output[1].content[0].text
            except (IndexError, AttributeError, TypeError) as e:
                print(
                    colored(
                        f"Error extracting content from o1-pro response structure: {e}",
                        "red",
                    )
                )
                print(f"Raw o1-pro response: {api_response}")
                raise ValueError(
                    "Could not parse expected content from o1-pro response."
                ) from e
            mock_message = SimpleNamespace(
                content=content_text,
                role="assistant",
                function_call=None,
                tool_calls=None,
                annotations=[],
            )
            mock_choice = SimpleNamespace(
                finish_reason="stop", index=0, logprobs=None, message=mock_message
            )
            o1_usage = getattr(api_response, "usage", None)
            mock_usage = SimpleNamespace(
                completion_tokens=getattr(o1_usage, "output_tokens", 0)
                if o1_usage
                else 0,
                prompt_tokens=getattr(o1_usage, "input_tokens", 0) if o1_usage else 0,
                total_tokens=getattr(o1_usage, "total_tokens", 0) if o1_usage else 0,
                completion_tokens_details=None,
                prompt_tokens_details=None,
            )
            normalized_response = SimpleNamespace(
                id=getattr(api_response, "id", "unknown"),
                choices=[mock_choice],
                created=int(getattr(api_response, "created_at", 0)),
                model=getattr(api_response, "model", model),
                object="chat.completion",  # Mimic OpenAI object type
                system_fingerprint=None,
                usage=mock_usage,
                service_tier=None,
            )
            return normalized_response
        except Exception as e:
            print(f"🚫 Unable to generate response using litellm.responses for {model}")
            print(f"Exception: {e}")
            raise e
    else:
        try:
            if model == "o4-mini":
                temperature = 1
            if functions is not None and function_call is not None:
                response = litellm.completion(
                    model=model,
                    messages=messages,
                    functions=functions,
                    function_call=function_call,
                    temperature=temperature,
                    n=n,
                )
            else:
                response = litellm.completion(
                    model=model, messages=messages, temperature=temperature, n=n
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
            raise ValueError(
                f"Invalid JSON in function arguments: {message.function_call.arguments}"
            )
    return None


def get_verifier_schemas_and_messages(
    assistant_content, program_synthesis_language=PROGRAM_SYNTHESIS_LANGUAGE
):
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
        raise ValueError(
            f"Unsupported program synthesis language: {program_synthesis_language}"
        )
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
    For Python, 'output' is expected to be the unittest.TestResult object.
    For Haskell, 'output' is the raw string output from GHCi.
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
        elif start_index:  # Handle case where failure is at the end
            failed_info = "\n".join(lines[start_index:])
            return failed_info
        else:
            # attempt to find generic failure markers if "Failed:" isn't present
            failure_markers = ["Failures:", "Exception:", "Error:"]
            for i, line in enumerate(lines):
                if any(marker in line for marker in failure_markers):
                    # snippet starting from the marker
                    return "\n".join(
                        lines[i : min(i + 10, len(lines))]
                    )  # marker line + next few lines

            # if no specific markers found, return a generic message
            return (
                "Failed to extract detailed failure information. Raw output snippet:\n"
                + output[:500]
            )

    elif program_synthesis_language == "python":
        # expect 'output' to be the unittest.TestResult object
        if not isinstance(output, unittest.TestResult):
            return "Error: Expected a unittest.TestResult object for Python failure extraction."

        failures_report = []

        # AssertionErrors
        for test_case, traceback_str in output.failures:
            assertion_error_marker = "AssertionError: "
            assertion_msg_start = traceback_str.find(assertion_error_marker)

            if assertion_msg_start != -1:
                # message part after "AssertionError: "
                error_details = traceback_str[
                    assertion_msg_start + len(assertion_error_marker) :
                ]

                # look for custom message marker as seen in unittest output
                custom_msg_marker = " : Failed:\n"
                custom_msg_start = error_details.find(custom_msg_marker)

                if custom_msg_start != -1:
                    # custom message part starting after the marker
                    failure_summary = error_details[
                        custom_msg_start + len(custom_msg_marker) :
                    ].strip()
                    # prepend "AssertionError: Failed:" for context
                    failures_report.append(
                        f"Failure in {test_case}:\nAssertionError: Failed:\n{failure_summary}"
                    )
                else:
                    # If custom marker not found, use the standard unittest diff message
                    # Take the first few lines of the error details as the summary
                    diff_summary = "\n".join(
                        error_details.split("\n")[:5]
                    )  # Limit lines for brevity
                    failures_report.append(
                        f"Failure in {test_case}:\nAssertionError: {diff_summary}"
                    )
            else:
                # if AssertionError marker isn't found
                failures_report.append(f"Failure in {test_case}:\n{traceback_str}")

        # other Exceptions
        for test_case, traceback_str in output.errors:
            #  concise summary for errors
            lines = traceback_str.strip().split("\n")
            if len(lines) >= 2:
                # often the last two lines give the error type and location/context
                error_summary = "\n".join(lines[-2:])
            elif len(lines) == 1:
                error_summary = lines[0]
            else:
                error_summary = traceback_str
            failures_report.append(f"Error in {test_case}:\n{error_summary}")

        # Join reports with a clear separator
        return "\n---\n".join(failures_report)
    else:
        raise ValueError(
            f"Unsupported program synthesis language: {program_synthesis_language}"
        )


def send_desktop_notification(message):
    if platform.system() == "Darwin":
        pync.notify(message, title="Haskell Challenge Solver")
    # elif platform.system() == "Linux":
    #     notify2.init("Haskell Challenge Solver")
    #     notification = notify2.Notification("Haskell Challenge Solver", message)
    #     notification.show()
    else:
        print("Desktop notifications are not supported on this platform.")