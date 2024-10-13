from openai import OpenAI
from openai._types import NOT_GIVEN
from tenacity import retry, stop_after_attempt, wait_random_exponential
from dotenv import load_dotenv, find_dotenv
import os
import json
import weave

load_dotenv(find_dotenv())

client = OpenAI(
    api_key=os.getenv("OPENAI_API_KEY"),
    organization=os.getenv("OPENAI_ORGANIZATION"),
    project=os.getenv("OPENAI_PROJECT"),
)

MAX_ROUNDS = 5
GENERATOR_MODEL_NAME = "gpt-4o-mini"
VERIFIER_MODEL_NAME = "gpt-4o-mini"
QUICKCHECK_TEST_FILE = "../test/Spec.hs"
HASKELL_PROMPT_FILE = "../haskell_prompt.md"
LIB_FILE = "../src/Lib.hs"
NUM_INITIAL_SOLUTIONS = 4

LIB_FILE_TEMPLATE = "../src/Lib_template.hs"
SPEC_FILE_TEMPLATE = "../test/Spec_template.hs"

@weave.op()
@retry(wait=wait_random_exponential(multiplier=1, max=40), stop=stop_after_attempt(5))
def chat_completion_request(messages, functions=NOT_GIVEN, function_call=NOT_GIVEN, model=GENERATOR_MODEL_NAME, temperature=0, n=1):
    try:
        response = client.chat.completions.create(
            model=model,
            messages=messages,
            functions=functions,
            function_call=function_call,
            temperature=temperature,
            n=n,
        )
        return response
    except Exception as e:
        print("🚫 Unable to generate ChatCompletion response")
        print(f"Exception: {e}")
        raise e

def execute_function_call(message, function_name):
    if not message.tool_calls:
        return None
    
    for tool_call in message.tool_calls:
        if tool_call.function.name == function_name:
            try:
                return json.loads(tool_call.function.arguments)
            except json.JSONDecodeError:
                raise ValueError(f"Invalid JSON in function arguments: {tool_call.function.arguments}")
    return None