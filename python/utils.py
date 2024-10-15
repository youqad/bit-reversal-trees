from openai import OpenAI
from openai._types import NOT_GIVEN
from anthropic import Anthropic
from tenacity import retry, stop_after_attempt, wait_random_exponential
from dotenv import load_dotenv, find_dotenv
import os
import json
import litellm
import weave
litellm.drop_params = True
litellm.modify_params = True

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
LIB_FILE = "../src/Lib.hs"

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

def execute_function_call(message, function_name):
    if not message.function_call:
        return None
    
    if message.function_call.name == function_name:
        try:
            return json.loads(message.function_call.arguments)
        except json.JSONDecodeError:
            raise ValueError(f"Invalid JSON in function arguments: {message.function_call.arguments}")
    return None