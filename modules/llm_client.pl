:- module(llm_client, [llm_query/2, llm_query/3]).

:- use_module(library(http/http_client)).
:- use_module(library(http/json)).
:- use_module(dotenv).

% Load .env on module load
:- load_env.

% Get API key from environment
get_api_key(Key) :-
    getenv('ANTHROPIC_API_KEY', Key),
    !.
get_api_key(_) :-
    throw(error(missing_api_key, 'ANTHROPIC_API_KEY not set in environment')).

% Default system prompt
default_system_prompt("You are Zarathustra, a wise and concise assistant. Be helpful, direct, and philosophical when appropriate.").

% LLM query with default system prompt
llm_query(Prompt, Response) :-
    default_system_prompt(System),
    llm_query(Prompt, System, Response).

% LLM query with custom system prompt
llm_query(Prompt, SystemPrompt, Response) :-
    get_api_key(Key),
    atom_string(Key, KeyString),

    % Build request with system prompt
    Request = json([
        model="claude-sonnet-4-20250514",
        max_tokens=1024,
        system=SystemPrompt,
        messages=[json([role=user, content=Prompt])]
    ]),

    % Make API call
    http_post(
        'https://api.anthropic.com/v1/messages',
        json(Request),
        Reply,
        [
            request_header('x-api-key'=KeyString),
            request_header('anthropic-version'='2023-06-01'),
            json_object(dict)
        ]
    ),

    % Extract response
    get_dict(content, Reply, [First|_]),
    get_dict(text, First, Response).
