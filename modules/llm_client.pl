:- module(llm_client, [llm_query/2, llm_query/3]).

:- use_module(library(http/http_client)).
:- use_module(library(http/json)).
:- use_module(library(lists)).
:- use_module(library(process)).
:- use_module(dotenv).
:- use_module('../kb/config').

% Load .env on module load
:- load_env.

% Get LLM provider configuration
get_llm_provider(Provider) :-
    kb_config:llm_provider(Provider), !.
get_llm_provider(anthropic) :-
    % Default fallback
    format(user_error, 'Warning: llm_provider not configured, defaulting to anthropic~n', []).

get_llm_model(Model) :-
    kb_config:llm_model(Model), !.
get_llm_model("claude-sonnet-4-20250514") :-
    % Default fallback for Anthropic
    format(user_error, 'Warning: llm_model not configured, using default~n', []).

get_llm_endpoint(Endpoint) :-
    kb_config:llm_endpoint(Endpoint), !.
get_llm_endpoint(Endpoint) :-
    % Provide defaults based on provider
    get_llm_provider(Provider),
    ( Provider = ollama ->
        Endpoint = "http://localhost:11434/api/chat"
    ; Provider = openai ->
        Endpoint = "https://api.openai.com/v1/chat/completions"
    ; Provider = anthropic ->
        throw(error(no_endpoint_needed, 'Anthropic uses SDK'))
    ).

% Get API key from environment (Anthropic/OpenAI)
get_api_key(Key) :-
    get_llm_provider(Provider),
    ( Provider = anthropic ->
        getenv('ANTHROPIC_API_KEY', Key)
    ; Provider = openai ->
        getenv('OPENAI_API_KEY', Key)
    ; throw(error(no_api_key_needed, 'Ollama does not require API key'))
    ),
    !.
get_api_key(_) :-
    throw(error(missing_api_key, 'API key not set in environment')).

% Default system prompt
default_system_prompt("You are Zarathustra, a wise and concise assistant. Be helpful, direct, and philosophical when appropriate.").

% LLM query with default system prompt
llm_query(Prompt, Response) :-
    default_system_prompt(System),
    llm_query(Prompt, System, Response).

% LLM query with custom system prompt
llm_query(Prompt, SystemPrompt, Response) :-
    get_llm_provider(Provider),
    ( Provider = ollama ->
        llm_query_ollama(Prompt, SystemPrompt, Response)
    ; Provider = anthropic ->
        llm_query_anthropic(Prompt, SystemPrompt, Response)
    ; Provider = openai ->
        llm_query_openai(Prompt, SystemPrompt, Response)
    ; throw(error(unsupported_provider, Provider))
    ).

% Ollama implementation using curl with process_create
llm_query_ollama(Prompt, SystemPrompt, Response) :-
    get_llm_model(Model),
    get_llm_endpoint(Endpoint),
    
    % Build request JSON as string
    format(atom(RequestStr), '{"model": "~w", "messages": [{"role": "system", "content": "~w"}, {"role": "user", "content": "~w"}], "stream": false}', 
           [Model, SystemPrompt, Prompt]),
    
    % Use process_create to call curl and capture output
    format(atom(Cmd), 'curl -s -X POST ~w -H "Content-Type: application/json" -d \'~w\'', [Endpoint, RequestStr]),
    
    catch(
        process_create('/bin/sh', ['-c', Cmd], [stdout(pipe(Out))]),
        Error,
        (format('Failed to create process: ~w~n', [Error]), fail)
    ),
    
    read_string(Out, _, ResponseStr),
    close(Out),
    
    % Parse JSON response and extract content
    atom_json_dict(ResponseStr, ResponseDict, []),
    get_dict(message, ResponseDict, Message),
    get_dict(content, Message, Response).

% Anthropic implementation
llm_query_anthropic(Prompt, SystemPrompt, Response) :-
    get_api_key(Key),
    get_llm_model(Model),
    atom_string(Key, KeyString),

    % Build request with system prompt
    Request = json([
        model=Model,
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

% OpenAI implementation
llm_query_openai(Prompt, SystemPrompt, Response) :-
    get_api_key(Key),
    get_llm_model(Model),
    atom_string(Key, KeyString),
    get_llm_endpoint(Endpoint),

    % OpenAI uses messages format
    Request = json([
        model=Model,
        messages=[
            json([role=system, content=SystemPrompt]),
            json([role=user, content=Prompt])
        ],
        max_tokens=1024,
        temperature=0.7
    ]),

    http_post(
        Endpoint,
        json(Request),
        Reply,
        [
            request_header('Authorization'=("Bearer " + KeyString)),
            request_header('Content-Type'='application/json'),
            json_object(dict)
        ]
    ),

    get_dict(choices, Reply, [First|_]),
    get_dict(message, First, Message),
    get_dict(content, Message, Response).
