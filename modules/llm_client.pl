:- module(llm_client, [llm_query/2, llm_query/3, llm_query_with_history/2]).

:- use_module(library(http/http_client)).
:- use_module(library(http/json)).
:- use_module(library(lists)).
:- use_module(library(process)).
:- use_module(library(filesex)).
:- use_module(library(files)).
:- use_module(dotenv).
:- use_module('../kb/config').

% Dynamic predicates for storing conversation history
:- dynamic chat_history/2.  % chat_history(Timestamp, Messages)
:- dynamic current_conversation/1.  % current_conversation(Messages)

% Export public predicates
:- export(llm_query/2).
:- export(llm_query/3).
:- export(llm_query_with_history/2).
:- export(get_llm_provider/1).
:- export(get_llm_model/1).
:- export(get_llm_endpoint/1).
:- export(get_llm_provider/1).
:- export(get_llm_model/1).
:- export(get_llm_endpoint/1).
:- discontiguous llm_client:llm_query_ollama_messages/2.
% Load .env on module load

% Helper predicates for JSON conversion
messages_to_json(Messages, JsonArray) :-
    maplist(message_to_json, Messages, JsonTerms),
    atomic_list_concat(JsonTerms, ", ", JsonContent),
    format(atom(JsonArray), '[~w]', [JsonContent]).

message_to_json(json([role=Role, content=Content]), JsonStr) :-
    format(atom(JsonStr), '{"role": "~w", "content": "~w"}', [Role, Content]).


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
llm_query(Prompt, System, Response) :-
    get_llm_provider(Provider),
    ( Provider = anthropic ->
        llm_query_anthropic(Prompt, System, Response)
    ; Provider = ollama ->
        llm_query_ollama(Prompt, System, Response)
    ; Provider = openai ->
        llm_query_openai(Prompt, System, Response)
    ; throw(error(unknown_provider, Provider))
    ).

% LLM query with conversation history
llm_query_with_history(Prompt, Response) :-
    default_system_prompt(System),
    
    % Add user message to current conversation
    get_time(_Stamp),
    ( current_conversation(Messages) ->
        retractall(current_conversation(_)),
        append(Messages, [user-Prompt], NewMessages)
    ; NewMessages = [user-Prompt]
    ),
    asserta(current_conversation(NewMessages)),
    
    % Build conversation messages for LLM
    build_conversation_from_history(NewMessages, Prompt, System, ConvMessages),
    
    % Get LLM response
    call_llm_with_messages(ConvMessages, Response),
    
    % Add assistant response to conversation
    append(NewMessages, [assistant-Response], FinalMessages),
    retractall(current_conversation(_)),
    asserta(current_conversation(FinalMessages)).

% Build conversation messages from history
build_conversation_from_history(Messages, _Prompt, System, ConvMessages) :-
    % Convert Prolog conversation format (user-Content, assistant-Response) 
    % to JSON message format
    prolog_messages_to_json(Messages, UserMessages),
    append([json([role=system, content=System])], UserMessages, ConvMessages).

% Convert Prolog message pairs to JSON format
prolog_messages_to_json([], []).
prolog_messages_to_json([user-Content|Rest], [json([role=user, content=Content])|JsonRest]) :-
    prolog_messages_to_json(Rest, JsonRest).
prolog_messages_to_json([assistant-Content|Rest], [json([role=assistant, content=Content])|JsonRest]) :-
    prolog_messages_to_json(Rest, JsonRest).

% Call LLM with message list
call_llm_with_messages(Messages, Response) :-
    get_llm_provider(Provider),
    ( Provider = ollama ->
        llm_query_ollama_messages(Messages, Response)
    ; Provider = anthropic ->
        llm_query_anthropic_messages(Messages, Response)
    ; Provider = openai ->
        llm_query_openai_messages(Messages, Response)
    ; throw(error(unsupported_provider, Provider))
    ).

% Ollama implementation with message list
llm_query_ollama_messages(Messages, Response) :-
    get_llm_model(Model),
    get_llm_endpoint(Endpoint),
    
    % Convert message list to JSON array
    messages_to_json(Messages, MessagesJson),
    format(atom(RequestStr), '{"model": "~w", "messages": ~w, "stream": false}', [Model, MessagesJson]),
    
    format(atom(Cmd), 'curl -s -X POST ~w -H "Content-Type: application/json" -d \'~w\'', [Endpoint, RequestStr]),
    
    catch(
        process_create('/bin/sh', ['-c', Cmd], [stdout(pipe(Out))]),
        Error,
        (format('Failed to create process: ~w~n', [Error]), fail)
    ),
    
    read_string(Out, _, ResponseStr),
    close(Out),
    
    atom_json_dict(ResponseStr, ResponseDict, []),
    get_dict(message, ResponseDict, Message),
    get_dict(content, Message, Response).

% Anthropic implementation with message list
llm_query_anthropic_messages(Messages, Response) :-
    get_api_key(Key),
    get_llm_model(Model),
    atom_string(Key, KeyString),

    % Build request with message list
    Request = json([
        model=Model,
        max_tokens=1024,
        messages=Messages,
        stream=false
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

% OpenAI implementation with message list (placeholder)
llm_query_openai_messages(_Messages, _Response) :-
    throw(error(not_implemented, 'OpenAI implementation not yet updated')).

% Call LLM with message list
call_llm_with_messages(Messages, Response) :-
    get_llm_provider(Provider),
    ( Provider = ollama ->
        llm_query_ollama_messages(Messages, Response)
    ; Provider = anthropic ->
        llm_query_anthropic_messages(Messages, Response)
    ; Provider = openai ->
        llm_query_openai_messages(Messages, Response)
    ; throw(error(unsupported_provider, Provider))
    ).

% Ollama implementation with message list
llm_query_ollama_messages(Messages, Response) :-
    get_llm_model(Model),
    get_llm_endpoint(Endpoint),
    
    % Convert message list to JSON array
    messages_to_json(Messages, MessagesJson),
    format(atom(RequestStr), '{"model": "~w", "messages": ~w, "stream": false}', [Model, MessagesJson]),
    
    format(atom(Cmd), 'curl -s -X POST ~w -H "Content-Type: application/json" -d \'~w\'', [Endpoint, RequestStr]),
    
    catch(
        process_create('/bin/sh', ['-c', Cmd], [stdout(pipe(Out))]),
        Error,
        (format('Failed to create process: ~w~n', [Error]), fail)
    ),
    
    read_string(Out, _, ResponseStr),
    close(Out),
    
    atom_json_dict(ResponseStr, ResponseDict, []),
    get_dict(message, ResponseDict, Message),
    get_dict(content, Message, Response).

% Anthropic implementation with message list
llm_query_anthropic_messages(Messages, Response) :-
    get_api_key(Key),
    get_llm_model(Model),
    atom_string(Key, KeyString),

    % Build request with message list
    Request = json([
        model=Model,
        max_tokens=1024,
        messages=Messages,
        stream=false
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

% OpenAI implementation with message list (placeholder)
llm_query_openai_messages(_Messages, _Response) :-
    throw(error(not_implemented, 'OpenAI implementation not yet updated')).

% ============================================================
% Chat History Management
% ============================================================



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

% Ollama implementation with prompt and system
llm_query_ollama(Prompt, SystemPrompt, Response) :-
    Messages = [
        json([role=system, content=SystemPrompt]),
        json([role=user, content=Prompt])
    ],
    llm_query_ollama_messages(Messages, Response).
