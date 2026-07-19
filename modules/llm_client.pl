:- module(llm_client, [
    llm_query/2,
    llm_query/3,
    llm_query_result/3,
    llm_query_with_history/2,
    llm_query_with_history_result/2,
    serialize_llm_request/7,
    reset_llm_history/0,
    close_llm_client/0,
    get_llm_provider/1,
    get_llm_model/1,
    get_llm_endpoint/1
]).

:- use_module(library(http/http_client)).
:- use_module(library(http/http_open), [http_open/3, http_close_keep_alive/1]).
:- use_module(library(http/json)).
:- use_module(library(lists)).
:- use_module(library(time)).
:- use_module('../kb/config').

:- dynamic current_conversation/1.
:- dynamic llm_timeouts/3.
:- dynamic llm_retry_limit/1.

llm_timeouts(5.0, 20.0, 30.0).
llm_retry_limit(2).
history_limit(20).

get_llm_provider(Provider) :-
    kb_config:llm_provider(Provider), !.
get_llm_provider(anthropic) :-
    format(user_error, 'Warning: llm_provider not configured, defaulting to anthropic~n', []).

get_llm_model(Model) :-
    kb_config:llm_model(Model), !.
get_llm_model("claude-sonnet-4-20250514") :-
    format(user_error, 'Warning: llm_model not configured, using default~n', []).

get_llm_endpoint(Endpoint) :-
    kb_config:llm_endpoint(Endpoint), !.
get_llm_endpoint(Endpoint) :-
    get_llm_provider(Provider),
    default_endpoint(Provider, Endpoint).

default_endpoint(ollama, "http://localhost:11434/api/chat").
default_endpoint(openai, "https://api.openai.com/v1/chat/completions").
default_endpoint(anthropic, "https://api.anthropic.com/v1/messages").

provider_endpoint(anthropic, Endpoint) :-
    ( getenv('ZARA_ANTHROPIC_ENDPOINT', Value), Value \== ''
    -> atom_string(Value, Endpoint)
    ; default_endpoint(anthropic, Endpoint)
    ), !.
provider_endpoint(openai, Endpoint) :-
    get_llm_endpoint(Configured),
    default_endpoint(ollama, OllamaDefault),
    ( Configured == OllamaDefault
    -> default_endpoint(openai, Endpoint)
    ; Endpoint = Configured
    ), !.
provider_endpoint(_, Endpoint) :-
    get_llm_endpoint(Endpoint).

get_api_key(anthropic, Key) :-
    getenv('ANTHROPIC_API_KEY', Key), Key \== '', !.
get_api_key(openai, Key) :-
    getenv('OPENAI_API_KEY', Key), Key \== '', !.
get_api_key(Provider, _) :-
    throw(error(missing_api_key(Provider), _)).

default_system_prompt("You are Zarathustra, a wise and concise assistant. Be helpful, direct, and philosophical when appropriate.").

llm_query(Prompt, Response) :-
    default_system_prompt(System),
    llm_query(Prompt, System, Response).

llm_query(Prompt, System, Response) :-
    llm_query_result(Prompt, System, Result),
    unwrap_llm_result(Result, Response).

llm_query_result(Prompt, System, Result) :-
    Messages = [_{role:user, content:Prompt}],
    call_provider(System, Messages, Result).

llm_query_with_history(Prompt, Response) :-
    llm_query_with_history_result(Prompt, Result),
    unwrap_llm_result(Result, Response).

llm_query_with_history_result(Prompt, Result) :-
    default_system_prompt(System),
    ( current_conversation(History) -> true ; History = [] ),
    append(History, [user-Prompt], WithPrompt0),
    trim_history(WithPrompt0, WithPrompt),
    history_messages(WithPrompt, Messages),
    call_provider(System, Messages, Result),
    store_successful_turn(Result, WithPrompt).

store_successful_turn(llm_result(success, Response), WithPrompt) :-
    append(WithPrompt, [assistant-Response], Complete0),
    trim_history(Complete0, Complete),
    retractall(current_conversation(_)),
    asserta(current_conversation(Complete)), !.
store_successful_turn(_, _).

reset_llm_history :-
    retractall(current_conversation(_)).

trim_history(Messages, Trimmed) :-
    history_limit(Limit),
    length(Messages, Length),
    Drop is max(0, Length - Limit),
    length(Prefix, Drop),
    append(Prefix, Trimmed, Messages), !.

history_messages([], []).
history_messages([Role-Content|Rest], [_{role:Role, content:Content}|Messages]) :-
    memberchk(Role, [user, assistant]),
    history_messages(Rest, Messages).

call_provider(System, Messages, Result) :-
    get_llm_provider(Provider),
    get_llm_model(Model),
    provider_endpoint(Provider, Endpoint),
    provider_key(Provider, KeyResult),
    ( KeyResult = key(Key)
    -> serialize_llm_request(Provider, Model, Key, System, Messages, Headers, Request),
       request_provider(Provider, Endpoint, Headers, Request, Result)
    ; KeyResult = error(Error),
      Result = llm_result(error, Error)
    ).

provider_key(ollama, key("")) :- !.
provider_key(Provider, Result) :-
    catch(get_api_key(Provider, Key), Error, true),
    ( var(Error)
    -> text_string(Key, KeyString), Result = key(KeyString)
    ; Result = error(llm_error(authentication, "API key is not set", none))
    ).

serialize_llm_request(anthropic, Model, Key, System, Messages, Headers, Request) :-
    text_string(Key, KeyString),
    Headers = [
        'x-api-key'=KeyString,
        'anthropic-version'="2023-06-01"
    ],
    Request = _{
        model:Model,
        max_tokens:1024,
        system:System,
        messages:Messages
    }.
serialize_llm_request(openai, Model, Key, System, Messages, Headers, Request) :-
    text_string(Key, KeyString),
    format(string(Authorization), 'Bearer ~s', [KeyString]),
    Headers = ['Authorization'=Authorization],
    Request = _{
        model:Model,
        messages:[_{role:system, content:System}|Messages],
        max_tokens:1024
    }.
serialize_llm_request(ollama, Model, _, System, Messages, Headers, Request) :-
    Headers = [],
    Request = _{
        model:Model,
        messages:[_{role:system, content:System}|Messages],
        stream:false,
        options:_{num_predict:1024}
    }.

request_provider(Provider, Endpoint, Headers, Request, Result) :-
    llm_timeouts(_, _, TotalTimeout),
    catch(
        call_with_time_limit(
            TotalTimeout,
            request_with_retries(Provider, Endpoint, Headers, Request, 0, Result)
        ),
        Error,
        request_exception_result(Error, Result)
    ).

request_with_retries(Provider, Endpoint, Headers, Request, Attempt, Result) :-
    request_once(Endpoint, Headers, Request, Outcome),
    ( retry_outcome(Outcome), llm_retry_limit(MaxRetries), Attempt < MaxRetries
    -> NextAttempt is Attempt + 1,
       request_with_retries(Provider, Endpoint, Headers, Request, NextAttempt, Result)
    ; outcome_result(Provider, Outcome, Result)
    ).

request_once(Endpoint, Headers, Request, Outcome) :-
    llm_timeouts(ConnectTimeout, ReadTimeout, _TotalTimeout),
    maplist(header_option, Headers, HeaderOptions),
    append(
        [
            method(post),
            post(json(Request)),
            status_code(Status),
            timeout(ReadTimeout),
            connection('Keep-Alive')
        ],
        HeaderOptions,
        Options
    ),
    catch(
        call_with_time_limit(
            ConnectTimeout,
            http_open(Endpoint, Stream, Options)
        ),
        OpenError,
        Outcome = exception(OpenError)
    ),
    ( var(Outcome)
    -> read_response(Stream, Status, Outcome)
    ; true
    ).

read_response(Stream, Status, Outcome) :-
    catch(
        setup_call_cleanup(
            true,
            json_read_dict(Stream, Reply),
            close(Stream)
        ),
        ReadError,
        true
    ),
    ( var(ReadError)
    -> Outcome = response(Status, Reply)
    ; Outcome = response_error(Status, ReadError)
    ).

header_option(Name=Value, request_header(Name=Value)).

retry_outcome(response(Status, _)) :-
    memberchk(Status, [429, 500, 502, 503, 504]).
retry_outcome(response_error(Status, _)) :-
    memberchk(Status, [429, 500, 502, 503, 504]).
retry_outcome(exception(Error)) :-
    retryable_exception(Error).

retryable_exception(time_limit_exceeded).
retryable_exception(error(timeout_error(_, _), _)).
retryable_exception(error(socket_error(_), _)).
retryable_exception(error(io_error(_, _), _)).

outcome_result(Provider, response(Status, Reply), Result) :-
    ( between(200, 299, Status)
    -> parse_provider_response(Provider, Reply, Result)
    ; Status =:= 429
    -> Result = llm_result(error, llm_error(rate_limit, "Provider rate limit", Status))
    ; Result = llm_result(error, llm_error(http, "Provider HTTP error", Status))
    ).
outcome_result(_, exception(Error), Result) :-
    request_exception_result(Error, Result).
outcome_result(_, response_error(Status, Error), Result) :-
    ( Status =:= 429
    -> Result = llm_result(
           error,
           llm_error(rate_limit, "Provider rate limit", Status)
       )
    ; between(200, 299, Status)
    -> request_exception_result(Error, Result)
    ; Result = llm_result(error, llm_error(http, "Provider HTTP error", Status))
    ).

request_exception_result(
    time_limit_exceeded,
    llm_result(error, llm_error(timeout, "LLM request timed out", none))
) :- !.
request_exception_result(Error, llm_result(error, llm_error(connection, Message, none))) :-
    retryable_exception(Error),
    term_string(Error, Message), !.
request_exception_result(Error, llm_result(error, llm_error(malformed_response, Message, none))) :-
    term_string(Error, Message).

parse_provider_response(anthropic, Reply, Result) :-
    ( get_dict(content, Reply, Content),
      member(Block, Content),
      get_dict(type, Block, "text"),
      get_dict(text, Block, Text)
    -> response_text_result(Text, Result)
    ; Result = llm_result(
          error,
          llm_error(malformed_response, "Anthropic response schema mismatch", none)
      )
    ).
parse_provider_response(openai, Reply, Result) :-
    ( get_dict(choices, Reply, [First|_]),
      get_dict(message, First, Message),
      get_dict(content, Message, Text)
    -> response_text_result(Text, Result)
    ; Result = llm_result(
          error,
          llm_error(malformed_response, "OpenAI response schema mismatch", none)
      )
    ).
parse_provider_response(ollama, Reply, Result) :-
    ( get_dict(message, Reply, Message),
      get_dict(content, Message, Text)
    -> response_text_result(Text, Result)
    ; Result = llm_result(
          error,
          llm_error(malformed_response, "Ollama response schema mismatch", none)
      )
    ).

response_text_result(Text, Result) :-
    text_string(Text, TextString),
    ( TextString == ""
    -> Result = llm_result(
           error,
           llm_error(empty_response, "Provider returned an empty response", none)
       )
    ; Result = llm_result(success, TextString)
    ).

unwrap_llm_result(llm_result(success, Response), Response).
unwrap_llm_result(llm_result(error, Error), _) :-
    throw(error(Error, _)).

text_string(Value, String) :-
    ( string(Value) -> String = Value ; atom_string(Value, String) ).

close_llm_client :-
    http_close_keep_alive(_),
    reset_llm_history.
