:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_server)).
:- use_module(library(http/http_json)).
:- use_module(library(plunit)).
:- use_module('../modules/llm_client').
:- use_module('../kb/config').

:- dynamic server_port/1.
:- dynamic fake_provider/1.
:- dynamic fake_mode/1.
:- dynamic request_count/1.
:- dynamic captured_request/2.

:- http_handler(root(llm), fake_llm_handler, []).

start_fake_server :-
    http_server(http_dispatch, [port(Port)]),
    asserta(server_port(Port)),
    setenv('ANTHROPIC_API_KEY', 'literal-key'),
    setenv('OPENAI_API_KEY', 'literal-key').

stop_fake_server :-
    close_llm_client,
    server_port(Port),
    http_stop_server(Port, []),
    retractall(server_port(_)),
    unsetenv('ZARA_ANTHROPIC_ENDPOINT').

configure_provider(Provider, Mode) :-
    server_port(Port),
    format(string(Endpoint), 'http://127.0.0.1:~w/llm', [Port]),
    retractall(kb_config:llm_provider(_)),
    asserta(kb_config:llm_provider(Provider)),
    retractall(kb_config:llm_model(_)),
    asserta(kb_config:llm_model("test-model")),
    retractall(kb_config:llm_endpoint(_)),
    asserta(kb_config:llm_endpoint(Endpoint)),
    ( Provider == anthropic -> setenv('ZARA_ANTHROPIC_ENDPOINT', Endpoint) ; true ),
    retractall(fake_provider(_)),
    asserta(fake_provider(Provider)),
    retractall(fake_mode(_)),
    asserta(fake_mode(Mode)),
    retractall(request_count(_)),
    asserta(request_count(0)),
    retractall(captured_request(_, _)),
    reset_llm_history.

fake_llm_handler(Request) :-
    http_read_json_dict(Request, Body),
    retractall(captured_request(_, _)),
    asserta(captured_request(Request, Body)),
    retract(request_count(Count)),
    Next is Count + 1,
    asserta(request_count(Next)),
    fake_mode(Mode),
    fake_reply(Mode, Next).

fake_reply(rate_limit, Count) :-
    Count < 3, !,
    reply_json_dict(_{error:"slow down"}, [status(429)]).
fake_reply(timeout, _) :-
    sleep(0.2),
    reply_json_dict(_{message:_{content:"late"}}).
fake_reply(malformed, _) :-
    format('Content-type: application/json~n~nnot-json').
fake_reply(empty, _) :-
    reply_json_dict(_{message:_{content:""}}).
fake_reply(_, _) :-
    fake_provider(Provider),
    success_reply(Provider).

success_reply(anthropic) :-
    reply_json_dict(_{content:[_{type:"text", text:"anthropic-ok"}]}).
success_reply(openai) :-
    reply_json_dict(_{choices:[_{message:_{content:"openai-ok"}}]}).
success_reply(ollama) :-
    reply_json_dict(_{message:_{content:"ollama-ok"}}).

:- begin_tests(llm_clients, [setup(start_fake_server), cleanup(stop_fake_server)]).

test(anthropic_golden_request) :-
    Messages = [_{role:user, content:"hello"}],
    serialize_llm_request(
        anthropic, "model", "literal-key", "system", Messages, Headers, Request
    ),
    assertion(memberchk('x-api-key'="literal-key", Headers)),
    assertion(memberchk('anthropic-version'="2023-06-01", Headers)),
    assertion(Request.system == "system"),
    assertion(Request.messages == Messages),
    assertion(\+ member(_{role:system}, Request.messages)).

test(openai_golden_request) :-
    Messages = [_{role:user, content:"hello"}],
    serialize_llm_request(
        openai, "model", "literal-key", "system", Messages, Headers, Request
    ),
    assertion(memberchk('Authorization'="Bearer literal-key", Headers)),
    assertion(Request.messages =@= [_{role:system, content:"system"}|Messages]).

test(ollama_golden_request) :-
    Messages = [_{role:user, content:"hello"}],
    serialize_llm_request(
        ollama, "model", "", "system", Messages, Headers, Request
    ),
    assertion(Headers == []),
    assertion(Request.stream == false),
    assertion(Request.options.num_predict == 1024).

test(provider_round_trips, [forall(member(Provider-Expected, [
    anthropic-"anthropic-ok",
    openai-"openai-ok",
    ollama-"ollama-ok"
]))]) :-
    configure_provider(Provider, success),
    llm_query_result("hello", "system", Result),
    assertion(Result == llm_result(success, Expected)).

test(anthropic_history_uses_top_level_system) :-
    configure_provider(anthropic, success),
    llm_query_with_history_result("hello", llm_result(success, _)),
    captured_request(_, Body),
    assertion(Body.system \== ""),
    assertion(\+ member(_{role:"system"}, Body.messages)).

test(openai_authorization_is_literal) :-
    configure_provider(openai, success),
    llm_query_result("hello", "system", llm_result(success, _)),
    captured_request(Request, _),
    assertion(memberchk(authorization('Bearer literal-key'), Request)).

test(rate_limit_retries_are_bounded) :-
    configure_provider(openai, rate_limit),
    llm_client:retractall(llm_retry_limit(_)),
    llm_client:asserta(llm_retry_limit(2)),
    llm_query_result("hello", "system", Result),
    request_count(Count),
    assertion(Result == llm_result(success, "openai-ok")),
    assertion(Count == 3).

test(timeout_is_typed, [cleanup((
    llm_client:retractall(llm_timeouts(_, _, _)),
    llm_client:asserta(llm_timeouts(5.0, 20.0, 30.0)),
    llm_client:retractall(llm_retry_limit(_)),
    llm_client:asserta(llm_retry_limit(2))
))]) :-
    configure_provider(ollama, timeout),
    llm_client:retractall(llm_timeouts(_, _, _)),
    llm_client:asserta(llm_timeouts(0.01, 0.01, 0.03)),
    llm_client:retractall(llm_retry_limit(_)),
    llm_client:asserta(llm_retry_limit(0)),
    llm_query_result("hello", "system", Result),
    sleep(0.2),
    assertion(Result = llm_result(error, llm_error(timeout, _, none))).

test(malformed_response_is_typed) :-
    configure_provider(ollama, malformed),
    llm_query_result("hello", "system", Result),
    assertion(Result = llm_result(error, llm_error(malformed_response, _, none))).

test(empty_response_is_typed) :-
    configure_provider(ollama, empty),
    llm_query_result("hello", "system", Result),
    assertion(Result = llm_result(error, llm_error(empty_response, _, none))).

test(history_is_bounded) :-
    configure_provider(ollama, success),
    forall(
        between(1, 15, Number),
        ( number_string(Number, Prompt),
          llm_query_with_history_result(Prompt, llm_result(success, _))
        )
    ),
    llm_client:current_conversation(History),
    length(History, Length),
    assertion(Length == 20).

test(compatibility_predicate_throws_typed_failure, [throws(error(llm_error(empty_response, _, none), _))]) :-
    configure_provider(ollama, empty),
    llm_query("hello", _).

:- end_tests(llm_clients).
