:- use_module(library(plunit)).
:- use_module('../modules/rlm_rewrite').

:- dynamic ambient_rlm_root/1.
:- dynamic ambient_openrouter_key/1.
:- dynamic captured_request/1.

:- (   getenv('ZARA_PROLOG_RLM_ROOT', RootValue), RootValue \== ''
   ->  atom_string(RootValue, AmbientRoot),
       assertz(ambient_rlm_root(AmbientRoot))
   ;   true
   ),
   (   getenv('OPENROUTER_API_KEY', KeyValue), KeyValue \== ''
   ->  atom_string(KeyValue, AmbientKey),
       assertz(ambient_openrouter_key(AmbientKey))
   ;   true
   ).

restore_root :-
    (   ambient_rlm_root(Root)
    ->  setenv('ZARA_PROLOG_RLM_ROOT', Root)
    ;   unsetenv('ZARA_PROLOG_RLM_ROOT')
    ).

restore_openrouter_key :-
    (   ambient_openrouter_key(Key)
    ->  setenv('OPENROUTER_API_KEY', Key)
    ;   unsetenv('OPENROUTER_API_KEY')
    ).

reset_rlm_state :-
    rlm_rewrite:reset_rlm_runtime,
    retractall(captured_request(_)).

scripted_rewrite_model(Request, ok(Response)) :-
    retractall(captured_request(_)),
    assertz(captured_request(Request)),
    model_response_with_text('{"intent":"open","args":["firefox"]}', Response).

unparseable_rewrite_model(Request, ok(Response)) :-
    retractall(captured_request(_)),
    assertz(captured_request(Request)),
    model_response_with_text('this is not json at all', Response).

model_response_with_text(Text, model_response{
        provider:fake,
        requested_model:fake,
        selected_model:fake,
        response_id:"response_1",
        assistant:message{role:assistant,
                          content:Text,
                          tool_calls:[],
                          reasoning:"",
                          reasoning_details:[]},
        text:Text,
        tool_calls:[],
        reasoning:"",
        reasoning_details:[],
        finish_reason:stop,
        usage:usage{present:true,
                    prompt_tokens:2,
                    completion_tokens:1,
                    total_tokens:3,
                    cost:0.0},
        metadata:provider_metadata{provider:fake,
                                   http_status:200,
                                   response_received:true}
    }).

failing_rewrite_model(_, error(direct_error{phase:provider,
                                            kind:fake_provider_failure,
                                            message:"scripted direct failure"})).

captured_user_message(Content) :-
    captured_request(Request),
    member(Message, Request.messages),
    Message.role == user,
    sub_string(Message.content, _, _, _, Content).

:- begin_tests(rlm_rewrite).

test(rewrite_fails_closed_without_root,
     [throws(error(rlm_rewrite_error(root_missing), _)),
      cleanup(restore_root)]) :-
    reset_rlm_state,
    unsetenv('ZARA_PROLOG_RLM_ROOT'),
    rlm_rewrite:rewrite_with_rlm("open firefox", _, _).

test(rewrite_fails_closed_with_invalid_root,
     [throws(error(rlm_rewrite_error(root_invalid(_)), _)),
      cleanup(restore_root)]) :-
    reset_rlm_state,
    setenv('ZARA_PROLOG_RLM_ROOT', '/nonexistent-zara-rlm-root'),
    rlm_rewrite:rewrite_with_rlm("open firefox", _, _).

test(rewrite_fails_closed_without_api_key,
     [condition(ambient_rlm_root(_)),
      throws(error(rlm_rewrite_error(missing_api_key), _)),
      cleanup((restore_root, restore_openrouter_key))]) :-
    reset_rlm_state,
    ambient_rlm_root(Root),
    setenv('ZARA_PROLOG_RLM_ROOT', Root),
    unsetenv('OPENROUTER_API_KEY'),
    rlm_rewrite:rewrite_with_rlm("open firefox", _, _).

test(rlm_runtime_contract_predicates_exist, [condition(ambient_rlm_root(_))]) :-
    reset_rlm_state,
    ambient_rlm_root(Root),
    setenv('ZARA_PROLOG_RLM_ROOT', Root),
    rlm_rewrite:ensure_runtime_loaded,
    assertion(current_predicate(rlm_direct:rlm_direct/4)),
    assertion(current_predicate(rlm_chain:openrouter_provider/2)).

test(scripted_direct_rewrite_parses_intent_and_args, [condition(ambient_rlm_root(_))]) :-
    reset_rlm_state,
    ambient_rlm_root(Root),
    setenv('ZARA_PROLOG_RLM_ROOT', Root),
    setenv('OPENROUTER_API_KEY', 'test-only-key'),
    rlm_rewrite:rewrite_with_rlm("play some spotify music", Intent, Args,
                                 [model_handler(user:scripted_rewrite_model)]),
    assertion(Intent == "open"),
    assertion(Args == ["firefox"]),
    rlm_rewrite:rewrite_prompt("play some spotify music", Prompt),
    assertion(captured_user_message(Prompt)).

test(unparseable_direct_value_falls_back_to_ask, [condition(ambient_rlm_root(_))]) :-
    reset_rlm_state,
    ambient_rlm_root(Root),
    setenv('ZARA_PROLOG_RLM_ROOT', Root),
    setenv('OPENROUTER_API_KEY', 'test-only-key'),
    rlm_rewrite:rewrite_with_rlm("play some spotify music", Intent, Args,
                                 [model_handler(user:unparseable_rewrite_model)]),
    assertion(Intent == ask),
    assertion(Args == ["this is not json at all"]).

test(direct_error_outcome_fails_closed,
     [condition(ambient_rlm_root(_)),
      throws(error(rlm_rewrite_error(direct(_)), _))]) :-
    reset_rlm_state,
    ambient_rlm_root(Root),
    setenv('ZARA_PROLOG_RLM_ROOT', Root),
    setenv('OPENROUTER_API_KEY', 'test-only-key'),
    rlm_rewrite:rewrite_with_rlm("open firefox", _, _,
                                 [model_handler(user:failing_rewrite_model)]).

test(rewrite_options_are_bounded_and_direct, [condition(ambient_rlm_root(_))]) :-
    rlm_rewrite:rewrite_options([], Options),
    memberchk(capabilities([]), Options),
    memberchk(provider_name(openrouter), Options),
    memberchk(provider(Provider), Options),
    Provider = provider(openrouter, _),
    memberchk(budget(Budget), Options),
    Budget.max_model_calls =< 4,
    Budget.max_tool_calls == 0,
    Budget.max_context_ops == 0,
    Budget.time_limit =< 60.0,
    memberchk(temperature(0), Options).

test(rewrite_options_accept_test_handler, [condition(ambient_rlm_root(_))]) :-
    rlm_rewrite:rewrite_options([model_handler(user:scripted_rewrite_model)], Options),
    memberchk(model_handler(user:scripted_rewrite_model), Options).

:- end_tests(rlm_rewrite).
