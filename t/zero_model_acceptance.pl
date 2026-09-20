:- begin_tests(zero_model_acceptance).

:- use_module('../modules/symbolic_dialogue').
:- use_module('../modules/symbolic_dialogue_turn').
:- use_module('../modules/rlm_rewrite').

zero_evidence(evidence(renderer('symbolic-dcg/v1'), provider_calls(0), model_calls(0))).

assert_zero_reply(Input, ExpectedText) :-
    symbolic_dialogue:symbolic_reply(Input, Text, Evidence),
    assertion(Text == ExpectedText),
    zero_evidence(ExpectedEvidence),
    assertion(Evidence == ExpectedEvidence).

test(multi_turn_clarification_and_verified_result_stay_zero_model) :-
    symbolic_dialogue_turn:dialogue_turn("timer", passive, [], Turn1),
    Turn1 = turn([Frame1], clarify(slot(duration)), Context1),
    assertion(Context1 == partial_frame(Frame1, [duration])),
    assert_zero_reply(frame(Frame1), "How long should I set the timer for?"),

    symbolic_dialogue_turn:dialogue_turn("5 minutes", passive, Context1, Turn2),
    Turn2 = turn([Frame2], dispatch_required(Frame2), []),
    assertion(Frame2 = frame(intent(ns(device), name('timer.set')),
        [slot(name(duration), value(duration(300)), origin(follow_up))], complete)),
    assert_zero_reply(
        frame(Frame2),
        "That action needs capability-checked execution before I can report success."
    ),

    assert_zero_reply(
        effect_result(verified(timer_set, postcondition('timer:duration=300'))),
        "Done: timer_set."
    ).

test(parse_miss_is_terminal_symbolic_unsupported_without_fallback) :-
    symbolic_dialogue_turn:dialogue_turn(
        "frobnicate quantum socks",
        passive,
        [],
        turn([], unsupported, [])
    ),
    assert_zero_reply(
        unsupported("frobnicate quantum socks"),
        "I don’t know how to handle that symbolically yet."
    ).

test(cancelled_reply_is_terminal_and_zero_model) :-
    CancelFrame = frame(intent(ns(conversation), name(cancel)), [], complete),
    assert_zero_reply(frame(CancelFrame), "Cancelled.").

test(zero_budget_blocks_rlm_before_runtime_or_credentials) :-
    assertion(\+ getenv('OPENROUTER_API_KEY', _)),
    assertion(\+ getenv('ZARA_PROLOG_RLM_ROOT', _)),
    assertion(\+ rlm_rewrite:runtime_loaded(_)),
    assertion(\+ current_predicate(rlm_direct:rlm_direct/4)),
    catch(
        rlm_rewrite:rewrite_with_rlm(
            "this input must never reach a provider",
            _Intent,
            _Args,
            [budget(_{max_model_calls:0,
                      max_tool_calls:0,
                      max_context_ops:0,
                      max_total_tokens:0,
                      max_cost_usd:0.0,
                      max_output_bytes:0,
                      time_limit:1.0})]
        ),
        Error,
        true
    ),
    assertion(nonvar(Error)),
    assertion(Error = error(rlm_rewrite_error(model_calls_disabled), _)),
    assertion(\+ rlm_rewrite:runtime_loaded(_)),
    assertion(\+ current_predicate(rlm_direct:rlm_direct/4)).

:- end_tests(zero_model_acceptance).
