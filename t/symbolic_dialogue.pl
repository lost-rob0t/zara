:- begin_tests(symbolic_dialogue).

:- use_module('../modules/symbolic_dialogue').
:- use_module('../modules/symbolic_dialogue_turn').

frame_greet(frame(intent(ns(conversation), name(greet)), [], complete)).
frame_missing_timer(frame(intent(ns(device), name('timer.set')), [], missing([duration]))).
frame_cancel(frame(intent(ns(conversation), name(cancel)), [], complete)).
frame_ambiguous(frame(intent(ns(app), name(open)), [], ambiguous([firefox, chromium, emacs]))).
frame_open_firefox(frame(intent(ns(app), name(open)),
    [slot(name(target), value(ref(kind(app_alias), id(firefox))), origin(utterance))],
    complete)).
frame_timer_5m(frame(intent(ns(device), name('timer.set')),
    [slot(name(duration), value(duration(300)), origin(utterance))],
    complete)).

test(greeting_is_zero_model_symbolic_reply) :-
    frame_greet(Frame),
    symbolic_dialogue:symbolic_reply(frame(Frame), Text, Evidence),
    assertion(Text == "Hey — what can I help with?"),
    assertion(Evidence == evidence(renderer('symbolic-dcg/v1'), provider_calls(0), model_calls(0))).

test(missing_slot_renders_typed_clarification) :-
    frame_missing_timer(Frame),
    symbolic_dialogue:response_act(frame(Frame), Act),
    assertion(Act == clarify(slot(duration))),
    symbolic_dialogue:render_response(Act, Text),
    assertion(Text == "How long should I set the timer for?").

test(canonical_timer_clarification_carries_context_across_turns) :-
    symbolic_dialogue_turn:dialogue_turn("timer", passive, [], Turn1),
    Turn1 = turn([Frame1], clarify(slot(duration)), Context1),
    assertion(Context1 == partial_frame(Frame1, [duration])),
    symbolic_dialogue_turn:dialogue_turn("5 minutes", passive, Context1, Turn2),
    Turn2 = turn([Frame2], dispatch_required(Frame2), Context2),
    assertion(Context2 == completed_frame(Frame2)),
    assertion(Frame2 = frame(intent(ns(device), name('timer.set')),
        [slot(name(duration), value(duration(300)), origin(follow_up))], complete)).

test(shared_context_validator_accepts_emitted_partial_and_completed_contexts) :-
    symbolic_dialogue_turn:dialogue_turn("timer", passive, [], Turn1),
    Turn1 = turn(_, _, Context1),
    assertion(symbolic_dialogue_turn:valid_dialogue_context(Context1)),
    symbolic_dialogue_turn:dialogue_turn("5 minutes", passive, Context1, Turn2),
    Turn2 = turn(_, _, Context2),
    assertion(symbolic_dialogue_turn:valid_dialogue_context(Context2)).

test(shared_context_validator_accepts_ambiguous_context) :-
    frame_ambiguous(Frame),
    Context = partial_frame(Frame, [firefox, chromium, emacs]),
    assertion(symbolic_dialogue_turn:valid_dialogue_context(Context)).

test(shared_context_validator_rejects_mismatched_open_slots, [fail]) :-
    frame_missing_timer(Frame),
    symbolic_dialogue_turn:valid_dialogue_context(partial_frame(Frame, [target])).

test(shared_context_validator_rejects_future_shape, [fail]) :-
    symbolic_dialogue_turn:valid_dialogue_context(future_context(foo)).

test(completed_prior_frame_correction_is_deterministic) :-
    frame_timer_5m(Frame0),
    symbolic_dialogue_turn:dialogue_turn(
        "actually 10 minutes",
        passive,
        completed_frame(Frame0),
        Turn
    ),
    Turn = turn([Frame], dispatch_required(Frame), completed_frame(Frame)),
    assertion(Frame = frame(intent(ns(device), name('timer.set')),
        [slot(name(duration), value(duration(600)), origin(correction))], complete)).

test(completed_prior_frame_non_correction_resolves_fresh) :-
    frame_timer_5m(Frame0),
    symbolic_dialogue_turn:dialogue_turn(
        "open firefox",
        passive,
        completed_frame(Frame0),
        Turn
    ),
    Turn = turn([Frame], dispatch_required(Frame), completed_frame(Frame)),
    assertion(Frame = frame(intent(ns(app), name(open)),
        [slot(name(target), value(ref(kind(app_alias), id(firefox))), origin(utterance))],
        complete)).

test(cancel_clears_completed_prior_frame_context) :-
    frame_timer_5m(Frame0),
    symbolic_dialogue_turn:dialogue_turn(
        "cancel",
        passive,
        completed_frame(Frame0),
        turn([Frame], cancelled, [])
    ),
    assertion(Frame == frame(intent(ns(conversation), name(cancel)), [], complete)).

test(social_turn_preserves_completed_prior_frame_context) :-
    frame_timer_5m(Frame0),
    symbolic_dialogue_turn:dialogue_turn(
        "thanks",
        passive,
        completed_frame(Frame0),
        turn([Frame], acknowledgement(thanks), Context1)
    ),
    assertion(Frame == frame(intent(ns(conversation), name(thanks)), [], complete)),
    assertion(Context1 == completed_frame(Frame0)).

test(ordinal_follow_up_reuses_canonical_ambiguous_frame) :-
    frame_ambiguous(Frame0),
    Context0 = partial_frame(Frame0, [firefox, chromium, emacs]),
    symbolic_dialogue_turn:dialogue_turn("the second one", passive, Context0, Turn),
    Turn = turn([Frame], dispatch_required(Frame), completed_frame(Frame)),
    assertion(Frame = frame(intent(ns(app), name(open)),
        [slot(name(target), value(ref(kind(app_alias), id(chromium))), origin(follow_up))],
        complete)).

test(cancel_is_terminal_without_fallback) :-
    frame_cancel(Frame),
    symbolic_dialogue:symbolic_reply(frame(Frame), Text, Evidence),
    assertion(Text == "Cancelled."),
    assertion(Evidence == evidence(renderer('symbolic-dcg/v1'), provider_calls(0), model_calls(0))).

test(second_choice_reference_resolves_deterministically) :-
    symbolic_dialogue:resolve_reference("the second one", [firefox, chromium, emacs], Result),
    assertion(Result == resolved(chromium)).

test(pronoun_reference_requires_unique_candidate) :-
    symbolic_dialogue:resolve_reference("it", [firefox, chromium], Result),
    assertion(Result == clarify(ambiguous_reference)).

test(pronoun_reference_resolves_single_candidate) :-
    symbolic_dialogue:resolve_reference("that", [firefox], Result),
    assertion(Result == resolved(firefox)).

test(out_of_range_ordinal_does_not_guess) :-
    symbolic_dialogue:resolve_reference("the fourth one", [firefox, chromium, emacs], Result),
    assertion(Result == clarify(reference_not_found)).

test(ambiguous_frame_lists_choices_in_stable_order) :-
    frame_ambiguous(Frame),
    symbolic_dialogue:response_act(frame(Frame), Act),
    assertion(Act == choose([firefox, chromium, emacs])),
    symbolic_dialogue:render_response(Act, Text),
    assertion(Text == "I found a few matches: 1) firefox; 2) chromium; 3) emacs. Which one?").

test(complete_effect_frame_never_claims_unverified_success) :-
    frame_open_firefox(Frame),
    symbolic_dialogue:response_act(frame(Frame), Act),
    assertion(Act == dispatch_required(Frame)),
    symbolic_dialogue:symbolic_reply(frame(Frame), Text, Evidence),
    assertion(Text == "That action needs capability-checked execution before I can report success."),
    assertion(Evidence == evidence(renderer('symbolic-dcg/v1'), provider_calls(0), model_calls(0))).

test(verified_effect_may_report_success) :-
    symbolic_dialogue:symbolic_reply(
        effect_result(verified(opened_firefox, postcondition('process:firefox'))),
        Text,
        Evidence),
    assertion(Text == "Done: opened_firefox."),
    assertion(Evidence == evidence(renderer('symbolic-dcg/v1'), provider_calls(0), model_calls(0))).

test(denied_plan_is_typed_and_bounded) :-
    symbolic_dialogue:symbolic_reply(plan_status(denied(capability_required)), Text, Evidence),
    assertion(Text == "I can’t do that: capability_required."),
    assertion(Evidence == evidence(renderer('symbolic-dcg/v1'), provider_calls(0), model_calls(0))).

test(unavailable_plan_is_typed_and_bounded) :-
    symbolic_dialogue:symbolic_reply(plan_status(unavailable(provider_unavailable)), Text, _),
    assertion(Text == "That isn’t available right now: provider_unavailable.").

test(expert_summary_preserves_supplied_text_and_evidence) :-
    symbolic_dialogue:response_act(
        expert_result(summary("Timer is already running."), evidence('expert:timer/7')),
        Act),
    assertion(Act == answer(expert, "Timer is already running.", evidence('expert:timer/7'))),
    symbolic_dialogue:render_response(Act, Text),
    assertion(Text == "Timer is already running.").

test(expert_why_follow_up_is_deterministic_and_zero_model) :-
    Previous = answer(expert, "Timer is already running.", evidence('expert:timer/7')),
    symbolic_dialogue:symbolic_follow_up("why?", Previous, Text, Evidence),
    assertion(Text == "I answered from evidence expert:timer/7."),
    assertion(Evidence == evidence(renderer('symbolic-dcg/v1'), provider_calls(0), model_calls(0))).

test(expert_pronoun_follow_up_repeats_unique_typed_answer) :-
    Previous = answer(expert, "Timer is already running.", evidence('expert:timer/7')),
    symbolic_dialogue:symbolic_follow_up("that", Previous, Text, Evidence),
    assertion(Text == "Timer is already running."),
    assertion(Evidence == evidence(renderer('symbolic-dcg/v1'), provider_calls(0), model_calls(0))).

test(expert_follow_up_without_referenceable_prior_act_clarifies) :-
    symbolic_dialogue:symbolic_follow_up("why?", greeting, Text, Evidence),
    assertion(Text == "What are you referring to?"),
    assertion(Evidence == evidence(renderer('symbolic-dcg/v1'), provider_calls(0), model_calls(0))).

test(unknown_input_never_escalates) :-
    symbolic_dialogue:symbolic_reply(unsupported("frobnicate quantum socks"), Text, Evidence),
    assertion(Text == "I don’t know how to handle that symbolically yet."),
    assertion(Evidence == evidence(renderer('symbolic-dcg/v1'), provider_calls(0), model_calls(0))).

:- end_tests(symbolic_dialogue).
