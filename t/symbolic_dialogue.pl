:- begin_tests(symbolic_dialogue).

:- use_module('../modules/symbolic_dialogue').

frame_greet(frame(intent(ns(conversation), name(greet)), [], complete)).
frame_missing_timer(frame(intent(ns(device), name('timer.set')), [], missing([duration]))).
frame_cancel(frame(intent(ns(conversation), name(cancel)), [], complete)).
frame_ambiguous(frame(intent(ns(app), name(open)), [], ambiguous([firefox, chromium, emacs]))).

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

test(unknown_input_never_escalates) :-
    symbolic_dialogue:symbolic_reply(unsupported("frobnicate quantum socks"), Text, Evidence),
    assertion(Text == "I don’t know how to handle that symbolically yet."),
    assertion(Evidence == evidence(renderer('symbolic-dcg/v1'), provider_calls(0), model_calls(0))).

:- end_tests(symbolic_dialogue).
