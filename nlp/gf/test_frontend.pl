:- use_module('../../modules/gf_frontend').
:- use_module('../../modules/symbolic_dialogue_turn').
:- begin_tests(gf_frontend).

test(help_uses_canonical_dialogue) :-
    gf_canonical_turn(help, passive, [],
        gf_turn(turn([frame(intent(ns(conversation), name(help)), [], complete)], help, []),
                'HelpReply', evidence(frontend('gf-pilot/v1'), model_calls(0), provider_calls(0)))).

test(greeting_preserves_existing_context) :-
    Context = partial_frame(frame(intent(ns(device), name('timer.set')), [], missing([duration])), [duration]),
    gf_canonical_turn(hello, passive, Context, gf_turn(turn(_, greeting, Context), 'GreetingReply', _)).

test(timer_is_not_execution_success) :-
    gf_canonical_turn('timer 2 minutes', passive, [], gf_turn(turn([Frame], dispatch_required(Frame), completed_frame(Frame)), 'PendingReply', _)),
    Frame = frame(intent(ns(device), name('timer.set')),
        [slot(name(duration), value(duration(120)), origin(utterance))], complete).

test(missing_timer_duration) :-
    gf_canonical_turn(timer, passive, [], gf_turn(turn([Frame], clarify(slot(duration)), partial_frame(Frame, [duration])), 'DurationReply', _)).

test(followup_reuses_canonical_owner) :-
    gf_canonical_turn(timer, passive, [], gf_turn(turn(_, _, Context), _, _)),
    gf_canonical_turn('5 minutes', passive, Context, gf_turn(turn([Frame], _, _), 'PendingReply', _)),
    Frame = frame(_, [slot(name(duration), value(duration(300)), origin(follow_up))], complete).

test(correction_keeps_origin) :-
    gf_canonical_turn(timer, passive, [], gf_turn(turn(_, _, Context), _, _)),
    gf_canonical_turn('actually 5 minutes', passive, Context, gf_turn(turn([Frame], _, _), 'PendingReply', _)),
    Frame = frame(_, [slot(name(duration), value(duration(300)), origin(correction))], complete).

test(cancel_closes_context) :-
    gf_canonical_turn(timer, passive, [], gf_turn(turn(_, _, Context), _, _)),
    gf_canonical_turn(cancel, passive, Context, gf_turn(turn(_, cancelled, []), 'CancelledReply', _)).

test(parity_not_parallel_semantics) :-
    forall(member(Text, [hello, help, thanks, okay, cancel, timer, 'timer 1 minutes', 'open termux']),
        (symbolic_dialogue_turn:dialogue_turn(Text, passive, [], Expected),
         gf_canonical_turn(Text, passive, [], gf_turn(Actual, _, _)),
         assertion(Actual == Expected))).

test(identity_is_not_a_greeting_or_a_fake_capability) :-
    gf_canonical_turn('who are you', passive, [], gf_turn(turn([], unsupported, []), 'UnsupportedReply', _)).

test(empty_input, [fail]) :- gf_canonical_turn('', passive, [], _).
test(non_text_input, [fail]) :- gf_canonical_turn(shell(id), passive, [], _).
test(oversized_input, [fail]) :-
    length(Codes, 513), maplist(=(97), Codes), atom_codes(Text, Codes),
    gf_canonical_turn(Text, passive, [], _).
test(unverified_actions_never_render_done) :-
    gf_reply_tree(dispatch_required(anything), 'PendingReply'),
    gf_reply_tree(verified('Done', evidence), 'UnsupportedReply').
:- end_tests(gf_frontend).
