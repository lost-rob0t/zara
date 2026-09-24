:- begin_tests(symbolic_social_vocabulary).

:- use_module('../modules/symbolic_dialogue').
:- use_module('../modules/symbolic_dialogue_turn').

zero_evidence(evidence(renderer('symbolic-dcg/v1'), provider_calls(0), model_calls(0))).

assert_social_turn(Input, ExpectedIntent, ExpectedAct, ExpectedText) :-
    symbolic_dialogue_turn:dialogue_turn(
        Input,
        passive,
        [],
        turn([Frame], ExpectedAct, [])
    ),
    assertion(Frame == frame(intent(ns(conversation), name(ExpectedIntent)), [], complete)),
    symbolic_dialogue:symbolic_reply(frame(Frame), Text, Evidence),
    assertion(Text == ExpectedText),
    zero_evidence(ExpectedEvidence),
    assertion(Evidence == ExpectedEvidence).

test(help_me_is_local_symbolic_help) :-
    assert_social_turn(
        "help me",
        help,
        help,
        "I can help with conversation, device and media actions, search, navigation, and registered experts. What do you want to do?"
    ).

test(thank_you_is_local_symbolic_acknowledgement) :-
    assert_social_turn(
        "thank you",
        thanks,
        acknowledgement(thanks),
        "You’re welcome."
    ).

test(thanks_is_local_symbolic_acknowledgement) :-
    assert_social_turn(
        "thanks",
        thanks,
        acknowledgement(thanks),
        "You’re welcome."
    ).

test(okay_is_local_symbolic_acknowledgement) :-
    assert_social_turn(
        "okay",
        acknowledge,
        acknowledgement(acknowledged),
        "Got it."
    ).

test(got_it_is_local_symbolic_acknowledgement) :-
    assert_social_turn(
        "got it",
        acknowledge,
        acknowledgement(acknowledged),
        "Got it."
    ).

:- end_tests(symbolic_social_vocabulary).
