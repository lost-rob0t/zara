:- begin_tests(replies).

:- use_module('../modules/command_loop').
:- use_module('../modules/zara_hooks').

without_rich_replies(Goal) :-
    ( getenv('ZARA_RICH_REPLIES', Previous)
    -> setup_call_cleanup(unsetenv('ZARA_RICH_REPLIES'), Goal,
                          setenv('ZARA_RICH_REPLIES', Previous))
    ;  setup_call_cleanup(unsetenv('ZARA_RICH_REPLIES'), Goal, true)
    ).

test(acknowledgement_is_noncommittal) :-
    zara_hooks:reply_text(acknowledgement, Output),
    string_lower(Output, Lower),
    \+ sub_string(Lower, _, _, _, "opened"),
    \+ sub_string(Lower, _, _, _, "completed"),
    \+ sub_string(Lower, _, _, _, "sent").

test(success_has_structured_result) :-
    command_loop:execution_result(greet, [], Result),
    assertion(Result == command_result(success, greet, [], none)).

test(failure_has_structured_result) :-
    command_loop:execution_result(no_such_intent, [target], Result),
    assertion(Result == command_result(failure, no_such_intent,
                                       [target], failed)).

test(open_reply_uses_app_argument) :-
    without_rich_replies((
        zara_hooks:reply_text(
            command_result(success, open, ['Visual Studio Code'], none), Text),
        assertion(Text == "Opened Visual Studio Code.")
    )).

test(search_reply_uses_complete_query) :-
    without_rich_replies((
        zara_hooks:reply_text(
            command_result(success, search, [nietzsche, books], none), Text),
        assertion(Text == "Searching for nietzsche books.")
    )).

test(text_reply_uses_contact_argument) :-
    without_rich_replies((
        zara_hooks:reply_text(
            command_result(success, text, ['Ada Lovelace', hello], none), Text),
        assertion(Text == "Sent the message to Ada Lovelace.")
    )).

test(failure_does_not_masquerade_as_success) :-
    without_rich_replies((
        zara_hooks:reply_text(
            command_result(failure, open, [firefox], failed), Text),
        assertion(Text == "I couldn't open firefox.")
    )).

test(specific_phrases_exclude_generic_fallback) :-
    without_rich_replies((
        zara_hooks:reply_phrases(success, open, Phrases),
        assertion(Phrases == ["Opened ~w."]),
        \+ member("Completed ~w.", Phrases)
    )).

test(generic_phrase_is_used_only_without_specific_set) :-
    zara_hooks:reply_phrases(success, custom_action, Phrases),
    assertion(Phrases == ["Completed ~w."]).

test(rich_phrases_are_opt_in) :-
    setup_call_cleanup(
        setenv('ZARA_RICH_REPLIES', '1'),
        ( zara_hooks:reply_phrases(success, open, Phrases),
          assertion(Phrases = ["Opened ~w."|_]),
          assertion(member("The gate to ~w is open; let new creation arise.",
                           Phrases)),
          \+ member("Completed ~w.", Phrases)
        ),
        unsetenv('ZARA_RICH_REPLIES')
    ).

:- end_tests(replies).
