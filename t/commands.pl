:- begin_tests(commands).

:- use_module('../modules/commands').

test(known_handler_succeeds) :-
    commands:execute(greet, []).

test(unknown_intent_fails, [fail]) :-
    commands:execute(unknown_intent, []).

test(malformed_arguments_fail, [fail]) :-
    commands:execute(open, []).

test(missing_executable_fails, [fail]) :-
    commands:launch_process('command_that_does_not_exist_zara', []).

test(handler_exception_fails, [fail]) :-
    commands:launch_process(not_a_command(42), []).

:- end_tests(commands).
