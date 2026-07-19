:- begin_tests(process_safety).

:- use_module('../modules/alert').
:- use_module('../modules/commands').
:- use_module('../modules/config_loader').
:- use_module('../modules/dictate').
:- use_module('../kb/config').
:- use_module(library(http/json)).

payload(Payload) :-
    getenv('ZARA_PROCESS_MARKER', Marker),
    format(string(Payload), 'quo''te;$(touch ~w)~n--leading Ω', [Marker]).

reset_log :-
    getenv('ZARA_PROCESS_LOG', Log),
    ( exists_file(Log) -> delete_file(Log) ; true ).

read_invocation(Expected) :-
    getenv('ZARA_PROCESS_LOG', Log),
    wait_for_file(Log, 20),
    read_file_to_string(Log, Contents, []),
    split_string(Contents, "\n", "\n", Lines),
    last(Lines, Line),
    atom_string(LineAtom, Line),
    atom_json_term(LineAtom, Actual, [value_string_as(string)]),
    assertion(Actual == Expected).

wait_for_file(Path, _) :-
    exists_file(Path), !.
wait_for_file(Path, Attempts) :-
    Attempts > 0,
    sleep(0.05),
    Remaining is Attempts - 1,
    wait_for_file(Path, Remaining).

with_app_mapping(Name, Command, Goal) :-
    asserta(kb_config:app_mapping(Name, Command), Ref),
    call_cleanup(Goal, erase(Ref)).

with_dictation_command(Command, Goal) :-
    asserta(kb_config:dictation_command(Command), Ref),
    call_cleanup(Goal, erase(Ref)).

test(search_preserves_literal_query, [setup(reset_log)]) :-
    payload(Payload),
    commands:execute(search, [Payload]),
    config_loader:search_url(Payload, URLAtom),
    atom_string(URLAtom, URL),
    read_invocation(["xdg-open", "--", URL]).

test(alert_preserves_literal_title_and_message, [setup(reset_log)]) :-
    payload(Payload),
    alert:alert("--literal title", normal, '~w', [Payload]),
    read_invocation([
        "notify-send", "-u", "normal", "--", "--literal title", Payload
    ]).

test(app_mapping_preserves_explicit_argv, [setup(reset_log)]) :-
    payload(Payload),
    with_app_mapping(process_safety_app,
                     ["safe-launch", "--leading", Payload],
                     commands:open_app(process_safety_app)),
    read_invocation(["safe-launch", "--leading", Payload]).

test(dictation_preserves_explicit_argv,
     [setup((reset_log, dictation:stop_dictation)),
      cleanup(dictation:stop_dictation)]) :-
    payload(Payload),
    with_dictation_command(["safe-dictate", "--leading", Payload],
        ( dictation:start_dictation,
          read_invocation(["safe-dictate", "--leading", Payload])
        )).

test(nonzero_launch_status_fails, [setup(reset_log), fail]) :-
    commands:launch_process('fail-launch', []).

test(constrained_strings_reject_shell_syntax) :-
    Invalid = [
        "safe-launch 'quote'",
        "safe-launch \"quote\"",
        "safe-launch;touch marker",
        "safe-launch $(touch marker)",
        "safe-launch line\nbreak",
        "sh -c true"
    ],
    forall(member(Command, Invalid),
           \+ config_loader:command_argv(Command, _, _)),
    \+ config_loader:command_argv(["sh", "-c", "true"], _, _).

test(constrained_strings_accept_unicode_and_leading_dash) :-
    config_loader:command_argv("safe-launch --leading Ω",
                               'safe-launch',
                               ["--leading", "Ω"]).

:- end_tests(process_safety).
