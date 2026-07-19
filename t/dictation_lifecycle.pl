:- begin_tests(dictation_lifecycle).

:- use_module('../modules/dictate').
:- use_module('../kb/config').
:- use_module(library(process)).

with_command(Command, Goal) :-
    asserta(kb_config:dictation_command(Command), Ref),
    call_cleanup(Goal, erase(Ref)).

reset_dictation :-
    dictation:stop_dictation.

wait_until_inactive(0) :-
    \+ dictation:dictation_active, !.
wait_until_inactive(Attempts) :-
    Attempts > 0,
    sleep(0.05),
    Next is Attempts - 1,
    wait_until_inactive(Next).

test(start_and_duplicate_start, [setup(reset_dictation), cleanup(reset_dictation)]) :-
    with_command("sleep 30", (
        dictation:start_dictation,
        dictation:dictation_active,
        dictation:read_pidfile(FirstPid),
        dictation:start_dictation,
        dictation:read_pidfile(FirstPid)
    )).

test(voice_self_stop, [setup(reset_dictation), cleanup(reset_dictation)]) :-
    with_command("sleep 30", (
        dictation:start_dictation,
        dictation:read_pidfile(Pid),
        process_kill(Pid, kill),
        wait_until_inactive(40)
    )).

test(stale_pid_is_cleaned, [setup(reset_dictation), cleanup(reset_dictation)]) :-
    dictation:write_pidfile(99999999),
    \+ dictation:dictation_active,
    dictation:dictation_pidfile(Path),
    \+ exists_file(Path).

test(invalid_pid_is_cleaned, [setup(reset_dictation), cleanup(reset_dictation)]) :-
    dictation:dictation_pidfile(Path),
    setup_call_cleanup(open(Path, write, Stream), write(Stream, 'not-a-pid'), close(Stream)),
    \+ dictation:dictation_active,
    \+ exists_file(Path).

test(empty_pid_is_cleaned, [setup(reset_dictation), cleanup(reset_dictation)]) :-
    dictation:dictation_pidfile(Path),
    setup_call_cleanup(open(Path, write, Stream), true, close(Stream)),
    \+ dictation:dictation_active,
    \+ exists_file(Path).

test(already_dead_process_fails_start,
     [setup(reset_dictation), cleanup(reset_dictation), fail]) :-
    with_command(["true"], dictation:start_dictation).

test(cleanup_after_launch_exception, [setup(reset_dictation), cleanup(reset_dictation)]) :-
    getenv('ZARA_DICTATION_LOGFILE', OriginalLog),
    setenv('ZARA_DICTATION_LOGFILE', '/missing/zara/dictation.log'),
    call_cleanup(
        catch(dictation:start_dictation, _, true),
        setenv('ZARA_DICTATION_LOGFILE', OriginalLog)
    ),
    \+ dictation:dictation_active.

test(repeated_start_stop, [setup(reset_dictation), cleanup(reset_dictation)]) :-
    with_command("sleep 30", (
        dictation:start_dictation,
        dictation:stop_dictation,
        \+ dictation:dictation_active,
        dictation:start_dictation,
        dictation:stop_dictation,
        \+ dictation:dictation_active
    )).

:- end_tests(dictation_lifecycle).
