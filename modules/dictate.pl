:- module(dictation, [start_dictation/0, stop_dictation/0, dictation_status/0]).

:- use_module(library(process)).
:- use_module(library(filesex)).
:- use_module(library(system)).

% Adjust path to the helper script if you store it elsewhere:
dictation_script_path('/home/you/starintel/tools/zara_dictate.py').
dictation_pidfile('/tmp/zara_dictation.pid').

start_dictation :-
    dictation_pidfile(PIDFILE),
    ( exists_file(PIDFILE) ->
        format("Dictation already running (pidfile ~w). Use stop_dictation/0 first.~n", [PIDFILE])
    ;
        dictation_script_path(SCRIPT),
        % Ensure the script is executable
        ( exists_file(SCRIPT) ->
            true
        ;
            format("ERROR: dictation script not found at ~w~n", [SCRIPT]), !
        ),
        % Start as detached background process. We use sh -c 'nohup ... & echo $! > pidfile'
        atomic_list_concat(['nohup python3 ', shell_quote(SCRIPT), ' >> /tmp/zara_dictation.log 2>&1 & echo $! > ', PIDFILE], Cmd),
        process_create(path(sh), ['-c', Cmd], [detached(true)]),
        sleep(1),
        ( exists_file(PIDFILE) ->
            read_file_to_string(PIDFILE, S, []),
            string_codes(S, Codes), string_trim(S, T),
            format("Dictation started, pid: ~w~n", [T])
        ;
            format("Failed to start dictation. See /tmp/zara_dictation.log~n")
        )
    ).

stop_dictation :-
    dictation_pidfile(PIDFILE),
    ( exists_file(PIDFILE) ->
        catch(read_file_to_string(PIDFILE, S, []), _, S = ""),
        string_trim(S, PIDStr),
        ( PIDStr == "" ->
            format("Pidfile ~w is empty. Removing it.~n", [PIDFILE]), delete_file(PIDFILE)
        ;
            atomic_list_concat(['kill ', PIDStr], KillCmd),
            process_create(path(sh), ['-c', KillCmd], []),
            sleep(1),
            ( exists_file(PIDFILE) -> delete_file(PIDFILE) ; true ),
            format("Sent SIGTERM to pid ~w and removed pidfile.~n", [PIDStr])
        )
    ;
        format("No dictation pidfile found at ~w.~n", [PIDFILE])
    ).

dictation_status :-
    dictation_pidfile(PIDFILE),
    ( exists_file(PIDFILE) ->
        read_file_to_string(PIDFILE, S, []),
        string_trim(S, PIDStr),
        format("Dictation appears running; pidfile ~w contains: ~w~n", [PIDFILE, PIDStr])
    ;
        format("Dictation not running (no pidfile ~w).~n", [PIDFILE])
    ).

% helper to shell-quote a path
shell_quote(Str, Quoted) :-
    format(string(Quoted), "'~w'", [Str]).
