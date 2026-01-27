:- module(dictation, [start_dictation/0, stop_dictation/0, dictation_status/0]).

:- use_module(library(process)).
:- use_module(library(filesex)).
:- use_module(library(system)).
:- use_module('../kb/config').

dictation_pidfile('/tmp/zara_dictation.pid').
dictation_logfile('/tmp/zara_dictation.log').

%% dictation_command_string(-Command) is det.
%
%  Resolves the full command used to start dictation.
%  Users override this via kb_config:dictation_command/1.
dictation_command_string(Command) :-
    (   kb_config:dictation_command(Command0)
    ->  Command = Command0
    ;   Command = "zara-dictate"
    ).

%% bundled_dictation_script(-Path) is det.
%
%  Fallback to repo-bundled script (useful in dev).
bundled_dictation_script(Path) :-
    prolog_load_context(directory, Here),
    directory_file_path(Here, '..', Root),
    directory_file_path(Root, 'scripts', ScriptsDir),
    directory_file_path(ScriptsDir, 'zara_dictate.py', Path).

start_dictation :-
    dictation_pidfile(PIDFILE),
    ( exists_file(PIDFILE) ->
        format("Dictation already running (pidfile ~w). Use stop_dictation/0 first.~n", [PIDFILE])
    ;
        dictation_logfile(LOGFILE),
        dictation_command_string(Command0),
        (   Command0 == "zara-dictate",
            \+ exists_in_path('zara-dictate'),
            bundled_dictation_script(ScriptPath),
            exists_file(ScriptPath)
        ->  format(string(Command), "python3 ~w", [ScriptPath])
        ;   Command = Command0
        ),
        atomic_list_concat([
            'nohup ',
            Command,
            ' >> ', LOGFILE,
            ' 2>&1 & echo $! > ', PIDFILE
        ], Cmd),
        process_create(path(sh), ['-c', Cmd], [detached(true)]),
        sleep(1),
        ( exists_file(PIDFILE) ->
            read_file_to_string(PIDFILE, S, []),
            string_trim(S, PIDStr),
            format("Dictation started, pid: ~w~n", [PIDStr])
        ;
            format("Failed to start dictation. See ~w~n", [LOGFILE])
        )
    ).

exists_in_path(Cmd) :-
    format(string(Check), 'command -v ~w >/dev/null 2>&1', [Cmd]),
    catch(shell(Check, 0), _, fail).

string_trim(In, Out) :-
    normalize_space(string(Out), In).

stop_dictation :-
    dictation_pidfile(PIDFILE),
    ( exists_file(PIDFILE) ->
        catch(read_file_to_string(PIDFILE, S, []), _, S = ""),
        string_trim(S, PIDStr),
        ( PIDStr == "" ->
            format("Pidfile ~w is empty. Removing it.~n", [PIDFILE]),
            delete_file(PIDFILE)
        ;
            atomic_list_concat(['kill ', PIDStr], KillCmd),
            (   catch(process_create(path(sh), ['-c', KillCmd], []), _, fail)
            ->  true
            ;   format("Warning: failed to kill pid ~w (already dead?)~n", [PIDStr])
            ),
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
