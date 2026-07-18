:- module(dictation, [start_dictation/0, stop_dictation/0, dictation_status/0,
                      dictation_active/0]).

:- use_module(library(process)).
:- use_module(library(filesex)).
:- use_module(library(system)).
:- use_module('../kb/config').

dictation_pidfile(Path) :-
    ( getenv('ZARA_DICTATION_PIDFILE', Configured), Configured \= ''
    -> Path = Configured
    ; Path = '/tmp/zara_dictation.pid'
    ).

dictation_logfile(Path) :-
    ( getenv('ZARA_DICTATION_LOGFILE', Configured), Configured \= ''
    -> Path = Configured
    ; Path = '/tmp/zara_dictation.log'
    ).

dictation_command_string(Command) :-
    ( once(kb_config:dictation_command(Configured))
    -> Command = Configured
    ; Command = "zara-dictate"
    ).

bundled_dictation_script(Path) :-
    prolog_load_context(directory, Here),
    directory_file_path(Here, '..', Root),
    directory_file_path(Root, 'scripts', ScriptsDir),
    directory_file_path(ScriptsDir, 'zara_dictate.py', Path).

start_dictation :-
    ( live_dictation_process(Pid)
    -> format("Dictation already running with pid ~w.~n", [Pid])
    ; cleanup_pidfile,
      launch_dictation(Pid),
      sleep(0.1),
      ( process_alive(Pid)
      -> format("Dictation started, pid: ~w~n", [Pid])
      ; cleanup_pidfile,
        fail
      )
    ).

launch_dictation(Pid) :-
    dictation_command_string(Command0),
    ( Command0 == "zara-dictate",
      \+ exists_in_path('zara-dictate'),
      bundled_dictation_script(ScriptPath),
      exists_file(ScriptPath)
    -> format(string(Command), "python3 ~w", [ScriptPath])
    ; Command = Command0
    ),
    dictation_logfile(LogPath),
    setup_call_cleanup(
        open(LogPath, append, Log),
        process_create(path(sh), ['-c', Command],
                       [detached(true), stdout(stream(Log)), stderr(stream(Log)),
                        process(Pid)]),
        close(Log)
    ),
    write_pidfile(Pid).

stop_dictation :-
    ( live_dictation_process(Pid)
    -> catch(process_kill(Pid, term), _, true)
    ; true
    ),
    cleanup_pidfile.

dictation_status :-
    ( live_dictation_process(Pid)
    -> format("Dictation running with pid ~w.~n", [Pid])
    ; writeln("Dictation not running.")
    ).

dictation_active :-
    live_dictation_process(_).

live_dictation_process(Pid) :-
    read_pidfile(Pid),
    ( process_alive(Pid)
    -> true
    ; cleanup_pidfile,
      fail
    ).

read_pidfile(Pid) :-
    dictation_pidfile(Path),
    catch(read_file_to_string(Path, Contents, []), _, fail),
    normalize_space(string(PidText), Contents),
    catch(number_string(Pid, PidText), _, fail),
    integer(Pid),
    Pid > 0,
    !.
read_pidfile(_) :-
    cleanup_pidfile,
    fail.

write_pidfile(Pid) :-
    dictation_pidfile(Path),
    setup_call_cleanup(
        open(Path, write, Stream),
        format(Stream, '~d', [Pid]),
        close(Stream)
    ).

cleanup_pidfile :-
    dictation_pidfile(Path),
    ( exists_file(Path) -> catch(delete_file(Path), _, true) ; true ).

process_alive(Pid) :-
    format(atom(StatusPath), '/proc/~d/status', [Pid]),
    catch(read_file_to_string(StatusPath, Status, []), _, fail),
    \+ sub_string(Status, _, _, _, "State:\tZ").

exists_in_path(Command) :-
    absolute_file_name(path(Command), _, [access(execute), file_errors(fail)]).
