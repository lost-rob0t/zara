% top of file
:- module(commands, [execute/2]).
:- use_module('../kb/config').
:- use_module(library(process)).
:- use_module('normalizer', [strip_fillers/2]).  % <- FIXED PATH
:- use_module(library(http/http_client)).
                                % :- use_module('todo_capture').                   % Broken
:- use_module(alarm).

% ============================================
% Execution Layer
% ============================================
execute(greet, []) :-
    format('Hello! How can I help you?~n'), !.

execute(play, [Media]) :-
    format('Playing: ~w~n', [Media]), !.

execute(pause, []) :-
    format('Pausing playback~n'), !.

execute(call, [Contact]) :-
    format('Calling: ~w~n', [Contact]), !.

execute(text, [Contact, Message]) :-
    format('Texting ~w: ~w~n', [Contact, Message]), !.

execute(open, [AppName]) :-
    open_app(AppName), !.

execute(search, Args) :-
    atomic_list_concat(Args, ' ', Query),
    search_url(Query, URL),
    format('Searching for: ~w~n', [Query]),
    format(atom(Cmd), 'xdg-open "~w"', [URL]),
    shell(Cmd), !.

execute(ask, Args) :-
    atomic_list_concat(Args, ' ', Query),
    format('Chat history not yet implemented. Query was: ~w~n', [Query]), !.

% execute(ask, Args) :-
%     atomic_list_concat(Args, ' ', Query),
%     catch(
%         (llm_client:llm_query(Query, Response),
%          format('~n~w~n~n', [Response])),
%         Error,
%         format('LLM Error: ~w~n', [Error])
%     ), !.

% % TODO creation (no required date)
% execute(todo, Args) :-
%     atomic_list_concat(Args, ' ', Task),
%     format('📝 Adding TODO: ~w~n', [Task]),
%     todo_capture:capture_todo(Task), !.

% % Reminder/Schedule (requires date prompt if missing)
% execute(reminder, Args) :-
%     atomic_list_concat(Args, ' ', Task),
%     format('⏰ Setting Reminder: ~w~n', [Task]),
%     todo_capture:capture_todo(Task), !.

execute(schedule, Args) :-
    delete(Args, reminder, Args1),
    strip_fillers(Args1, Core),
    ( Core = [] -> TaskStr = "unspecified task"
    ; tokens_to_string(Core, TaskStr)
    ),
    format('⏰ Scheduling: ~w~n', [TaskStr]),
    todo_capture:capture_todo(TaskStr), !.



%%======================
%% Named Timers
%%======================

execute(timer, [_, Seconds, second, Name]) :-
    alarm:start_timer(Name, Seconds),
    format('⏳ Timer "~w" set for ~w second~n', [Name, Seconds]),
    !.

execute(timer, [_, Seconds, seconds, Name]) :-
    alarm:start_timer(Name, Seconds),
    format('⏳ Timer "~w" set for ~w seconds~n', [Name, Seconds]),
    !.

execute(timer, [_, Minute, minute, Name]) :-
    Seconds is Minute * 60,
    alarm:start_timer(Name, Seconds),
    format('⏳ Timer "~w" set for ~w minute (~w seconds)~n', [Name, Minute, Seconds]),
    !.

execute(timer, [_, Minutes, minutes, Name]) :-
    Seconds is Minutes * 60,
    alarm:start_timer(Name, Seconds),
    format('⏳ Timer "~w" set for ~w minutes (~w seconds)~n', [Name, Minutes, Seconds]),
    !.

execute(timer, [_, Hour, hour, Name]) :-
    Seconds is Hour * 3600,
    alarm:start_timer(Name, Seconds),
    format('⏳ Timer "~w" set for ~w hour (~w seconds)~n', [Name, Hour, Seconds]),
    !.

execute(timer, [_, Hours, hours, Name]) :-
    Seconds is Hours * 3600,
    alarm:start_timer(Name, Seconds),
    format('⏳ Timer "~w" set for ~w hours (~w seconds)~n', [Name, Hours, Seconds]),
    !.


% Timer in second (singular)
execute(timer, [_, Seconds, second]) :-
    alarm:start_timer(Seconds),
    format('⏳ Timer set for ~w second~n', [Seconds]),
    .

% Timer in seconds (plural)
execute(timer, [_, Seconds, seconds]) :-
    alarm:start_timer(Seconds),
    format('⏳ Timer set for ~w seconds~n', [Seconds]),
    .

% Timer in minute (singular)
execute(timer, [_, Minute, minute]) :-
    Seconds is Minute * 60,
    alarm:start_timer(Seconds),
    format('⏳ Timer set for ~w minute (~w seconds)~n', [Minute, Seconds]),
    .

% Timer in minutes (plural)
execute(timer, [_, Minutes, minutes]) :-
    Seconds is Minutes * 60,
    alarm:start_timer(Seconds),
    format('⏳ Timer set for ~w minutes (~w seconds)~n', [Minutes, Seconds]),
    .

execute(timer, [_, Hour, hour]) :-
    Seconds is Hour * 3600,
    alarm:start_timer(Seconds),
    format('⏳ Timer set for ~w hour (~w seconds)~n', [Hour, Seconds]),
    .

execute(timer, [_, Hours, hours]) :-
    Seconds is Hours * 3600,
    alarm:start_timer(Seconds),
    format('⏳ Timer set for ~w hours (~w seconds)~n', [Hours, Seconds]),
    .

execute(say, Rest) :-
    format('Executing ~w with args: ~w~n', [Rest]), !.

execute(Intent, Args) :-
    format('Executing ~w with args: ~w~n', [Intent, Args]), !.

% ============================================
% LLM Integration
% ============================================
llm_query(Query, Response) :-
    % Get the directory of the current script
    current_prolog_flag(argv, [Script|_]),
    file_directory_name(Script, ScriptDir),
    atomic_list_concat([ScriptDir, '/scripts/claude.sh'], ClaudeScript),
    % Escape quotes in the query
    atomic_list_concat(QueryParts, '"', Query),
    atomic_list_concat(QueryParts, '\\"', EscapedQuery),
    % Build the shell command
    atomic_list_concat([ClaudeScript, ' "', EscapedQuery, '"'], Command),
    % Execute and capture output
    setup_call_cleanup(
        open(pipe(Command), read, Stream),
        read_stream_to_codes(Stream, Codes),
        close(Stream)
    ),
    atom_codes(Response, Codes).

% ============================================
% App Opening System
% ============================================
% Main app opening logic
open_app(AppName) :-
    (   kb_config:app_mapping(AppName, Command)
    ->  format('Opening ~w via: ~w~n', [AppName, Command]),
        run_system_command(Command)
    ;   kb_config:direct_app(AppName)
    ->  format('Launching ~w directly~n', [AppName]),
        atom_string(AppName, AppCmd),
        run_system_command(AppCmd)
    ;   format('Attempting to launch ~w (not in config)~n', [AppName]),
        atom_string(AppName, AppCmd),
        run_system_command(AppCmd)
    ),
    !.

% Execute system command using process_create with shell wrapper
run_system_command(Command) :-
    catch(
        process_create('/bin/sh', ['-c', Command], [
            detached(true),
            stdout(null),
            stderr(null),
            process(_)
        ]),
        Error,
        format('Failed to launch: ~w~n', [Error])
    ), !.

tokens_to_string(Toks, S) :-
    maplist(atom_string, Toks, Parts),
    atomic_list_concat(Parts, ' ', S).

handle_command("set a timer for 1 minute for tea").
