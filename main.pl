% Main entry point with config loading
% ====================================

:- use_module('modules/parser').
:- use_module('modules/commands').
:- use_module('kb/config').      % This automatically loads user config via initialization
:- use_module('kb/intents').
:- use_module('modules/config_loader').
:- discontiguous handle_input/1.

main :-
    writeln('Thus spoke Zarathustra...'),
    show_config_status,
    repl.

% Show user if custom config is loaded
show_config_status :-
    config_loader:user_config_path(Path),
    ( exists_file(Path)
    -> format('✓ User config loaded: ~w~n~n', [Path])
    ; format('⚠ Using defaults (no user config at ~w)~n~n', [Path])
    ).

repl :-
    write('> '), flush_output,
    read_line_to_string(user_input, Input),
    (   Input == "quit" ; Input == end_of_file
    ->  writeln('The prophet has spoken.')
    ;   catch(handle_input(Input),
              E, (print_message(error, E))),
        repl
    ).

handle_input(Input) :-
    catch(
        (parser:handle_command(Input), !),
        Error,
        format('Error: ~w~n', [Error])
    ).

% Special commands for config management
handle_input("reload_config") :-
    config_loader:reload_user_config,
    writeln('Configuration reloaded.'), !.

handle_input("show_config") :-
    show_config, !.

handle_input("help") :-
    show_help, !.

% Helper predicates
show_help :-
    writeln(''),
    writeln('Special commands:'),
    writeln('  help           - Show this help message'),
    writeln('  reload_config  - Reload user configuration'),
    writeln('  show_config    - Display current configuration'),
    writeln('  quit           - Exit'),
    writeln('').


show_config :-
    writeln('Current configuration:'),
    writeln(''),
    findall(Name-Cmd, app_mapping(Name, Cmd), Apps),
    ( Apps = []
    -> writeln('  No app mappings defined.')
    ; writeln('  App Mappings:'),
      forall(member(N-C, Apps),
             format('    ~w -> ~w~n', [N, C]))
    ),
    writeln(''),
    findall(App, direct_app(App), DirectApps),
    ( DirectApps = []
    -> writeln('  No direct apps defined.')
    ; writeln('  Direct Apps:'),
      forall(member(A, DirectApps),
             format('    ~w~n', [A]))
    ),
    writeln(''),
    ( todo_destination(Path)
    -> format('  TODO destination: ~w~n', [Path])
    ; writeln('  TODO destination: (not set)')
    ),
    ( todo_context_mode(Mode)
    -> format('  TODO context mode: ~w~n', [Mode])
    ; writeln('  TODO context mode: (not set)')
    ),
    writeln(''), !.

:- initialization(main, main).
