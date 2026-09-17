% Main entry point with config loading
% ====================================

:- use_module('modules/command_loop').
:- use_module('modules/commands').
:- use_module('modules/todo_schedule').
:- use_module('modules/market_data').
:- use_module('kb/config').          % shared semantic configuration
:- use_module('kb/device_providers'). % Linux device provider configuration (desktop only)
:- use_module('kb/intents').
:- use_module('modules/config_loader').
:- use_module('modules/exec_config_loader').
:- use_module('modules/hooks_loader').
:- discontiguous handle_input/1.

:- initialization(config_loader:load_user_config).
:- initialization(exec_config_loader:load_user_exec_config).
:- initialization(hooks_loader:load_user_hooks).

main :-
    writeln('Thus spoke Zarathustra...'),
    show_config_status,
    repl.

% Show user if custom config is loaded
show_config_status :-
    config_loader:user_config_path(Path),
    ( exists_file(Path)
    -> format('✓ User config loaded: ~w~n', [Path])
    ; format('⚠ Using defaults (no user config at ~w)~n', [Path])
    ),
    exec_config_loader:user_exec_config_path(ExecPath),
    ( exists_file(ExecPath)
    -> format('✓ Executable Prolog config loaded: ~w~n~n', [ExecPath])
    ; format('  No executable Prolog config at ~w~n~n', [ExecPath])
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
        (command_loop:handle_command(Input), !),
        Error,
        format('Error: ~w~n', [Error])
    ).

% Special commands for config management
handle_input("reload_config") :-
    config_loader:reload_user_config,
    exec_config_loader:reload_user_exec_config,
    hooks_loader:reload_user_hooks,
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
    writeln('  reload_config  - Reload user + executable Prolog configuration'),
    writeln('  show_config    - Display current configuration'),
    writeln('  quit           - Exit'),
    writeln('').


show_config :-
    writeln('Current configuration:'),
    writeln(''),
    findall(Name-Cmd, kb_device_providers:app_mapping(Name, Cmd), Apps),
    ( Apps = []
    -> writeln('  No app mappings defined.')
    ; writeln('  App Mappings:'),
      forall(member(N-C, Apps),
             format('    ~w -> ~w~n', [N, C]))
    ),
    writeln(''),
    findall(App, kb_device_providers:direct_app(App), DirectApps),
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
    market_data:current_market_provider(MarketProvider),
    format('  Market provider: ~w~n', [MarketProvider]),
    writeln(''), !.

:- initialization(main, main).
