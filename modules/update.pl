:- module(update, [execute/2]).

:- use_module(library(process)).
:- use_module(library(readutil)).

execute(update, []) :-
    format('Starting system update in background...~n'),
    thread_create(update_worker, ThreadId, []),
    format('Update thread ~w started~n', [ThreadId]).

% Background worker thread
update_worker :-
    catch(
        (get_system_info(OS, HasHomeManager, HasFlake),
         run_system_update(OS, HasHomeManager, HasFlake)),
        Error,
        format('Update error: ~w~n', [Error])
    ).

% Get system information
get_system_info(OS, HasHomeManager, HasFlake) :-
    detect_os(OS),
    check_home_manager(HasHomeManager),
    check_flake(HasFlake),
    format('System: ~w, HomeManager: ~w, Flake: ~w~n', [OS, HasHomeManager, HasFlake]).

% Detect operating system
detect_os(nixos) :-
    exists_file('/etc/nixos/configuration.nix'), !.
detect_os(linux) :-
    exists_file('/etc/os-release'), !.
detect_os(macos) :-
    exists_file('/System/Library/CoreServices/SystemVersion.plist'), !.
detect_os(unknown).

% Check if home-manager is available
check_home_manager(true) :-
    shell('command -v home-manager >/dev/null 2>&1', 0), !.
check_home_manager(false).

% Check if flake.nix exists
check_flake(true) :-
    (exists_file('./flake.nix') ;
     exists_file('~/.dotfiles/flake.nix') ;
     exists_file('~/.config/nixos/flake.nix') ;
     exists_file('/etc/nixos/flake.nix')), !.
check_flake(false).

% Run updates with progress reporting
run_system_update(nixos, HasHomeManager, HasFlake) :-
    format('[UPDATE] Detected NixOS system~n'),
    run_nixos_update(HasFlake),
    (HasHomeManager = true -> run_home_manager_update ; true),
    format('[UPDATE] NixOS update sequence completed~n').

run_system_update(linux, HasHomeManager, _) :-
    format('[UPDATE] Detected Linux system~n'),
    run_linux_update,
    (HasHomeManager = true -> run_home_manager_update ; true),
    format('[UPDATE] Linux update sequence completed~n').

run_system_update(macos, HasHomeManager, _) :-
    format('[UPDATE] Detected macOS system~n'),
    run_macos_update,
    (HasHomeManager = true -> run_home_manager_update ; true),
    format('[UPDATE] macOS update sequence completed~n').

run_system_update(unknown, _, _) :-
    format('[UPDATE] Unknown system - no update performed~n').

% NixOS updates with async execution
run_nixos_update(true) :-
    format('[UPDATE] Running nixos-rebuild switch with flake...~n'),
    find_flake_path(FlakePath),
    atomic_list_concat(['sudo nixos-rebuild switch --flake ', FlakePath, ' 2>&1'], Command),
    run_async_command(Command, 'NixOS rebuild').

run_nixos_update(false) :-
    format('[UPDATE] Running nixos-rebuild switch...~n'),
    run_async_command('sudo nixos-rebuild switch 2>&1', 'NixOS rebuild').

% Find flake path
find_flake_path('./') :-
    exists_file('./flake.nix'), !.
find_flake_path('~/.config/nixos/') :-
    exists_file('~/.config/nixos/flake.nix'), !.
find_flake_path('/etc/nixos/') :-
    exists_file('/etc/nixos/flake.nix'), !.
find_flake_path('./').

% Home Manager update
run_home_manager_update :-
    format('[UPDATE] Running home-manager switch...~n'),
    run_async_command('home-manager switch 2>&1', 'Home Manager switch').

% Other system updates
run_linux_update :-
    format('[UPDATE] Running system package update...~n'),
    (shell('command -v apt >/dev/null 2>&1', 0) ->
        run_async_command('sudo apt update && sudo apt upgrade -y 2>&1', 'APT update') ;
    shell('command -v pacman >/dev/null 2>&1', 0) ->
        run_async_command('sudo pacman -Syu --noconfirm 2>&1', 'Pacman update') ;
    shell('command -v dnf >/dev/null 2>&1', 0) ->
        run_async_command('sudo dnf upgrade -y 2>&1', 'DNF update') ;
        format('[UPDATE] No supported package manager found~n')).

run_macos_update :-
    format('[UPDATE] Running macOS updates...~n'),
    (shell('command -v brew >/dev/null 2>&1', 0) ->
        run_async_command('brew update && brew upgrade 2>&1', 'Homebrew update') ;
        format('[UPDATE] Homebrew not found~n')).

% Generic async command runner with output streaming
run_async_command(Command, Description) :-
    catch(
        (setup_call_cleanup(
            open(pipe(Command), read, Stream),
            stream_output(Stream, Description),
            close(Stream)
        )),
        Error,
        format('[UPDATE] ~w failed: ~w~n', [Description, Error])
    ).
:- discontiguous execute/2. % is this bad?

% Stream command output line by line
stream_output(Stream, Description) :-
    repeat,
    (at_end_of_stream(Stream) ->
        (format('[UPDATE] ~w completed~n', [Description]), !) ;
        (read_line_to_string(Stream, Line),
         format('[UPDATE] ~w: ~s~n', [Description, Line]),
         fail)
    ).

% Optional: Check update status
execute(update_status, []) :-
    findall(ThreadId,
        (thread_property(ThreadId, status(_)),
         thread_property(ThreadId, alias(update_worker))),
        Threads),
    (Threads = [] ->
        format('No update threads running~n') ;
        format('Update threads: ~w~n', [Threads])).
