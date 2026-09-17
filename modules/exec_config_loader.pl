:- module(exec_config_loader,
    [
        load_user_exec_config/0,
        reload_user_exec_config/0,
        user_exec_config_path/1
    ]).

:- use_module(library(filesex)).

%% user_exec_config_path(-Path) is det.
%
%  Trusted desktop-only executable Prolog configuration. Unlike config.pl
%  and config.local.pl this file is intentionally not parsed through a fact
%  allowlist: loading it has the full authority of the Zara Prolog process.
user_exec_config_path(Path) :-
    user_exec_config_dir(Dir),
    directory_file_path(Dir, 'config.exec.pl', Path).

user_exec_config_dir(Dir) :-
    ( getenv('XDG_CONFIG_HOME', Xdg), Xdg \= ''
    -> directory_file_path(Xdg, 'zarathushtra', Dir)
    ; expand_file_name('~/.config/zarathushtra', [Dir])
    ).

%% load_user_exec_config is det.
%
%  Load trusted user code when present. The file is never auto-created and
%  is deliberately excluded from server_main.pl.
load_user_exec_config :-
    user_exec_config_path(Path),
    ( exists_file(Path)
    -> load_exec_file(Path)
    ; true
    ).

%% reload_user_exec_config is det.
%
%  Reconsult and re-run directives even when the file timestamp did not
%  change. Side effects performed by user code remain the user's responsibility.
reload_user_exec_config :-
    user_exec_config_path(Path),
    ( exists_file(Path)
    -> load_exec_file(Path),
       format('Executable Prolog config reloaded: ~w~n', [Path])
    ; format('No executable Prolog config at ~w~n', [Path])
    ).

load_exec_file(Path) :-
    load_files(user:Path, [silent(true), if(true)]).
