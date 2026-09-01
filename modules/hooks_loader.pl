:- module(hooks_loader, [
    load_user_hooks/0,
    reload_user_hooks/0,
    user_hooks_path/1
]).

:- use_module('config_loader.pl', [user_config_path/1]).
:- use_module('zara_hooks.pl').

user_hooks_path(Path) :-
    user_config_path(ConfigPath),
    file_directory_name(ConfigPath, ConfigDir),
    directory_file_path(ConfigDir, 'hooks.pl', Path).

load_user_hooks :-
    reload_user_hooks.

reload_user_hooks :-
    zara_hooks:clear_hook_owner(user),
    user_hooks_path(Path),
    ( exists_file(Path)
    -> load_files(Path, [silent(true), if(true)])
    ; true
    ).
