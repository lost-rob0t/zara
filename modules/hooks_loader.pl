:- module(hooks_loader, [
    load_user_hooks/0,
    reload_user_hooks/0,
    user_hooks_path/1,
    set_hook_policy/2,
    reset_hook_policy/0,
    hook_policy/2
]).

:- use_module('config_loader.pl', [user_config_path/1]).
:- use_module('zara_hooks.pl').

:- dynamic hook_policy_state/2.

user_hooks_path(Path) :-
    user_config_path(ConfigPath),
    file_directory_name(ConfigPath, ConfigDir),
    directory_file_path(ConfigDir, 'hooks.pl', Path).

hook_policy(Enabled, AllowOverride) :-
    ( hook_policy_state(CurrentEnabled, CurrentAllowOverride)
    -> Enabled = CurrentEnabled,
       AllowOverride = CurrentAllowOverride
    ; Enabled = false,
      AllowOverride = false
    ).

set_hook_policy(Enabled, AllowOverride) :-
    validate_hook_policy(Enabled, AllowOverride),
    with_mutex(zara_hook_policy,
        ( retractall(hook_policy_state(_, _)),
          assertz(hook_policy_state(Enabled, AllowOverride))
        )),
    ( Enabled == false
    -> zara_hooks:clear_hook_owner(user)
    ; true
    ).

reset_hook_policy :-
    with_mutex(zara_hook_policy,
        retractall(hook_policy_state(_, _))),
    zara_hooks:clear_hook_owner(user).

load_user_hooks :-
    reload_user_hooks.

reload_user_hooks :-
    zara_hooks:clear_hook_owner(user),
    hook_policy(Enabled, _),
    ( Enabled == true
    -> load_enabled_user_hooks
    ; true
    ).

load_enabled_user_hooks :-
    user_hooks_path(Path),
    ( exists_file(Path)
    -> load_files(Path, [silent(true), if(true)])
    ; true
    ).

validate_hook_policy(Enabled, AllowOverride) :-
    validate_boolean(Enabled),
    validate_boolean(AllowOverride),
    ( AllowOverride == true,
      Enabled == false
    -> throw(error(permission_error(enable, hook_override, hooks_disabled), _))
    ; true
    ).

validate_boolean(Value) :-
    ( memberchk(Value, [true, false])
    -> true
    ; throw(error(type_error(boolean, Value), _))
    ).
