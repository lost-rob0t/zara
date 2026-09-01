:- module(hooks_loader, [
    load_user_hooks/0,
    reload_user_hooks/0,
    user_hooks_path/1,
    set_hook_policy/2,
    reset_hook_policy/0,
    hook_policy/2
]).

:- use_module(library(readutil)).
:- use_module('config_loader.pl', [user_config_path/1]).
:- use_module('zara_hooks.pl').

:- dynamic hook_policy_state/2.

user_hooks_path(Path) :-
    user_config_path(ConfigPath),
    file_directory_name(ConfigPath, ConfigDir),
    directory_file_path(ConfigDir, 'hooks.pl', Path).

hook_config_path(Path) :-
    user_config_path(ConfigPath),
    file_directory_name(ConfigPath, ConfigDir),
    directory_file_path(ConfigDir, 'config.toml', Path).

hook_policy(Enabled, AllowOverride) :-
    ( hook_policy_state(CurrentEnabled, CurrentAllowOverride)
    -> Enabled = CurrentEnabled,
       AllowOverride = CurrentAllowOverride
    ; Enabled = false,
      AllowOverride = false
    ).

set_hook_policy(Enabled, AllowOverride) :-
    validate_boolean(Enabled),
    validate_boolean(AllowOverride),
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
    sync_hook_policy_from_config,
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

sync_hook_policy_from_config :-
    hook_config_path(Path),
    ( exists_file(Path)
    -> read_file_to_string(Path, Text, []),
       parse_hook_policy(Text, Enabled, AllowOverride)
    ; Enabled = false,
      AllowOverride = false
    ),
    set_hook_policy(Enabled, AllowOverride).

parse_hook_policy(Text, Enabled, AllowOverride) :-
    split_string(Text, "\n", "\r", Lines),
    hooks_section_lines(Lines, HookLines),
    policy_value(HookLines, "enabled", false, Enabled),
    policy_value(HookLines, "allow_override", false, AllowOverride),
    !.
parse_hook_policy(_, false, false).

hooks_section_lines([], []).
hooks_section_lines([Line|Lines], HookLines) :-
    clean_toml_line(Line, Clean),
    ( Clean == "[hooks]"
    -> take_section_lines(Lines, HookLines)
    ; hooks_section_lines(Lines, HookLines)
    ).

take_section_lines([], []).
take_section_lines([Line|Lines], HookLines) :-
    clean_toml_line(Line, Clean),
    ( toml_section_header(Clean)
    -> HookLines = []
    ; HookLines = [Line|Rest],
      take_section_lines(Lines, Rest)
    ).

toml_section_header(Line) :-
    sub_string(Line, 0, 1, _, "[").

policy_value(Lines, Key, Default, Value) :-
    findall(Found,
            ( member(Line, Lines),
              hook_boolean_assignment(Line, Key, Found)
            ),
            Values),
    unique_policy_value(Values, Default, Value).

unique_policy_value([], Default, Default).
unique_policy_value([Value], _, Value).

hook_boolean_assignment(Line, Key, Value) :-
    clean_toml_line(Line, Clean),
    Clean \= "",
    split_string(Clean, "=", " \t", Parts),
    Parts = [RawKey, RawValue],
    normalize_space(string(NormalizedKey), RawKey),
    normalize_space(string(NormalizedValue), RawValue),
    NormalizedKey == Key,
    toml_boolean(NormalizedValue, Value).

toml_boolean("true", true).
toml_boolean("false", false).

clean_toml_line(Line, Clean) :-
    split_string(Line, "#", "", Parts),
    Parts = [BeforeComment|_],
    normalize_space(string(Clean), BeforeComment).

validate_boolean(Value) :-
    ( memberchk(Value, [true, false])
    -> true
    ; throw(error(type_error(boolean, Value), _))
    ).
