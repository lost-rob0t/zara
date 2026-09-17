:- module(policy_config, [setting/2, default_setting/2, user_setting/2]).
:- use_module(library(error)).
:- multifile user_setting/2.
:- dynamic user_setting/2.

default_setting(enabled, true).
default_setting(mode, advise).
default_setting(max_input_chars, 32768).
default_setting(max_findings, 16).
default_setting(max_revisions, 1).
default_setting(revision_timeout_seconds, 10).
default_setting(disabled_rules, []).

setting(Key, Value) :-
    findall(Override, user_setting(Key, Override), Overrides),
    ( Overrides = [] -> default_setting(Key, Value)
    ; Overrides = [Value] -> true
    ; throw(error(domain_error(unique_policy_setting, Key), _))
    ).
