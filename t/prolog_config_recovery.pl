:- begin_tests(prolog_config_recovery).

:- use_module('../kb/config').
:- use_module('../modules/config_loader').

write_recovery_config(Path, Text) :-
    setup_call_cleanup(open(Path, write, Stream), write(Stream, Text), close(Stream)).

remove_recovery_config(Path) :-
    ( exists_file(Path) -> delete_file(Path) ; true ).

test(default_search_engine_is_brave) :-
    once(kb_config:search_engine("https://search.brave.com/search?q=~w")).

test(dead_prolog_rlm_predicates_are_absent) :-
    \+ current_predicate(kb_config:prolog_rlm_enabled/1),
    \+ current_predicate(kb_config:prolog_rlm_model/1).

test(dead_prolog_rlm_facts_fail_closed,
     [ forall(dead_prolog_rlm_config(Config)),
       throws(error(domain_error(zarathushtra_user_config_fact, _), _))
     ]) :-
    config_loader:user_config_path(BasePath),
    config_loader:user_local_config_path(LocalPath),
    remove_recovery_config(LocalPath),
    write_recovery_config(BasePath, Config),
    config_loader:reload_user_config.

dead_prolog_rlm_config('prolog_rlm_enabled(true).\n').
dead_prolog_rlm_config('prolog_rlm_model("openrouter/free").\n').

test(generated_base_config_teaches_private_local_overlay_without_rlm) :-
    config_loader:user_config_path(BasePath),
    config_loader:user_local_config_path(LocalPath),
    remove_recovery_config(BasePath),
    remove_recovery_config(LocalPath),
    config_loader:ensure_user_config,
    read_file_to_string(BasePath, Text, []),
    sub_string(Text, _, _, _, "config.local.pl"),
    \+ sub_string(Text, _, _, _, "prolog_rlm_").

test(local_overlay_still_overrides_base_without_rewrite) :-
    config_loader:user_config_path(BasePath),
    config_loader:user_local_config_path(LocalPath),
    Base = 'search_engine("https://base.example/search?q=~w").\n',
    Local = 'search_engine("https://local.example/search?q=~w").\n',
    write_recovery_config(BasePath, Base),
    write_recovery_config(LocalPath, Local),
    config_loader:reload_user_config,
    once(kb_config:search_engine("https://local.example/search?q=~w")),
    read_file_to_string(BasePath, BaseAfter, []),
    BaseAfter == Base.

:- end_tests(prolog_config_recovery).
