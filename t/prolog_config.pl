:- begin_tests(prolog_config).

:- use_module('../kb/intents').
:- use_module('../kb/config').
:- use_module('../kb/device_providers').
:- use_module('../modules/config_loader').

write_config(Path, Text) :-
    setup_call_cleanup(open(Path, write, Stream), write(Stream, Text), close(Stream)).

remove_config(Path) :-
    ( exists_file(Path) -> delete_file(Path) ; true ).

test(all_supported_overrides_and_reload) :-
    config_loader:user_config_path(Path),
    atomics_to_string([
        'app_mapping(github, ["custom-browser", "--new-window"]).',
        'direct_app(custom_app).',
        'search_engine("https://example.test/?q=~w").',
        'dictation_command("custom-dictate").',
        'timer_sound(disabled).',
        'alarm_sound("/tmp/custom-alarm.wav").',
        'llm_provider(openai).',
        'llm_model("custom-model").',
        'llm_endpoint("https://llm.example.test").',
        'verb_intent(hello, open, 1).'
    ], '\n', Config),
    write_config(Path, Config),
    config_loader:reload_user_config,
    once(kb_device_providers:app_mapping(github, ["custom-browser", "--new-window"])),
    once(kb_device_providers:direct_app(custom_app)),
    once(kb_config:search_engine("https://example.test/?q=~w")),
    once(kb_device_providers:dictation_command("custom-dictate")),
    once(kb_device_providers:timer_sound(disabled)),
    once(kb_device_providers:alarm_sound("/tmp/custom-alarm.wav")),
    once(kb_config:llm_provider(openai)),
    once(kb_config:llm_model("custom-model")),
    once(kb_config:llm_endpoint("https://llm.example.test")),
    once(kb_intents:verb_intent(hello, open, 1)),
    write_config(Path, 'app_mapping(github, "new-browser").\n'),
    config_loader:reload_user_config,
    once(kb_device_providers:app_mapping(github, "new-browser")),
    \+ kb_device_providers:app_mapping(github, ["custom-browser", "--new-window"]),
    findall(Command, kb_device_providers:app_mapping(github, Command), Commands),
    Commands = ["new-browser", "xdg-open https://github.com"].

test(device_facts_never_land_in_shared_semantic_config) :-
    config_loader:user_config_path(Path),
    write_config(Path,
        'app_mapping(split_app, "split-browser").\ntimer_sound(disabled).\n'),
    config_loader:reload_user_config,
    once(kb_device_providers:app_mapping(split_app, "split-browser")),
    \+ current_predicate(kb_config:app_mapping/2),
    \+ current_predicate(kb_config:timer_sound/1).

test(unsafe_declaration_is_rejected,
     [throws(error(domain_error(zarathushtra_user_config_fact, _), _))]) :-
    config_loader:user_config_path(Path),
    write_config(Path, ':- initialization(shell("false")).\n'),
    config_loader:reload_user_config.

test(unsafe_command_string_is_rejected,
     [throws(error(domain_error(zarathushtra_user_config_fact, _), _))]) :-
    config_loader:user_config_path(Path),
    write_config(Path, 'app_mapping(browser, "safe; touch marker").\n'),
    config_loader:reload_user_config.

test(invalid_sound_setting_is_rejected,
     [ forall(invalid_sound_config(Config)),
       throws(error(domain_error(zarathushtra_user_config_fact, _), _))
     ]) :-
    config_loader:user_config_path(Path),
    write_config(Path, Config),
    config_loader:reload_user_config.

invalid_sound_config('timer_sound(false).\n').
invalid_sound_config('timer_sound("").\n').
invalid_sound_config('timer_sound(["tone.wav"]).\n').
invalid_sound_config('timer_sound("disabled").\n').

test(default_wake_words_include_zarathushtra) :-
    once(kb_config:wake_word("zarathushtra")).

test(wake_word_override_is_supported) :-
    config_loader:user_config_path(Path),
    write_config(Path, 'wake_word("jarvis").\n'),
    config_loader:reload_user_config,
    once(kb_config:wake_word("jarvis")).

test(empty_wake_word_is_rejected,
     [throws(error(domain_error(zarathushtra_user_config_fact, _), _))]) :-
    config_loader:user_config_path(Path),
    write_config(Path, 'wake_word("").\n'),
    config_loader:reload_user_config.

test(openrouter_provider_override_is_accepted) :-
    config_loader:user_config_path(Path),
    write_config(Path, 'llm_provider(openrouter).\n'),
    config_loader:reload_user_config,
    once(kb_config:llm_provider(openrouter)).

test(unknown_llm_provider_override_is_rejected,
     [throws(error(domain_error(zarathushtra_user_config_fact, _), _))]) :-
    config_loader:user_config_path(Path),
    write_config(Path, 'llm_provider(gpt4all).\n'),
    config_loader:reload_user_config.

test(local_overlay_wins_without_modifying_provisioned_config) :-
    config_loader:user_config_path(BasePath),
    config_loader:user_local_config_path(LocalPath),
    Base = 'search_engine("https://base.example/?q=~w").\n',
    Local = 'search_engine("https://local.example/?q=~w").\n',
    write_config(BasePath, Base),
    write_config(LocalPath, Local),
    config_loader:reload_user_config,
    once(kb_config:search_engine("https://local.example/?q=~w")),
    read_file_to_codes(BasePath, BaseAfter, []),
    atom_codes(Base, BaseAfter).

test(local_overlay_is_not_auto_created) :-
    config_loader:user_config_path(BasePath),
    config_loader:user_local_config_path(LocalPath),
    write_config(BasePath, 'search_engine("https://base-only.example/?q=~w").\n'),
    remove_config(LocalPath),
    config_loader:load_user_config,
    \+ exists_file(LocalPath),
    once(kb_config:search_engine("https://base-only.example/?q=~w")).

test(local_overlay_reload_replaces_old_value_without_duplicates) :-
    config_loader:user_config_path(BasePath),
    config_loader:user_local_config_path(LocalPath),
    write_config(BasePath, 'search_engine("https://base-reload.example/?q=~w").\n'),
    write_config(LocalPath, 'search_engine("https://local-old.example/?q=~w").\n'),
    config_loader:reload_user_config,
    write_config(LocalPath, 'search_engine("https://local-new.example/?q=~w").\n'),
    config_loader:reload_user_config,
    \+ kb_config:search_engine("https://local-old.example/?q=~w"),
    findall(URL, kb_config:search_engine(URL), URLs),
    URLs = ["https://local-new.example/?q=~w",
            "https://base-reload.example/?q=~w",
            "https://search.brave.com/search?q=~w"].

test(invalid_local_overlay_leaves_previous_loaded_state) :-
    config_loader:user_config_path(BasePath),
    config_loader:user_local_config_path(LocalPath),
    write_config(BasePath, 'search_engine("https://base-atomic.example/?q=~w").\n'),
    write_config(LocalPath, 'search_engine("https://local-valid.example/?q=~w").\n'),
    config_loader:reload_user_config,
    write_config(LocalPath, ':- initialization(shell("false")).\n'),
    catch(config_loader:reload_user_config, Error, true),
    nonvar(Error),
    Error = error(domain_error(zarathushtra_user_config_fact, _), _),
    once(kb_config:search_engine("https://local-valid.example/?q=~w")).

test(server_rejects_device_fact_from_local_overlay,
     [throws(error(domain_error(zarathushtra_server_config_fact, _), _))]) :-
    config_loader:user_config_path(BasePath),
    config_loader:user_local_config_path(LocalPath),
    write_config(BasePath, 'search_engine("https://server-base.example/?q=~w").\n'),
    write_config(LocalPath, 'app_mapping(browser, ["xdg-open"]).\n'),
    config_loader:load_server_config.

:- end_tests(prolog_config).
