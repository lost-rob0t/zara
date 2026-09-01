:- begin_tests(prolog_config).

:- use_module('../kb/intents').
:- use_module('../kb/config').
:- use_module('../kb/device_providers').
:- use_module('../modules/config_loader').

write_config(Path, Text) :-
    setup_call_cleanup(open(Path, write, Stream), write(Stream, Text), close(Stream)).

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

test(prolog_rlm_facts_are_accepted_and_validated) :-
    config_loader:user_config_path(Path),
    write_config(Path,
        'prolog_rlm_enabled(true).\nprolog_rlm_model("openrouter/free").\n'),
    config_loader:reload_user_config,
    once(kb_config:prolog_rlm_enabled(true)),
    once(kb_config:prolog_rlm_model("openrouter/free")).

test(invalid_prolog_rlm_facts_are_rejected,
     [ forall(invalid_prolog_rlm_config(Config)),
       throws(error(domain_error(zarathushtra_user_config_fact, _), _))
     ]) :-
    config_loader:user_config_path(Path),
    write_config(Path, Config),
    config_loader:reload_user_config.

invalid_prolog_rlm_config('prolog_rlm_enabled("yes").\n').
invalid_prolog_rlm_config('prolog_rlm_enabled(1).\n').
invalid_prolog_rlm_config('prolog_rlm_model(12345).\n').

:- end_tests(prolog_config).
