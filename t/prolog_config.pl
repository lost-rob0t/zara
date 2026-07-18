:- begin_tests(prolog_config).

:- use_module('../kb/intents').
:- use_module('../kb/config').

write_config(Path, Text) :-
    setup_call_cleanup(open(Path, write, Stream), write(Stream, Text), close(Stream)).

test(all_supported_overrides_and_reload) :-
    config_loader:user_config_path(Path),
    atomics_to_string([
        'app_mapping(github, "custom-browser").',
        'direct_app(custom_app).',
        'search_engine("https://example.test/?q=~w").',
        'dictation_command("custom-dictate").',
        'timer_sound("/tmp/custom-timer.wav").',
        'alarm_sound("/tmp/custom-alarm.wav").',
        'llm_provider(openai).',
        'llm_model("custom-model").',
        'llm_endpoint("https://llm.example.test").',
        'verb_intent(hello, open, 1).'
    ], '\n', Config),
    write_config(Path, Config),
    config_loader:reload_user_config,
    once(kb_config:app_mapping(github, "custom-browser")),
    once(kb_config:direct_app(custom_app)),
    once(kb_config:search_engine("https://example.test/?q=~w")),
    once(kb_config:dictation_command("custom-dictate")),
    once(kb_config:timer_sound("/tmp/custom-timer.wav")),
    once(kb_config:alarm_sound("/tmp/custom-alarm.wav")),
    once(kb_config:llm_provider(openai)),
    once(kb_config:llm_model("custom-model")),
    once(kb_config:llm_endpoint("https://llm.example.test")),
    once(kb_intents:verb_intent(hello, open, 1)),
    write_config(Path, 'app_mapping(github, "new-browser").\n'),
    config_loader:reload_user_config,
    once(kb_config:app_mapping(github, "new-browser")),
    \+ kb_config:app_mapping(github, "custom-browser"),
    findall(Command, kb_config:app_mapping(github, Command), Commands),
    Commands = ["new-browser", "xdg-open https://github.com"].

test(unsafe_declaration_is_rejected,
     [throws(error(domain_error(zarathushtra_user_config_fact, _), _))]) :-
    config_loader:user_config_path(Path),
    write_config(Path, ':- initialization(shell("false")).\n'),
    config_loader:reload_user_config.

:- end_tests(prolog_config).
