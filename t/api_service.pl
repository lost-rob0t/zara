:- begin_tests(api_service).

:- use_module('../server_main').
:- use_module('../modules/config_loader').

write_config(Path, Text) :-
    setup_call_cleanup(open(Path, write, Stream), write(Stream, Text), close(Stream)).

server_boot_loaded :-
    current_predicate(kb_server_providers:api_service_provider/3),
    current_predicate(capability_plans:plan_for_frame/3),
    current_predicate(config_loader:load_server_config/0).

test(server_boot_loads_registry_and_selector_modules) :-
    server_boot_loaded.

test(server_boot_does_not_consult_semantic_defaults_file) :-
    \+ (source_file_property(File, module(kb_config)),
        atom(File),
        atom_concat(_, 'kb/config.pl', File)).

test(server_boot_never_loads_device_provider_configuration) :-
    \+ current_predicate(kb_device_providers:app_mapping/2),
    \+ current_predicate(kb_device_providers:timer_sound/1),
    \+ current_predicate(kb_device_providers:direct_app/1).

test(registry_version_is_current) :-
    once(kb_server_providers:api_service_registry_version(1)).

test(registry_declares_builtin_server_providers) :-
    once(kb_server_providers:api_service_provider(search_server, builtin, _)),
    once(kb_server_providers:api_service_provider(timer_server, builtin, _)),
    once(kb_server_providers:api_service_provider(admin_restart, builtin, _)).

test(registry_has_no_duplicate_provider_ids) :-
    findall(Id, kb_server_providers:api_service_provider(Id, _, _), Ids),
    sort(Ids, Sorted),
    length(Ids, Count),
    length(Sorted, Count).

test(registry_declares_no_desktop_execution_providers) :-
    findall(Id, kb_server_providers:api_service_provider(Id, _, _), Ids),
    \+ memberchk(open_desktop, Ids),
    \+ memberchk(open_app, Ids),
    \+ memberchk(screen_server, Ids).

test(server_scope_accepts_semantic_overrides) :-
    config_loader:user_config_path(Path),
    write_config(Path,
        'search_engine("https://server.example.test/?q=~w").\nllm_model("server-model").\n'),
    config_loader:load_server_config,
    once(kb_config:search_engine("https://server.example.test/?q=~w")),
    once(kb_config:llm_model("server-model")).

test(server_scope_rejects_app_mappings,
     [throws(error(domain_error(zarathushtra_server_config_fact, _), _))]) :-
    config_loader:user_config_path(Path),
    write_config(Path, 'app_mapping(github, "xdg-open https://github.com").\n'),
    config_loader:load_server_config.

test(server_scope_rejects_sound_settings,
     [throws(error(domain_error(zarathushtra_server_config_fact, _), _))]) :-
    config_loader:user_config_path(Path),
    write_config(Path, 'timer_sound("/tmp/tone.wav").\n'),
    config_loader:load_server_config.

test(server_scope_rejects_dictation_commands,
     [throws(error(domain_error(zarathushtra_server_config_fact, _), _))]) :-
    config_loader:user_config_path(Path),
    write_config(Path, 'dictation_command(["zara-dictate"]).\n'),
    config_loader:load_server_config.

test(server_scope_rejects_unknown_facts,
     [throws(error(domain_error(zarathushtra_server_config_fact, _), _))]) :-
    config_loader:user_config_path(Path),
    write_config(Path, 'totally_unknown_fact(1).\n'),
    config_loader:load_server_config.

test(server_scope_accepts_intent_verbs) :-
    config_loader:user_config_path(Path),
    write_config(Path, 'verb_intent(find_stuff, search, rest).\n'),
    config_loader:load_server_config,
    once(kb_intents:verb_intent(find_stuff, search, rest)).

test(server_scope_skips_missing_user_config) :-
    config_loader:user_config_path(Path),
    catch(delete_file(Path), _, true),
    config_loader:load_server_config.

test(server_search_url_uses_server_scope_override) :-
    config_loader:user_config_path(Path),
    write_config(Path, 'search_engine("https://engine.example.test/find?q=~w").\n'),
    config_loader:load_server_config,
    config_loader:search_url('prolog test', URL),
    sub_atom(URL, 0, _, _, 'https://engine.example.test/find?q=').

:- end_tests(api_service).
