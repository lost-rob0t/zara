:- begin_tests(philosophy_profiles).

:- use_module('../kb/philosophy').
:- use_module('../modules/philosophy_expert').
:- use_module('../kb/agent_profiles').
:- use_module('../modules/agent_profiles').
:- use_module('../modules/config_loader').

test(concept_lookup_normalizes_spaces) :-
    philosophy_expert:concept_summary("virtue ethics", virtue_ethics, Summary),
    sub_string(Summary, _, _, _, "character").

test(compare_positions_returns_distinct_views) :-
    philosophy_expert:compare_positions(
        "morality",
        "kant",
        "mill",
        Kant,
        Mill
    ),
    Kant \= Mill.

test(argument_lookup) :-
    philosophy_expert:argument_summary(
        "categorical imperative",
        categorical_imperative,
        deontology,
        Premises,
        Conclusion
    ),
    Premises = [_|_],
    string(Conclusion).

test(mara_profile_resolves_case_insensitive_mention) :-
    agent_profiles:resolve_mention(
        "@Mara",
        mara,
        "Mara",
        Prompt,
        all,
        KBs,
        shared
    ),
    string(Prompt),
    member(philosophy, KBs).

test(ci_worker_profile_resolves_hyphenated_mention) :-
    agent_profiles:resolve_mention(
        "@ci-worker",
        ci_worker,
        "CI Worker",
        _,
        Tools,
        _,
        session
    ),
    member(read_file, Tools),
    member(query_prolog, Tools).

test(prolog_config_accepts_profile_overrides) :-
    config_loader:validate_user_fact(
        agent_profile(test_worker, "Test Worker"),
        kb_agent_profiles,
        agent_profile(test_worker, "Test Worker")
    ),
    config_loader:validate_user_fact(
        agent_profile_tools(test_worker, [query_prolog, calculator]),
        kb_agent_profiles,
        agent_profile_tools(test_worker, [query_prolog, calculator])
    ),
    config_loader:validate_user_fact(
        agent_profile_kbs(test_worker, [philosophy]),
        kb_agent_profiles,
        agent_profile_kbs(test_worker, [philosophy])
    ).

test(unknown_profile_fails, [fail]) :-
    agent_profiles:resolve_mention("@nobody", _, _, _, _, _, _).

:- end_tests(philosophy_profiles).
