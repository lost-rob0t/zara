:- begin_tests(philosophy_profiles).

:- use_module('../kb/philosophy').
:- use_module('../modules/philosophy_expert').
:- use_module('../kb/agent_profiles').
:- use_module('../modules/agent_profiles').

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

test(unknown_profile_fails, [fail]) :-
    agent_profiles:resolve_mention("@nobody", _, _, _, _, _, _).

:- end_tests(philosophy_profiles).
