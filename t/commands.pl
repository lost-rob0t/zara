:- begin_tests(commands).

:- use_module('../modules/commands').
:- use_module('../modules/capability_resolver').

test(known_handler_succeeds) :-
    commands:execute(greet, []).

test(unknown_intent_fails, [fail]) :-
    commands:execute(unknown_intent, []).

test(malformed_arguments_fail, [fail]) :-
    commands:execute(open, []).

test(missing_executable_fails, [fail]) :-
    commands:launch_process('command_that_does_not_exist_zara', []).

test(handler_exception_fails, [fail]) :-
    commands:launch_process(not_a_command(42), []).

test(search_has_symbolic_provider) :-
    capability_resolver:candidate(search, [starintel, python], web_search, 100).

test(mapped_app_is_preferred) :-
    capability_resolver:select(open, [github], mapped_app).

test(direct_app_is_selected_without_mapping) :-
    capability_resolver:select(open, [emacs], direct_app).

test(unknown_app_has_fallback) :-
    capability_resolver:select(
        open,
        [zara_capability_test_missing_app],
        executable_fallback
    ).

test(open_candidates_are_ranked) :-
    capability_resolver:candidates(
        open,
        [vim],
        [100-mapped_app, 50-direct_app, 10-executable_fallback]
    ).

test(constraints_can_exclude_speculative_fallback) :-
    capability_resolver:candidates(
        open,
        [vim],
        [exclude(certainty(speculative))],
        [100-mapped_app, 50-direct_app]
    ).

test(strict_unknown_app_has_no_candidate, [fail]) :-
    capability_resolver:select(
        open,
        [zara_capability_test_missing_app],
        [exclude(certainty(speculative))],
        _
    ).

test(required_property_filters_candidates) :-
    capability_resolver:select(
        search,
        [starintel, python],
        [require(scope(web))],
        web_search
    ).

test(conflicting_constraints_have_no_candidate, [fail]) :-
    capability_resolver:candidate(
        open,
        [vim],
        [require(scope(local)), exclude(scope(local))],
        _,
        _
    ).

test(explanation_contains_evidence_and_alternatives) :-
    capability_resolver:explain(
        open,
        [github],
        [],
        decision(
            mapped_app,
            100,
            [mapping(github, _)],
            Properties,
            [100-mapped_app, 10-executable_fallback]
        )
    ),
    member(scope(local), Properties),
    member(certainty(configured), Properties).

test(unknown_intent_has_no_provider, [fail]) :-
    capability_resolver:select(unknown_intent, [], _).

:- end_tests(commands).
