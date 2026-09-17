:- use_module(library(plunit)).
:- use_module(library(http/json)).
:- use_module('../modules/response_policy').
:- use_module('../kb/response_policy', []).
:- use_module('../kb/policy_config', []).

:- begin_tests(response_policy).

ids(Text, Ids) :-
    response_policy:analyze_json(Text, Json),
    atom_json_dict(Json, Report, [value_string_as(atom)]),
    assertion(Report.status == ok),
    findall(Id, (member(Finding, Report.findings), Id = Finding.id), Ids).

test(all_authored_patterns, [forall((
    kb_response_policy:rule(Id, _, _, Patterns, _, _), member(Pattern, Patterns)
))]) :-
    ids(Pattern, Ids), assertion(memberchk(Id, Ids)).

test(case_punctuation_unicode) :-
    ids("ALL tests PASSED!", Ids), assertion(memberchk(tests_passed, Ids)),
    ids("I’ll work on this in the background.", Other),
    assertion(memberchk(background_promise, Other)).

test(honest_uncertainty) :-
    ids("I am not sure. I have not run the tests. I cannot verify that claim.", []).

test(justified_refusal) :-
    ids("I cannot help steal credentials. I can explain account protection.", []).

test(negated_claim) :- ids("Not all tests passed.", []).
test(conditional_claim) :- ids("If all tests passed, release the build.", []).
test(quoted_claim) :- ids("The model said \"all tests passed\".", []).
test(single_quoted_claim) :- ids("It says 'all tests passed' here.", []).
test(inline_code) :- ids("The string `all tests passed` is a fixture.", []).
test(fenced_code) :- ids("```text\nall tests passed\n```", []).
test(block_quote) :- ids("> all tests passed", []).
test(word_boundaries) :- ids("All tests passedness is not a word.", []).

test(deduplication) :-
    ids("All tests passed. All tests passed. The tests are green.", Ids),
    include(=(tests_passed), Ids, [tests_passed]).

test(disable_default, [
    setup(assertz(policy_config:user_setting(disabled_rules, [tests_passed]))),
    cleanup(retractall(policy_config:user_setting(disabled_rules, _)))
]) :- ids("All tests passed.", []).

test(user_extends_kb, [
    setup(assertz(response_policy:user_rule(custom_word, style, info,
        ["corporate sparkle"], "Use concrete wording.", local_style))),
    cleanup(retractall(response_policy:user_rule(custom_word, _, _, _, _, _)))
]) :- ids("Corporate sparkle.", [custom_word]).

test(user_override, [
    setup(assertz(response_policy:user_rule(tests_passed, verification, warning,
        ["custom success marker"], "Check my custom evidence.", local_style))),
    cleanup(retractall(response_policy:user_rule(tests_passed, _, _, _, _, _)))
]) :-
    ids("All tests passed.", []),
    ids("Custom success marker.", [tests_passed]).

test(duplicate_setting_fails_explicitly, [
    setup((assertz(policy_config:user_setting(enabled, true)),
           assertz(policy_config:user_setting(enabled, false)))),
    cleanup(retractall(policy_config:user_setting(enabled, _)))
]) :-
    response_policy:analyze_json("Text", Json),
    atom_json_dict(Json, Report, [value_string_as(atom)]),
    assertion(Report.status == inspection_failed).

test(oversized_not_clean, [
    setup(assertz(policy_config:user_setting(max_input_chars, 8))),
    cleanup(retractall(policy_config:user_setting(max_input_chars, _)))
]) :-
    response_policy:analyze_json("Longer than eight", Json),
    atom_json_dict(Json, Report, [value_string_as(atom)]),
    assertion(Report.status == input_limit).

test(disabled, [
    setup(assertz(policy_config:user_setting(enabled, false))),
    cleanup(retractall(policy_config:user_setting(enabled, _)))
]) :-
    response_policy:analyze_json("All tests passed", Json),
    atom_json_dict(Json, Report, [value_string_as(atom)]),
    assertion(Report.status == disabled).

test(catalog_has_provenance) :-
    response_policy:catalog_json(Json),
    atom_json_dict(Json, Catalog, [value_string_as(atom)]),
    length(Catalog.rules, 38),
    forall(member(Rule, Catalog.rules), (
        assertion(nonvar(Rule.source)), assertion(nonvar(Rule.origin))
    )).

test(settings_are_first_class_prolog) :-
    response_policy:settings_json(Json),
    atom_json_dict(Json, Settings, [value_string_as(atom)]),
    assertion(Settings.enabled == true),
    assertion(Settings.mode == advise),
    assertion(Settings.max_revisions == 1).


test(duplicate_rule_fails_explicitly, [
    setup((assertz(response_policy:user_rule(dup, style, info, ["sample"], "Advice.", local_style)),
           assertz(response_policy:user_rule(dup, style, info, ["other"], "Advice.", local_style)))),
    cleanup(retractall(response_policy:user_rule(dup, _, _, _, _, _)))
]) :-
    response_policy:analyze_json("sample", Json),
    atom_json_dict(Json, Report, [value_string_as(atom)]),
    assertion(Report.status == inspection_failed).

test(empty_pattern_fails_explicitly, [
    setup(assertz(response_policy:user_rule(empty, style, info, [""], "Advice.", local_style))),
    cleanup(retractall(response_policy:user_rule(empty, _, _, _, _, _)))
]) :-
    response_policy:analyze_json("text", Json),
    atom_json_dict(Json, Report, [value_string_as(atom)]),
    assertion(Report.status == inspection_failed).

test(review_only_refusal) :-
    response_policy:analyze_json("I cannot discuss any legal topic.", Json),
    atom_json_dict(Json, Report, [value_string_as(atom)]),
    member(Finding, Report.findings), Finding.id == broad_refusal,
    assertion(Finding.repair == false).

test(truncation_reported, [
    setup(assertz(policy_config:user_setting(max_findings, 1))),
    cleanup(retractall(policy_config:user_setting(max_findings, _)))
]) :-
    response_policy:analyze_json("All tests passed. Completely secure.", Json),
    atom_json_dict(Json, Report, [value_string_as(atom)]),
    assertion(Report.truncated == true), length(Report.findings, 1).

:- end_tests(response_policy).
