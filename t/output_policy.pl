:- begin_tests(output_policy).
:- use_module('../modules/output_policy').
:- use_module(library(http/json)).

result(Text, Dict) :-
    output_policy:evaluate_json(Text, "{}", JSON), atom_json_dict(JSON, Dict, []).

test(no_rules_allow) :-
    result("normal", Dict), assertion(Dict.status == "ok"), assertion(Dict.advice == []).
test(exact) :- output_policy:text_matches(exact("X"), "X").
test(exact_case_sensitive, [fail]) :- output_policy:text_matches(exact("X"), "x").
test(contains) :- output_policy:text_matches(contains("AI"), "An AI preamble").
test(unicode_case_fold) :- output_policy:text_matches(icontains("ÉCOLE"), "une école").
test(composition) :- output_policy:text_matches(all([contains("a"), not(contains("z"))]), "abc").
test(any) :- output_policy:text_matches(any([exact("no"), exact("yes")]), "yes").
test(text_not_code) :-
    result("halt. assertz(compromised).", Dict), assertion(Dict.status == "ok").
test(advice, [setup(assertz(output_policy:advice(test_rule, 10, _, "draft", "Revise."), Ref)), cleanup(erase(Ref))]) :-
    result("draft", Dict),
    Dict.advice = [Advice], assertion(Advice.id == "test_rule"), assertion(Advice.message == "Revise.").
test(exception_is_not_allow, [setup(assertz((output_policy:advice(_, _, _, _, _) :- throw(private_detail)), Ref)), cleanup(erase(Ref))]) :-
    result("draft", Dict), assertion(Dict.status == "error"), assertion(Dict.error == "evaluation_failed").
test(duplicate_ids_rejected, [setup((assertz(output_policy:advice(dup, 1, _, _, "a"), A), assertz(output_policy:advice(dup, 2, _, _, "b"), B))), cleanup((erase(A),erase(B)))]) :-
    result("draft", Dict), assertion(Dict.status == "error").
:- end_tests(output_policy).
