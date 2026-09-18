:- begin_tests(prolog_mode).
:- use_module('../modules/prolog_mode').
:- use_module(library(http/json)).

result(Input, Principal, Dict) :-
    prolog_mode:run_json(Input, Principal, JSON),
    atom_json_dict(JSON, Dict, []).

test(bindings) :-
    result("member(X, [a,b]).", "local", Dict),
    assertion(Dict.status == "ok"),
    Dict.bindings = [First, Second], assertion(First.'X' == "a"), assertion(Second.'X' == "b").
test(false) :-
    result("fail.", "local", Dict),
    assertion(Dict.status == "false").
test(ground_success) :-
    result("true.", "local", Dict),
    assertion(Dict.status == "ok"),
    Dict.bindings = [Row], assertion(is_dict(Row)), dict_pairs(Row, _, []).
test(remote_requires_explicit_permission) :-
    result("true.", "untrusted", Dict),
    assertion(Dict.error == "permission_denied").
test(effectful_query_is_not_implicitly_authorized) :-
    result("shell('false').", "local", Dict),
    assertion(Dict.error == "permission_denied").
test(syntax_error) :-
    result("member(.", "local", Dict),
    assertion(Dict.status == "error").
test(extra_terms_rejected) :-
    result("true. fail.", "local", Dict),
    assertion(Dict.status == "error").
test(solution_limit) :-
    result("between(1, 1000, X).", "local", Dict),
    assertion(Dict.limit_reached == true),
    length(Dict.bindings, 64).
:- end_tests(prolog_mode).
