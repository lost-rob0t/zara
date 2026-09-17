:- use_module('../app/src/main/assets/prolog/policy/policy.pl').

policy_driver_main :-
    catch((policy_checks -> halt(0) ; write(failed), nl, halt(2)),
          Error, (write_canonical(Error), nl, halt(3))).

policy_checks :-
    findall(Id, zara_policy:default_rule(Id,_,_,_,_,_), Ids), length(Ids,75),
    assertz(zara_policy:option(disabled_categories,[])),
    assertz(zara_policy:option(max_findings,32)),
    findall(Id-P, (zara_policy:default_rule(Id,_,_,any(Ps),_,_),member(P,Ps)), Pairs),
    length(Pairs,291), variant_checks(Pairs,1),
    check_case(clean, \+ hit(completion_tests,'The command exited with status 1.')),
    check_case(negation, \+ hit(completion_tests,'Not all tests pass.')),
    check_case(quoted, \+ hit(completion_tests,'"all tests pass"')),
    check_case(code, \+ hit(completion_tests,'`all tests pass`')),
    check_case(fenced, \+ hit(completion_tests,'```\nall tests pass\n```')),
    check_case(blockquote, \+ hit(completion_tests,'> all tests pass')),
    check_case(casefold, hit(completion_tests,'ALL TESTS PASS')),
    assertz(zara_policy:disabled(completion_tests)),
    check_case(disable, \+ hit(completion_tests,'all tests pass')),
    retractall(zara_policy:disabled(completion_tests)),
    assertz(zara_policy:user_rule(completion_tests,local,100,phrase('custom marker'),'Local advice',[local])),
    check_case(override_old, \+ hit(completion_tests,'all tests pass')),
    check_case(override_new, hit(completion_tests,'custom marker')),
    assertz(zara_policy:suppress(completion_tests,phrase('example'))),
    check_case(suppress, \+ hit(completion_tests,'example custom marker')),
    retractall(zara_policy:suppress(completion_tests,_)),
    retractall(zara_policy:user_rule(completion_tests,_,_,_,_,_)),
    check_case(wire, (atom_codes('all tests pass',Codes),zara_policy:advise_codes(Codes,[1|Guidance]),Guidance\=[])),
    check_case(unicode, zara_policy:advise_codes([128512,10,39,41,44,104,97,108,116,46],[1])),
    assertz(zara_policy:option(mode,off)),
    check_case(off, zara_policy:advise_codes([65],[0])),
    retractall(zara_policy:option(mode,off)),
    check_case(timeout, timeout_check).

variant_checks([],_).
variant_checks([Id-Phrase|Pairs],Index) :-
    zara_policy:text_codes(Phrase,Codes),
    zara_policy:advise_terms(Codes,[],Rows),
    (member(finding(Id,_,_,_,_),Rows) -> true
    ; write_canonical(missing(Id,Index)),nl,fail),
    write_canonical(variant(Index,Id)),nl,
    Next is Index+1,variant_checks(Pairs,Next).

hit(Id,Text) :- atom_codes(Text,Codes), zara_policy:advise_terms(Codes,[],Rows),
    member(finding(Id,_,_,_,_),Rows).

check_case(Name,Goal) :-
    (call(Goal) -> write_canonical(verified(Name)),nl
    ; write_canonical(failed(Name)),nl,fail).

timeout_check :-
    assertz((zara_policy:user_rule(hang,local,1,phrase(x),x,[local]) :- repeat,fail)),
    catch(zara_policy:advise_codes([120],_),Error,true),
    retractall(zara_policy:user_rule(hang,_,_,_,_,_)),
    nonvar(Error).
