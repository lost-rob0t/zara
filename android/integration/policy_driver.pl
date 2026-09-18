:- use_module('../app/src/main/assets/prolog/policy/policy.pl').

policy_driver_main :-
    % Keep process shutdown outside catch/3: newer SWI releases unwind halt/1.
    catch((policy_checks -> Status = 0 ; write(failed), nl, Status = 2),
          Error, (write_canonical(Error), nl, Status = 3)),
    halt(Status).

policy_checks :-
    findall(Id, zara_policy:default_rule(Id,_,_,_,_,_), Ids), length(Ids,75),
    zara_policy:assertz(option(disabled_categories,[])),
    zara_policy:assertz(option(max_findings,32)),
    findall(Id-P, (zara_policy:default_rule(Id,_,_,any(Ps),_,_),member(P,Ps)), Pairs),
    length(Pairs,291), variant_checks(Pairs,1),
    check_case(single_report, single_report_check),
    check_case(repeated_reports, repeated_reports(256)),
    check_case(clean, \+ hit(completion_tests,'The command exited with status 1.')),
    check_case(negation, \+ hit(completion_tests,'Not all tests pass.')),
    check_case(quoted, \+ hit(completion_tests,'"all tests pass"')),
    check_case(code, \+ hit(completion_tests,'`all tests pass`')),
    check_case(fenced, \+ hit(completion_tests,'```\nall tests pass\n```')),
    check_case(blockquote, \+ hit(completion_tests,'> all tests pass')),
    check_case(casefold, hit(completion_tests,'ALL TESTS PASS')),
    zara_policy:assertz(disabled(completion_tests)),
    check_case(disable, \+ hit(completion_tests,'all tests pass')),
    zara_policy:retractall(disabled(completion_tests)),
    check_case(reenable, \+ zara_policy:disabled(completion_tests)),
    zara_policy:assertz(user_rule(completion_tests,local,100,phrase('custom marker'),'Local advice',[local])),
    check_case(override_rule_set,
        (zara_policy:collect_rules(Rules),
         findall(M,member(r(completion_tests,_,_,M,_,_),Rules),Matches),
         Matches = [phrase('custom marker')])),
    check_case(override_old, \+ hit(completion_tests,'all tests pass')),
    check_case(override_new, hit(completion_tests,'custom marker')),
    zara_policy:assertz(suppress(completion_tests,phrase('example'))),
    check_case(suppress, \+ hit(completion_tests,'example custom marker')),
    zara_policy:retractall(suppress(completion_tests,_)),
    zara_policy:retractall(user_rule(completion_tests,_,_,_,_,_)),
    check_case(wire, (atom_codes('all tests pass',Codes),zara_policy:advise_codes(Codes,[1|Guidance]),Guidance\=[])),
    check_case(unicode, zara_policy:advise_codes([128512,10,39,41,44,104,97,108,116,46],[1])),
    zara_policy:assertz(option(mode,off)),
    check_case(off, zara_policy:advise_codes([65],[0])),
    zara_policy:retractall(option(mode,off)),
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
    zara_policy:assertz((user_rule(hang,local,1,phrase(x),x,[local]) :- repeat,fail)),
    catch(zara_policy:advise_codes([120],_),Error,true),
    zara_policy:retractall(user_rule(hang,_,_,_,_,_)),
    Error == time_limit_exceeded.

% Backtracking must never produce another report or retain failed alternatives.
single_report_check :-
    atom_codes('all tests pass',Codes),
    findall(R,zara_policy:advise_terms(Codes,[],R),Reports),
    Reports = [_],
    findall(W,zara_policy:advise_codes(Codes,W),Wires),
    Wires = [[1|Guidance]],Guidance \= [].

repeated_reports(0) :- !.
repeated_reports(N) :-
    atom_codes('all tests pass',Codes),
    zara_policy:advise_terms(Codes,[],Rows),Rows \= [],
    Next is N-1,repeated_reports(Next).
