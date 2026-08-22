:- module(capability_resolver, [
    candidate/4,
    candidate/5,
    candidates/3,
    candidates/4,
    select/3,
    select/4,
    explain/4
]).

:- use_module('../kb/capabilities').
:- use_module('../kb/config').

candidate(Intent, Args, Provider, Priority) :-
    candidate(Intent, Args, [], Provider, Priority).

candidate(Intent, Args, Constraints, Provider, Priority) :-
    is_list(Constraints),
    kb_capabilities:capability_provider(Intent, Provider, Priority),
    provider_satisfies(Provider, Intent, Args),
    constraints_satisfied(Provider, Constraints).

candidates(Intent, Args, Candidates) :-
    candidates(Intent, Args, [], Candidates).

candidates(Intent, Args, Constraints, Candidates) :-
    findall(
        Priority-Provider,
        candidate(Intent, Args, Constraints, Provider, Priority),
        RawCandidates
    ),
    keysort(RawCandidates, Ascending),
    reverse(Ascending, Candidates).

select(Intent, Args, Provider) :-
    select(Intent, Args, [], Provider).

select(Intent, Args, Constraints, Provider) :-
    candidates(Intent, Args, Constraints, [_-Provider|_]).

explain(Intent, Args, Constraints,
        decision(Provider, Priority, Evidence, Properties, Alternatives)) :-
    select(Intent, Args, Constraints, Provider),
    kb_capabilities:capability_provider(Intent, Provider, Priority),
    provider_evidence(Provider, Intent, Args, Evidence),
    findall(
        Property,
        kb_capabilities:capability_property(Provider, Property),
        Properties
    ),
    candidates(Intent, Args, Constraints, Alternatives).

constraints_satisfied(_, []).
constraints_satisfied(Provider, [Constraint|Rest]) :-
    constraint_satisfied(Provider, Constraint),
    constraints_satisfied(Provider, Rest).

constraint_satisfied(Provider, require(Property)) :-
    kb_capabilities:capability_property(Provider, Property).
constraint_satisfied(Provider, exclude(Property)) :-
    \+ kb_capabilities:capability_property(Provider, Property).

provider_satisfies(web_search, search, Args) :-
    is_list(Args).
provider_satisfies(mapped_app, open, [AppName]) :-
    atom(AppName),
    once(kb_config:app_mapping(AppName, _)).
provider_satisfies(direct_app, open, [AppName]) :-
    atom(AppName),
    once(kb_config:direct_app(AppName)).
provider_satisfies(executable_fallback, open, [AppName]) :-
    atom(AppName).

provider_evidence(web_search, search, Args, [arguments(Args)]).
provider_evidence(mapped_app, open, [AppName], [mapping(AppName, Command)]) :-
    once(kb_config:app_mapping(AppName, Command)).
provider_evidence(direct_app, open, [AppName], [direct_app(AppName)]).
provider_evidence(executable_fallback, open, [AppName], [fallback(AppName)]).
