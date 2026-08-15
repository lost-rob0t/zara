:- module(capability_resolver, [
    candidate/4,
    candidates/3,
    select/3
]).

:- use_module('../kb/capabilities').
:- use_module('../kb/config').

candidate(Intent, Args, Provider, Priority) :-
    kb_capabilities:capability_provider(Intent, Provider, Priority),
    provider_satisfies(Provider, Intent, Args).

candidates(Intent, Args, Candidates) :-
    findall(
        Priority-Provider,
        candidate(Intent, Args, Provider, Priority),
        RawCandidates
    ),
    keysort(RawCandidates, Ascending),
    reverse(Ascending, Candidates).

select(Intent, Args, Provider) :-
    candidates(Intent, Args, [_-Provider|_]).

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
