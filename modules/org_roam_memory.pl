:- module(org_roam_memory, [
    org_memory/5,
    org_project_context/3,
    replace_org_memory/5,
    remove_org_memory/1,
    clear_org_memory/0
]).

:- use_module(library(error)).

:- dynamic org_memory/5.

replace_org_memory(Source, NodeId, Project, Text, Tags) :-
    validate_memory_row(Source, NodeId, Project, Text, Tags),
    with_mutex(org_roam_memory,
        ( retractall(org_memory(Source, _, _, _, _)),
          assertz(org_memory(Source, NodeId, Project, Text, Tags))
        )).

remove_org_memory(Source) :-
    must_be(string, Source),
    with_mutex(org_roam_memory,
        retractall(org_memory(Source, _, _, _, _))).

clear_org_memory :-
    with_mutex(org_roam_memory,
        retractall(org_memory(_, _, _, _, _))).

org_project_context(Project, NodeId, Text) :-
    org_memory(_, NodeId, Project, Text, _),
    Project \== "".

validate_memory_row(Source, NodeId, Project, Text, Tags) :-
    must_be(string, Source),
    must_be(string, NodeId),
    must_be(string, Project),
    must_be(string, Text),
    must_be(list, Tags),
    maplist(must_be_string, Tags).

must_be_string(Value) :-
    must_be(string, Value).
