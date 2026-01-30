:- module(todo_schedule, [no_overlap/2]).

no_overlap(Scheduled, Existing) :-
    \+ member(Scheduled, Existing).
