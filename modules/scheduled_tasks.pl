:- module(scheduled_tasks, [
    define/4,
    definition/4,
    clear/0,
    valid_mode/1,
    execution_route/2
]).

:- dynamic definition/4.

valid_mode(auto).
valid_mode(prolog).
valid_mode(llm).

define(Id, Cron, Goal, Mode) :-
    nonvar(Id),
    string(Cron),
    Cron \= "",
    string(Goal),
    Goal \= "",
    valid_mode(Mode),
    retractall(definition(Id, _, _, _)),
    assertz(definition(Id, Cron, Goal, Mode)).

clear :- retractall(definition(_, _, _, _)).

execution_route(Goal, llm) :-
    long_task(Goal), !.
execution_route(_, prolog).

long_task(Goal) :-
    string_lower(Goal, Lower),
    member(Verb, [
        "research", "investigate", "analyze", "analyse", "implement",
        "build", "review", "compare", "audit", "refactor", "summarize",
        "summarise", "design"
    ]),
    sub_string(Lower, _, _, _, Verb), !.
long_task(Goal) :-
    split_string(Goal, " \t\n", " \t\n", Words),
    length(Words, Count),
    Count >= 18.
