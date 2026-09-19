:- module(todo_expert,
    [ route_task/2,
      route_task/3,
      cadence_file/2,
      project_file/2,
      todo_state/6,
      todo_score/7
    ]).

:- use_module(library(error)).

route_task(Task, File) :-
    route_task(Task, File, _Reason).

route_task(Task, File, project(Project)) :-
    task_project(Task, Project),
    Project \== none,
    !,
    project_file(Project, File).
route_task(Task, File, explicit_horizon(Horizon)) :-
    task_horizon(Task, Horizon),
    Horizon \== unknown,
    !,
    cadence_file(Horizon, File).
route_task(Task, File, due_distance(Days)) :-
    task_days_until_due(Task, Days),
    integer(Days),
    !,
    inferred_horizon(Days, Horizon),
    cadence_file(Horizon, File).
route_task(_Task, 'agenda/inbox.org', unresolved).

project_file(Project, File) :-
    must_be(atom, Project),
    valid_project_slug(Project),
    atomic_list_concat(['agenda/', Project, '.org'], File).

cadence_file(daily, 'agenda/daily.org').
cadence_file(weekly, 'agenda/weekly.org').
cadence_file(monthly, 'agenda/monthly.org').
cadence_file(yearly, 'agenda/yearly.org').
cadence_file(inbox, 'agenda/inbox.org').

inferred_horizon(Days, daily) :-
    Days =< 1,
    !.
inferred_horizon(Days, weekly) :-
    Days =< 7,
    !.
inferred_horizon(Days, monthly) :-
    Days =< 31,
    !.
inferred_horizon(_Days, yearly).

todo_state(Status, _Scheduled, _Duration, _Deadline, _Now, done) :-
    terminal_status(Status),
    !.
todo_state(Status, _Scheduled, _Duration, _Deadline, _Now, waiting) :-
    waiting_status(Status),
    !.
todo_state(_Status, _Scheduled, _Duration, Deadline, Now, overdue) :-
    timestamp_value(Deadline),
    Deadline < Now,
    !.
todo_state(_Status, Scheduled, DurationMinutes, _Deadline, Now, active) :-
    timestamp_value(Scheduled),
    integer(DurationMinutes),
    DurationMinutes > 0,
    End is Scheduled + DurationMinutes * 60,
    Scheduled =< Now,
    Now < End,
    !.
todo_state(_Status, Scheduled, _Duration, _Deadline, Now, upcoming) :-
    timestamp_value(Scheduled),
    Scheduled > Now,
    !.
todo_state(_Status, _Scheduled, _Duration, _Deadline, _Now, actionable).

todo_score(Status, Priority, Scheduled, Duration, Deadline, Now, Score) :-
    todo_state(Status, Scheduled, Duration, Deadline, Now, State),
    state_weight(State, StateWeight),
    priority_weight(Priority, PriorityWeight),
    deadline_weight(Deadline, Now, DeadlineWeight),
    Score is StateWeight + PriorityWeight + DeadlineWeight.

terminal_status(done).
terminal_status(kill).
terminal_status(canceled).
terminal_status(cancelled).

waiting_status(wait).
waiting_status(waiting).
waiting_status(hold).

state_weight(active, 100).
state_weight(overdue, 90).
state_weight(actionable, 50).
state_weight(upcoming, 30).
state_weight(waiting, 0).
state_weight(done, -100).

priority_weight(a, 30).
priority_weight(b, 20).
priority_weight(c, 10).
priority_weight(none, 0).
priority_weight(_, 0).

deadline_weight(none, _Now, 0) :- !.
deadline_weight(Deadline, Now, 25) :-
    timestamp_value(Deadline),
    Deadline =< Now,
    !.
deadline_weight(Deadline, Now, 15) :-
    timestamp_value(Deadline),
    Deadline - Now =< 86400,
    !.
deadline_weight(Deadline, Now, 8) :-
    timestamp_value(Deadline),
    Deadline - Now =< 604800,
    !.
deadline_weight(_Deadline, _Now, 0).

timestamp_value(Value) :-
    number(Value).

task_project(Task, Project) :-
    dict_value(Task, project, none, Project).

task_horizon(Task, Horizon) :-
    dict_value(Task, horizon, unknown, Horizon).

task_days_until_due(Task, Days) :-
    dict_value(Task, days_until_due, unknown, Days).

dict_value(Dict, Key, Default, Value) :-
    (   is_dict(Dict),
        get_dict(Key, Dict, Found)
    ->  Value = Found
    ;   Value = Default
    ).

valid_project_slug(Project) :-
    atom_chars(Project, Chars),
    Chars \== [],
    maplist(valid_slug_char, Chars),
    \+ sub_atom(Project, _, _, _, '..').

valid_slug_char(Char) :-
    char_type(Char, alnum),
    !.
valid_slug_char('-').
valid_slug_char('_').
