:- begin_tests(todo_expert).

:- use_module('../modules/todo_expert').

test(project_route_overrides_horizon) :-
    route_task(task{project:zara, horizon:daily}, File, Reason),
    assertion(File == 'agenda/zara.org'),
    assertion(Reason == project(zara)).

test(unresolved_route_uses_inbox) :-
    route_task(task{}, File, Reason),
    assertion(File == 'agenda/inbox.org'),
    assertion(Reason == unresolved).

test(active_schedule_state) :-
    todo_state(strt, 1000, 60, none, 1200, State),
    assertion(State == active).

test(upcoming_schedule_state) :-
    todo_state(todo, 2000, 30, none, 1200, State),
    assertion(State == upcoming).

test(overdue_deadline_beats_unscheduled_actionable) :-
    todo_state(todo, none, 30, 1100, 1200, State),
    assertion(State == overdue).

test(waiting_state) :-
    todo_state(wait, none, 30, none, 1200, State),
    assertion(State == waiting).

test(done_state) :-
    todo_state(done, 1000, 30, 1100, 1200, State),
    assertion(State == done).

test(priority_score_rewards_active_and_priority) :-
    todo_score(strt, a, 1000, 60, none, 1200, ActiveA),
    todo_score(todo, c, 2000, 60, none, 1200, FutureC),
    assertion(ActiveA > FutureC).

:- end_tests(todo_expert).
