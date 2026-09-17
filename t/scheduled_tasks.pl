:- begin_tests(scheduled_tasks).

:- use_module('../modules/scheduled_tasks').

setup_clean :- scheduled_tasks:clear.


test(define_roundtrip, [setup(setup_clean)]) :-
    scheduled_tasks:define(morning, "0 9 * * 1-5", "open firefox", auto),
    scheduled_tasks:definition(morning, "0 9 * * 1-5", "open firefox", auto).


test(define_replaces_same_id, [setup(setup_clean)]) :-
    scheduled_tasks:define(morning, "0 9 * * *", "open firefox", auto),
    scheduled_tasks:define(morning, "30 9 * * *", "open chromium", prolog),
    findall(Cron-Goal-Mode,
            scheduled_tasks:definition(morning, Cron, Goal, Mode),
            Rows),
    assertion(Rows == ["30 9 * * *"-"open chromium"-prolog]).


test(short_auto_routes_to_prolog) :-
    scheduled_tasks:execution_route("open firefox", prolog).


test(long_auto_routes_to_llm) :-
    scheduled_tasks:execution_route(
        "research the release notes, compare the breaking changes, inspect the repository, and write a detailed implementation plan",
        llm
    ).


test(explicit_modes_are_valid) :-
    scheduled_tasks:valid_mode(auto),
    scheduled_tasks:valid_mode(prolog),
    scheduled_tasks:valid_mode(llm).


test(invalid_mode_fails, [fail]) :-
    scheduled_tasks:valid_mode(telepathy).

:- end_tests(scheduled_tasks).
