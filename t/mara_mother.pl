:- begin_tests(mara_mother).

:- use_module('../modules/mara_mother').

test(routine_is_pending_before_schedule) :-
    routine_state(1000, none, 300, 900, State),
    assertion(State == pending).

test(routine_is_due_inside_grace_window) :-
    routine_state(1000, none, 300, 1200, State),
    assertion(State == due).

test(routine_is_missed_after_grace_window) :-
    routine_state(1000, none, 300, 1400, State),
    assertion(State == missed).

test(acknowledgement_wins) :-
    routine_state(1000, 1100, 300, 1400, State),
    assertion(State == acknowledged).

test(critical_routine_escalates_after_schedule) :-
    reminder_action(critical, 1000, none, none, 300, 1200, Action),
    assertion(Action == escalate).

test(normal_routine_reminds) :-
    reminder_action(normal, 1000, none, none, 300, 1200, Action),
    assertion(Action == remind).

test(repeat_window_suppresses_duplicate) :-
    reminder_action(critical, 1000, none, 1150, 300, 1200, Action),
    assertion(Action == none).

test(active_surface_follows_preference) :-
    choose_notification_surface(
        [phone_active, desktop_active],
        [desktop, phone],
        Surface
    ),
    assertion(Surface == desktop).

test(falls_back_to_first_preference_when_no_surface_active) :-
    choose_notification_surface([away], [phone, desktop], Surface),
    assertion(Surface == phone).

test(slot_rejects_overlap, [fail]) :-
    slot_available([1000-1200], 1100, 300).

test(slot_accepts_touching_boundary) :-
    slot_available([1000-1200], 1200, 300).

test(earliest_slot_skips_busy_intervals) :-
    earliest_slot(
        1000,
        2200,
        300,
        300,
        [1000-1300, 1600-1900],
        Start
    ),
    assertion(Start == 1300).

:- end_tests(mara_mother).
