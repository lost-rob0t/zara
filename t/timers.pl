:- begin_tests(timers).

:- use_module('../modules/intent_resolver').
:- use_module('../modules/commands').
:- use_module('../modules/alarm').

:- dynamic pending_timer/2.
:- dynamic notification/2.

timer_fixture("set a timer for 10 seconds", 10, '').
timer_fixture("timer 1 second", 1, '').
timer_fixture("timer 2 minutes tea", 120, tea).
timer_fixture("set timer named pasta for 1 hour", 3600, pasta).
timer_fixture("timer 3 hrs laundry", 10800, laundry).
timer_fixture("timer 0 seconds", 0, '').

test(timer_parser_fixtures,
     [forall(timer_fixture(Input, Seconds, Name))]) :-
    intent_resolver:resolve(Input, timer, [Seconds, Name]).

test(malformed_timer_fails, [forall(member(Input, [
        "timer", "timer seconds", "timer ten seconds", "timer 2 days"
    ])), fail]) :-
    intent_resolver:resolve(Input, _, _).

test(unsupported_alarm_fails) :-
    intent_resolver:resolve("set an alarm for 7", alarm, Args),
    \+ commands:execute(alarm, Args).

test(completion_notifies_once) :-
    with_fake_timer_runtime((
        commands:execute(timer, [0, tea]),
        run_pending_timers,
        findall(Name-Message, notification(Name, Message), Notifications),
        Notifications = [tea-"Timer \"tea\" finished."]
    )).

test(resolved_timer_schedules_exact_seconds) :-
    with_fake_timer_runtime((
        intent_resolver:resolve("set a timer for 10 seconds", timer, Args),
        commands:execute(timer, Args),
        pending_timer(10, _)
    )).

test(unnamed_completion_notifies_once) :-
    with_fake_timer_runtime((
        commands:execute(timer, [0, '']),
        run_pending_timers,
        findall(Name-Message, notification(Name, Message), Notifications),
        Notifications = [''-"Timer finished."]
    )).

test(multiple_timers_complete_independently) :-
    with_fake_timer_runtime((
        commands:execute(timer, [10, first]),
        commands:execute(timer, [20, second]),
        findall(Goal, pending_timer(_, Goal), Pending),
        length(Pending, 2),
        run_pending_timers,
        findall(Name, notification(Name, _), Names),
        Names = [first, second]
    )).

test(lifecycle_events_are_ordered_and_principal_scoped) :-
    with_fake_timer_runtime((
        alarm:with_principal('uid:timer-owner',
            commands:execute(timer, [10, tea])),
        alarm:drain_timer_events('uid:other-owner', OtherEvents),
        OtherEvents = [],
        alarm:drain_timer_events('uid:timer-owner', [Scheduled]),
        get_dict(type, Scheduled, scheduled),
        get_dict(name, Scheduled, "tea"),
        get_dict(revision, Scheduled, 1),
        get_dict(created_at_ns, Scheduled, CreatedAtNs),
        get_dict(due_at_ns, Scheduled, DueAtNs),
        DueAtNs - CreatedAtNs =:= 10000000000,
        get_dict(timer_id, Scheduled, TimerId),
        atom(TimerId),
        TimerId \== '',
        run_pending_timers,
        alarm:drain_timer_events('uid:timer-owner', [Fired]),
        get_dict(type, Fired, fired),
        get_dict(timer_id, Fired, TimerId),
        get_dict(name, Fired, "tea"),
        get_dict(created_at_ns, Fired, CreatedAtNs),
        get_dict(due_at_ns, Fired, DueAtNs),
        get_dict(fired_at_ns, Fired, FiredAtNs),
        FiredAtNs >= DueAtNs,
        get_dict(revision, Fired, 2),
        get_dict(message, Fired, "Timer \"tea\" finished."),
        \+ alarm:timer_record(
            'uid:timer-owner', TimerId, _, _, _, _, _
        ),
        alarm:drain_timer_events('uid:timer-owner', [])
    )).

with_fake_timer_runtime(Goal) :-
    retractall(pending_timer(_, _)),
    retractall(notification(_, _)),
    retractall(alarm:timer_record(_, _, _, _, _, _, _)),
    retractall(alarm:timer_event(_, _)),
    asserta((alarm:timer_scheduler(Seconds, TimerGoal, Id) :-
             plunit_timers:fake_schedule(Seconds, TimerGoal, Id)), SchedulerRef),
    asserta((alarm:completion_notifier(Name, Message) :-
             plunit_timers:record_notification(Name, Message)), NotifierRef),
    asserta((sound:sound_player(_, _) :- true), PlayerRef),
    call_cleanup(Goal, (
        erase(SchedulerRef),
        erase(NotifierRef),
        erase(PlayerRef),
        retractall(pending_timer(_, _)),
        retractall(notification(_, _)),
        retractall(alarm:timer_record(_, _, _, _, _, _, _)),
        retractall(alarm:timer_event(_, _))
    )).

fake_schedule(Seconds, Goal, fake_timer) :-
    assertz(pending_timer(Seconds, Goal)).

record_notification(Name, Message) :-
    assertz(notification(Name, Message)).

run_pending_timers :-
    forall(retract(pending_timer(_, Goal)), call(Goal)).

:- end_tests(timers).
