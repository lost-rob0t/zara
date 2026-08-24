
:- module(alarm, [
    timer_done/1,
    start_timer/1,
    start_timer/2,
    with_principal/2,
    drain_timer_events/2,
    drain_timer_events_json/2
]).
:- use_module(library(time)).
:- use_module(library(http/json)).
:- use_module(library(uuid)).
:- use_module(alert).
:- use_module(sound).

:- dynamic timer_scheduler/3.
:- dynamic completion_notifier/2.
:- dynamic timer_principal/1.
:- dynamic timer_record/7.
:- dynamic timer_event/2.

timer_done(Name) :-
    timer_message(Name, Message),
    ( sound:play_notification_sound(timer) -> true ; true ),
    once(completion_notifier(Name, Message)).

start_timer(Seconds) :-
    start_timer('', Seconds).

start_timer(Name, Seconds) :-
    number(Seconds),
    Seconds >= 0,
    ( atom(Name) ; string(Name) ),
    current_timer_principal(Principal),
    uuid(TimerId),
    now_ns(CreatedAtNs),
    DueAtNs is CreatedAtNs + round(Seconds * 1000000000),
    timer_name_string(Name, NameString),
    Scheduled = timer{
        type:scheduled,
        timer_id:TimerId,
        name:NameString,
        created_at_ns:CreatedAtNs,
        due_at_ns:DueAtNs,
        revision:1
    },
    assertz(timer_record(
        Principal, TimerId, Name, CreatedAtNs, DueAtNs, scheduled, 1
    ), RecordRef),
    assertz(timer_event(Principal, Scheduled), EventRef),
    ( timer_scheduler(Seconds, alarm:timer_done(Principal, TimerId), _)
    -> true
    ;  erase(EventRef),
       erase(RecordRef),
       fail
    ).

timer_done(Principal, TimerId) :-
    retract(timer_record(
        Principal, TimerId, Name, CreatedAtNs, DueAtNs, scheduled, Revision
    )),
    now_ns(ObservedAtNs),
    FiredAtNs is max(ObservedAtNs, DueAtNs),
    NextRevision is Revision + 1,
    timer_name_string(Name, NameString),
    timer_message(Name, Message),
    assertz(timer_event(Principal, timer{
        type:fired,
        timer_id:TimerId,
        name:NameString,
        created_at_ns:CreatedAtNs,
        due_at_ns:DueAtNs,
        fired_at_ns:FiredAtNs,
        revision:NextRevision,
        message:Message
    })),
    ( sound:play_notification_sound(timer) -> true ; true ),
    once(completion_notifier(Name, Message)).

with_principal(Principal, Goal) :-
    setup_call_cleanup(
        asserta(timer_principal(Principal), Ref),
        call(Goal),
        erase(Ref)
    ).

drain_timer_events(Principal, Events) :-
    findall(Event, retract(timer_event(Principal, Event)), Events).

drain_timer_events_json(Principal, Json) :-
    drain_timer_events(Principal, Events),
    atom_json_dict(Json, Events, []).

current_timer_principal(Principal) :-
    once(timer_principal(Principal)), !.
current_timer_principal(local).

now_ns(Nanoseconds) :-
    get_time(Seconds),
    Nanoseconds is round(Seconds * 1000000000).

timer_name_string(Name, String) :-
    format(string(String), '~w', [Name]).

timer_scheduler(Seconds, Goal, Id) :-
    alarm(Seconds, Goal, Id, [remove(true)]).

completion_notifier(_, Message) :-
    alert:alert('Zara Timer', normal, '~w', [Message]).

timer_message('', "Timer finished.").
timer_message("", "Timer finished.").
timer_message(Name, Message) :-
    format(string(Message), 'Timer "~w" finished.', [Name]).
