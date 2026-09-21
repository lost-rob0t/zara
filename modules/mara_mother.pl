:- module(mara_mother,
    [ routine_state/5,
      reminder_action/7,
      choose_notification_surface/3,
      slot_available/3,
      earliest_slot/6
    ]).

:- use_module(library(error)).

routine_state(Scheduled, Acknowledged, _GraceSeconds, _Now, acknowledged) :-
    timestamp_value(Acknowledged),
    Acknowledged >= Scheduled,
    !.
routine_state(Scheduled, _Acknowledged, _GraceSeconds, Now, pending) :-
    Now < Scheduled,
    !.
routine_state(Scheduled, _Acknowledged, GraceSeconds, Now, due) :-
    Now =< Scheduled + GraceSeconds,
    !.
routine_state(_Scheduled, _Acknowledged, _GraceSeconds, _Now, missed).

reminder_action(_Criticality, Scheduled, Acknowledged, _LastReminder, _RepeatSeconds, Now, none) :-
    routine_state(Scheduled, Acknowledged, 0, Now, acknowledged),
    !.
reminder_action(_Criticality, Scheduled, _Acknowledged, _LastReminder, _RepeatSeconds, Now, none) :-
    Now < Scheduled,
    !.
reminder_action(Criticality, Scheduled, _Acknowledged, LastReminder, RepeatSeconds, Now, Action) :-
    must_be(nonneg, RepeatSeconds),
    reminder_ready(LastReminder, RepeatSeconds, Now),
    !,
    (   Criticality == critical,
        Now > Scheduled
    ->  Action = escalate
    ;   Action = remind
    ).
reminder_action(_Criticality, _Scheduled, _Acknowledged, _LastReminder, _RepeatSeconds, _Now, none).

choose_notification_surface(Activities, Preferred, Surface) :-
    must_be(list, Activities),
    must_be(list, Preferred),
    member(Surface, Preferred),
    surface_active(Surface, Activities),
    !.
choose_notification_surface(_Activities, [Surface | _], Surface).

surface_active(phone, Activities) :-
    memberchk(phone_active, Activities).
surface_active(desktop, Activities) :-
    memberchk(desktop_active, Activities).
surface_active(wear, Activities) :-
    memberchk(wear_active, Activities).

reminder_ready(none, _RepeatSeconds, _Now).
reminder_ready(LastReminder, RepeatSeconds, Now) :-
    timestamp_value(LastReminder),
    Now - LastReminder >= RepeatSeconds.

slot_available(Busy, Start, DurationSeconds) :-
    must_be(list, Busy),
    must_be(nonneg, DurationSeconds),
    End is Start + DurationSeconds,
    forall(
        member(BusyStart-BusyEnd, Busy),
        ( timestamp_value(BusyStart),
          timestamp_value(BusyEnd),
          ( End =< BusyStart ; Start >= BusyEnd )
        )
    ).

earliest_slot(WindowStart, WindowEnd, DurationSeconds, StepSeconds, Busy, Start) :-
    must_be(integer, WindowStart),
    must_be(integer, WindowEnd),
    must_be(integer, DurationSeconds),
    must_be(integer, StepSeconds),
    DurationSeconds > 0,
    StepSeconds > 0,
    LastStart is WindowEnd - DurationSeconds,
    LastStart >= WindowStart,
    first_available(WindowStart, LastStart, StepSeconds, Busy, DurationSeconds, Start).

first_available(Current, Last, _Step, Busy, Duration, Current) :-
    Current =< Last,
    slot_available(Busy, Current, Duration),
    !.
first_available(Current, Last, Step, Busy, Duration, Start) :-
    Next is Current + Step,
    Next =< Last,
    first_available(Next, Last, Step, Busy, Duration, Start).

timestamp_value(Value) :-
    number(Value).
