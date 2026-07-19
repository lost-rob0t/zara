:- begin_tests(sounds).

:- use_module('../modules/sound').
:- use_module('../modules/alarm').
:- use_module('../kb/config').

:- dynamic played/2.
:- dynamic notified/0.

test(distinct_default_sounds) :-
    once(kb_config:timer_sound(TimerPath)),
    once(kb_config:alarm_sound(AlarmPath)),
    TimerPath \= AlarmPath.

test(timer_and_alarm_sounds_are_playable) :-
    with_fake_player((
        once(sound:play_notification_sound(timer)),
        once(sound:play_notification_sound(alarm)),
        findall(Kind, played(Kind, _), Kinds),
        Kinds = [timer, alarm]
    )).

test(timer_completion_plays_timer_sound_once) :-
    with_fake_player(with_fake_notifier((
        alarm:timer_done(tea),
        findall(Path, played(timer, Path), Paths),
        Paths = [_],
        findall(true, notified, Notifications),
        Notifications = [true]
    ))).

test(disabled_timer_does_not_disable_alarm) :-
    with_fake_player(
        with_sound_setting(timer, disabled, (
            once(sound:play_notification_sound(timer)),
            once(sound:play_notification_sound(alarm)),
            findall(Kind, played(Kind, _), Kinds),
            Kinds = [alarm]
        ))
    ).

test(disabled_alarm_does_not_disable_timer) :-
    with_fake_player(
        with_sound_setting(alarm, disabled, (
            once(sound:play_notification_sound(alarm)),
            once(sound:play_notification_sound(timer)),
            findall(Kind, played(Kind, _), Kinds),
            Kinds = [timer]
        ))
    ).

test(disabled_timer_still_notifies_once) :-
    with_fake_player(
        with_sound_setting(timer, disabled, (
            with_fake_notifier((
                alarm:timer_done(tea),
                findall(true, notified, Notifications),
                Notifications = [true],
                \+ played(_, _)
            ))
        ))
    ).

with_fake_player(Goal) :-
    retractall(played(_, _)),
    asserta((sound:sound_player(Kind, Path) :-
             plunit_sounds:record_play(Kind, Path)), PlayerRef),
    call_cleanup(Goal, (
        erase(PlayerRef),
        retractall(played(_, _))
    )).

with_fake_notifier(Goal) :-
    retractall(notified),
    asserta((alarm:completion_notifier(_, _) :-
             plunit_sounds:record_notification), NotifierRef),
    call_cleanup(Goal, (
        erase(NotifierRef),
        retractall(notified)
    )).

record_play(Kind, Path) :-
    assertz(played(Kind, Path)).

record_notification :-
    assertz(notified).

with_sound_setting(timer, Setting, Goal) :-
    asserta(kb_config:timer_sound(Setting), Ref),
    call_cleanup(Goal, erase(Ref)).
with_sound_setting(alarm, Setting, Goal) :-
    asserta(kb_config:alarm_sound(Setting), Ref),
    call_cleanup(Goal, erase(Ref)).

:- end_tests(sounds).
