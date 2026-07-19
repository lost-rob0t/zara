:- begin_tests(sounds).

:- use_module('../modules/sound').
:- use_module('../modules/alarm').
:- use_module('../kb/config').

:- dynamic played/2.

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
    with_fake_player((
        asserta((alarm:completion_notifier(_, _) :- true), NotifierRef),
        call_cleanup(alarm:timer_done(tea), erase(NotifierRef)),
        findall(Path, played(timer, Path), Paths),
        Paths = [_]
    )).

with_fake_player(Goal) :-
    retractall(played(_, _)),
    asserta((sound:sound_player(Kind, Path) :-
             plunit_sounds:record_play(Kind, Path)), PlayerRef),
    call_cleanup(Goal, (
        erase(PlayerRef),
        retractall(played(_, _))
    )).

record_play(Kind, Path) :-
    assertz(played(Kind, Path)).

:- end_tests(sounds).
