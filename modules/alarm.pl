
:- module(alarm, [timer_done/1, start_timer/1, start_timer/2]).
:- use_module(library(time)).
:- use_module(alert).
:- use_module(sound).

:- dynamic timer_scheduler/3.
:- dynamic completion_notifier/2.

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
    timer_scheduler(Seconds, timer_done(Name), _).

timer_scheduler(Seconds, Goal, Id) :-
    alarm(Seconds, Goal, Id, [remove(true)]).

completion_notifier(_, Message) :-
    alert:alert('Zara Timer', normal, '~w', [Message]).

timer_message('', "Timer finished.").
timer_message("", "Timer finished.").
timer_message(Name, Message) :-
    format(string(Message), 'Timer "~w" finished.', [Name]).
