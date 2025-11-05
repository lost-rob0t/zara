
:- module(alarm, [timer_done/1,
                  start_timer/1,
                  start_timer/2]).
:- use_module(library(time)).
:- use_module(alert).   % not "alarm.pl" unless you really named it that
% TODO Implement time parsing to set timers like set an alarm for 3PM



timer_done(Name) :-
    alert("Zara", "Timer ~w has finished", [Name]).

start_timer(Minutes) :-
    Seconds is Minutes * 60,
    alarm(Seconds, timer_done(""), _Id, [remove(true)]).


start_timer(Name, Minutes) :-
    Seconds is Minutes * 60,
    alarm(Seconds, timer_done(Name), Name, [remove(true)]).
