
:- module(alarm, [timer_done/1,
                  start_timer/1,
                  start_timer/2]).
:- use_module(library(time)).
:- use_module(alert).   % not "alarm.pl" unless you really named it that
% TODO Implement time parsing to set timers like set an alarm for 3PM



% Note: timer_done is now handled by Python directly using notify-send
% This predicate is kept for compatibility but does nothing
timer_done(Name) :-
    % Python handles notifications directly for faster response
    format(user_error, 'Timer ~w finished (notification handled by Python)~n', [Name]).

start_timer(Seconds) :-
    alarm(Seconds, timer_done(""), _Id, [remove(true)]).


start_timer(Name, Seconds) :-
    alarm(Seconds, timer_done(Name), _Id, [remove(true)]).
