:- module(alert,
          [
           alert/4,
           alert/3
          ]).

:- use_module(library(process)).



alert(Title, Urgency, Format, Args) :-
    format(string(Msg), Format, Args),
    format(string(Cmd), "notify-send -u '~w' '~w' '~w'", [Urgency, Title, Msg]),
    shell(Cmd).

% Default urgency = normal
alert(Title, Format, Args) :-
    alert(Title, normal, Format, Args).
