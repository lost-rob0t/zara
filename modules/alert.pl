:- module(alert,
          [
           alert/4,
           alert/3
          ]).

:- use_module(library(process)).



alert(Title, Urgency, Format, Args) :-
    format(string(Msg), Format, Args),
    catch(
        process_create(path('notify-send'),
                       ['-u', Urgency, '--', Title, Msg],
                       [detached(true)]),
        Error,
        ( format(user_error, 'Alert failed: ~w~n', [Error]),
          fail
        )
    ).

% Default urgency = normal
alert(Title, Format, Args) :-
    alert(Title, normal, Format, Args).
