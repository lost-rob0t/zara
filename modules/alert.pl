:- module(alert,
          [
           alert/4,
           alert/3
          ]).

:- use_module(library(process)).



alert(Title, Urgency, Format, Args) :-
    memberchk(Urgency, [low, normal, critical]),
    format(string(Msg), Format, Args),
    catch(
        process_create(path('notify-send'),
                       ['-u', Urgency, '--', Title, Msg],
                       [detached(true), process(Process)]),
        Error,
        ( format(user_error, 'Alert failed: ~w~n', [Error]),
          fail
        )
    ),
    process_wait(Process, Status, [timeout(0.5)]),
    ( memberchk(Status, [exit(0), timeout])
    -> true
    ; format(user_error, 'Alert failed: notify-send returned ~w~n', [Status]),
      fail
    ).

% Default urgency = normal
alert(Title, Format, Args) :-
    alert(Title, normal, Format, Args).
