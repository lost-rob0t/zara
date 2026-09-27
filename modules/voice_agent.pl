:- module(voice_agent, [route/3]).
:- use_module('../kb/voice_agent').
:- use_module(library(lists)).

route(Text, Route, Argument) :-
    string(Text), string_length(Text, Size), Size > 0, Size =< 12000,
    string_lower(Text, Lower),
    split_string(Lower, " \t\r\n", " \t\r\n.,!?", Words),
    maplist(atom_string, Tokens, Words),
    strip_courtesy(Tokens, 4, Clean),
    classify(Clean, Text, Route, Argument), !.

strip_courtesy(Tokens, Remaining, Clean) :-
    Remaining > 0, kb_voice_agent:courtesy(Prefix),
    append(Prefix, Rest, Tokens), Rest \= [], !,
    Next is Remaining - 1,
    strip_courtesy(Rest, Next, Clean).
strip_courtesy(Tokens, _, Tokens).

classify(Tokens, _, Route, Argument) :-
    kb_voice_agent:control_phrase(Tokens, Route), !,
    ( memberchk(Route, [pause_task, cancel_task, resume_task])
    -> Argument = "current"
    ; Argument = ""
    ).
classify([Verb,task,Id], _, Route, Argument) :-
    memberchk(Verb-Route, [pause-pause_task,cancel-cancel_task,stop-cancel_task,resume-resume_task]),
    atom_length(Id, Length), Length > 5, Length =< 64,
    sub_atom(Id, 0, 5, _, 'task-'), !,
    atom_string(Id, Argument).
classify(Tokens, Text, new_task, Text) :-
    kb_voice_agent:task_prefix(Prefix), append(Prefix, Goal, Tokens), Goal \= [], !.
classify([Verb|Goal], Text, new_task, Text) :-
    kb_voice_agent:action_verb(Verb), Goal \= [], !.
classify(_, _, chat, "").
