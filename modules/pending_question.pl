:- module(pending_question, [
    set_pending_question/5,
    get_pending_question/5,
    get_latest_pending_question/5,
    clear_pending_question/1
]).

:- dynamic pending_question/5.
:- dynamic last_question_id/1.

next_question_id(Id) :-
    ( last_question_id(Prev) -> Id is Prev + 1 ; Id = 1 ),
    retractall(last_question_id(_)),
    assertz(last_question_id(Id)).

set_pending_question(Intent, MissingSlots, Prompt, Context, Id) :-
    next_question_id(Id),
    retractall(pending_question(_, _, _, _, _)),
    assertz(pending_question(Id, Intent, MissingSlots, Prompt, Context)).

get_pending_question(Id, Intent, MissingSlots, Prompt, Context) :-
    pending_question(Id, Intent, MissingSlots, Prompt, Context).

get_latest_pending_question(Id, Intent, MissingSlots, Prompt, Context) :-
    last_question_id(Id),
    pending_question(Id, Intent, MissingSlots, Prompt, Context).

clear_pending_question(Id) :-
    retractall(pending_question(Id, _, _, _, _)).
