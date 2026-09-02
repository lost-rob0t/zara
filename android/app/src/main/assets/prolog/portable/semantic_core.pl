:- module(zara_portable_semantic_core, [
    semantic_contract_version/1,
    resolve_frames/4,
    valid_frame/1,
    normalize_frames/2
]).

:- use_module('../shared/modules/intent_frames', []).

semantic_contract_version('ZARA-SEMANTIC/1').

resolve_frames(Text, _State, _Context, []) :-
    empty_text(Text),
    !.
resolve_frames(Text, State, Context, Frames) :-
    intent_frames:resolve_frames(Text, State, Context, Frames).

empty_text(Text) :-
    ( Text == ""
    ; Text == ''
    ).

valid_frame(frame(intent(ns(Namespace), name(Name)), Slots, Status)) :-
    atom(Namespace),
    atom(Name),
    valid_slots(Slots),
    valid_status(Status).

valid_slots([]).
valid_slots([Slot|Rest]) :-
    valid_slot(Slot),
    valid_slots(Rest).

valid_slot(slot(name(Name), value(Value), origin(Origin))) :-
    atom(Name),
    valid_value(Value),
    valid_origin(Origin).

valid_value(duration(Seconds)) :-
    integer(Seconds),
    Seconds >= 0.
valid_value(text(Text)) :-
    atom(Text).
valid_value(ref(kind(Kind), id(Id))) :-
    atom(Kind),
    atom(Id).

valid_origin(utterance).
valid_origin(follow_up).
valid_origin(correction).
valid_origin(inherited).

valid_status(complete).
valid_status(missing(Names)) :-
    atoms(Names).
valid_status(invalid(value(Name), Reason)) :-
    atom(Name),
    atom(Reason).
valid_status(ambiguous(Alternatives)) :-
    atoms(Alternatives).

atoms([]).
atoms([Atom|Rest]) :-
    atom(Atom),
    atoms(Rest).

normalize_frames([], []).
normalize_frames([Frame0|Rest0], [Frame|Rest]) :-
    normalize_frame(Frame0, Frame),
    normalize_frames(Rest0, Rest).

normalize_frame(
    frame(Intent, Slots0, Status),
    frame(Intent, Slots, Status)
) :-
    valid_frame(frame(Intent, Slots0, Status)),
    sort(Slots0, Slots).
