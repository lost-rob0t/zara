:- module(symbolic_dialogue_turn, [dialogue_turn/4]).

:- use_module('../modules/intent_frames', [resolve_frames/4]).
:- use_module('../modules/symbolic_dialogue', [response_act/2, resolve_reference/3]).

% Stateless composition adapter for #1252. Conversation persistence remains
% owned by the canonical conversation projection; this predicate only accepts
% and returns intent_frames' existing clarification context.

dialogue_turn(Text, State, Context0, turn(Frames, Act, Context1)) :-
    dialogue_input(Text, Context0, RoutedText),
    intent_frames:resolve_frames(RoutedText, State, Context0, Frames),
    turn_response(Frames, Context0, Act, Context1),
    !.

dialogue_input(Text,
        partial_frame(frame(_Intent, _Slots, ambiguous(Choices)), _Open),
        RoutedText) :-
    symbolic_dialogue:resolve_reference(Text, Choices, resolved(Choice)),
    !,
    RoutedText = Choice.
dialogue_input(Text, _Context, Text).

turn_response([], Context0, unsupported, Context1) :-
    preserve_open_context(Context0, Context1),
    !.
turn_response([Frame], Context0, Act, Context1) :-
    symbolic_dialogue:response_act(frame(Frame), Act),
    next_dialogue_context(Frame, Context0, Context1),
    !.
turn_response(_, Context0, unsupported, Context1) :-
    preserve_open_context(Context0, Context1).

next_dialogue_context(Frame, _Context0, partial_frame(Frame, Missing)) :-
    Frame = frame(_, _, missing(Missing)),
    !.
next_dialogue_context(Frame, _Context0, partial_frame(Frame, Alternatives)) :-
    Frame = frame(_, _, ambiguous(Alternatives)),
    !.
next_dialogue_context(frame(_, _, invalid(_, _)), Context0, Context1) :-
    preserve_open_context(Context0, Context1),
    !.
next_dialogue_context(_Frame, _Context0, []).

preserve_open_context(partial_frame(Frame, Open), partial_frame(Frame, Open)) :- !.
preserve_open_context(_, []).
