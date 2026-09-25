:- module(symbolic_dialogue_turn, [dialogue_turn/4, valid_dialogue_context/1]).

:- use_module('../modules/intent_frames', [resolve_frames/4]).
:- use_module('../modules/normalizer', [normalize_string/2]).
:- use_module('../modules/symbolic_dialogue', [response_act/2, resolve_reference/3]).

% Stateless composition adapter for #1252. Conversation persistence remains
% owned by the canonical conversation projection; this predicate only accepts
% and returns semantic dialogue context. partial_frame/2 remains the frozen
% IntentFrame clarification context. completed_frame/1 is a dialogue-layer
% snapshot supplied by the canonical conversation owner so a later correction
% can reuse the prior semantic frame without creating another history store.
% Runtime/project generations, cancellation and principal/conversation fencing
% remain the caller's responsibility at that canonical projection boundary.
%
% valid_dialogue_context/1 is the shared runtime-side trust boundary for a
% context restored from durable storage. It intentionally accepts only the
% three context shapes emitted by dialogue_turn/4. Adapters may serialize the
% ground term, but they must validate the decoded term here before routing a
% follow-up. This keeps persistence language-neutral without making raw stored
% Prolog executable.
%
% The bounded social vocabulary below is part of this canonical Prolog router;
% it does not create a second parser, history store, or provider fallback. It
% exists here because these conversational turns do not belong to an effect or
% command frame. Open/completed semantic context is preserved across them.

valid_dialogue_context([]).
valid_dialogue_context(partial_frame(Frame, Open)) :-
    ground(Frame),
    ground(Open),
    valid_partial_context(Frame, Open).
valid_dialogue_context(completed_frame(Frame)) :-
    ground(Frame),
    Frame = frame(_Intent, Slots, complete),
    bounded_context_list(Slots).

valid_partial_context(frame(_Intent, Slots, missing(Missing)), Open) :-
    bounded_context_list(Slots),
    bounded_context_list(Missing),
    Open == Missing.
valid_partial_context(frame(_Intent, Slots, ambiguous(Choices)), Open) :-
    bounded_context_list(Slots),
    bounded_context_list(Choices),
    Open == Choices.

bounded_context_list(List) :-
    is_list(List),
    length(List, Length),
    Length =< 64.

dialogue_turn(Text, _State, Context0, turn([Frame], Act, Context1)) :-
    valid_dialogue_context(Context0),
    conversation_vocabulary_frame(Text, Frame),
    symbolic_dialogue:response_act(frame(Frame), Act),
    preserve_dialogue_context(Context0, Context1),
    !.
dialogue_turn(Text, State, Context0, turn(Frames, Act, Context1)) :-
    valid_dialogue_context(Context0),
    dialogue_frames(Text, State, Context0, Frames),
    turn_response(Frames, Context0, Act, Context1),
    valid_dialogue_context(Context1),
    !.

% The frozen IntentFrame API only accepts [] or partial_frame/2. To correct a
% completed prior frame we reuse that canonical correction parser internally,
% but only accept its result when it actually changed the completed frame.
% Otherwise the utterance is resolved fresh, so an unrelated command cannot be
% misread as a follow-up merely because a completed frame is available.
dialogue_frames(Text, State, completed_frame(Frame0), Frames) :-
    intent_frames:resolve_frames(Text, State, partial_frame(Frame0, []), CandidateFrames),
    ( CandidateFrames = [Candidate], Candidate == Frame0 ->
        intent_frames:resolve_frames(Text, State, [], Frames)
    ; Frames = CandidateFrames
    ),
    !.
dialogue_frames(Text, State, Context0, Frames) :-
    dialogue_input(Text, Context0, RoutedText),
    intent_frames:resolve_frames(RoutedText, State, Context0, Frames).

dialogue_input(Text,
        partial_frame(frame(_Intent, _Slots, ambiguous(Choices)), _Open),
        RoutedText) :-
    symbolic_dialogue:resolve_reference(Text, Choices, resolved(Choice)),
    !,
    RoutedText = Choice.
dialogue_input(Text, _Context, Text).

conversation_vocabulary_frame(Text, Frame) :-
    text_atom(Text, Atom),
    normalize_string(Atom, Tokens),
    conversation_vocabulary_tokens(Tokens, Intent),
    Frame = frame(intent(ns(conversation), name(Intent)), [], complete).

conversation_vocabulary_tokens([help], help).
conversation_vocabulary_tokens([help, me], help).
conversation_vocabulary_tokens([what, can, you, do], help).
conversation_vocabulary_tokens([thanks], thanks).
conversation_vocabulary_tokens([thank, you], thanks).
conversation_vocabulary_tokens([thank, you, very, much], thanks).
conversation_vocabulary_tokens([cheers], thanks).
conversation_vocabulary_tokens([ok], acknowledge).
conversation_vocabulary_tokens([okay], acknowledge).
conversation_vocabulary_tokens([alright], acknowledge).
conversation_vocabulary_tokens([sure], acknowledge).
conversation_vocabulary_tokens([got, it], acknowledge).
conversation_vocabulary_tokens([sounds, good], acknowledge).
conversation_vocabulary_tokens([understood], acknowledge).

text_atom(Text, Text) :-
    atom(Text),
    !.
text_atom(Text, Atom) :-
    string(Text),
    string_codes(Text, Codes),
    atom_codes(Atom, Codes).

turn_response([], Context0, unsupported, Context1) :-
    preserve_dialogue_context(Context0, Context1),
    !.
turn_response([Frame], Context0, Act, Context1) :-
    symbolic_dialogue:response_act(frame(Frame), Act),
    next_dialogue_context(Frame, Context0, Context1),
    !.
turn_response(_, Context0, unsupported, Context1) :-
    preserve_dialogue_context(Context0, Context1).

next_dialogue_context(Frame, _Context0, partial_frame(Frame, Missing)) :-
    Frame = frame(_, _, missing(Missing)),
    !.
next_dialogue_context(Frame, _Context0, partial_frame(Frame, Alternatives)) :-
    Frame = frame(_, _, ambiguous(Alternatives)),
    !.
next_dialogue_context(frame(_, _, invalid(_, _)), Context0, Context1) :-
    preserve_dialogue_context(Context0, Context1),
    !.
next_dialogue_context(frame(intent(ns(conversation), name(cancel)), _, complete), _Context0, []) :-
    !.
next_dialogue_context(frame(intent(ns(conversation), name(end)), _, complete), _Context0, []) :-
    !.
next_dialogue_context(frame(intent(ns(conversation), name(greet)), _, complete), Context0, Context1) :-
    preserve_dialogue_context(Context0, Context1),
    !.
next_dialogue_context(Frame, _Context0, completed_frame(Frame)) :-
    Frame = frame(_, _, complete),
    !.
next_dialogue_context(_Frame, Context0, Context1) :-
    preserve_dialogue_context(Context0, Context1).

preserve_dialogue_context(partial_frame(Frame, Open), partial_frame(Frame, Open)) :- !.
preserve_dialogue_context(completed_frame(Frame), completed_frame(Frame)) :- !.
preserve_dialogue_context(_, []).
