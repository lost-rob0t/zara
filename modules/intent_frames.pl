:- module(intent_frames, [resolve_frames/4, frame_head_row/9, frame_slot_row/8, cancel_phrase/1]).
% Pure IntentFrame resolution (issue #156, contract docs/intentframe-contract.md).
%
% resolve_frames(+Text, +State, +Context, -Frames)
%   Text    atom or string utterance
%   State   passive | conversation | dictation (unchanged gate semantics)
%   Context [] or partial_frame(Frame0, Missing) with an open clarification
%           session frame
%   Frames  list of frame/3 terms (contract section 2, portable subset:
%           plain compounds + lists only)
%
% Pure: no assert/retract, no dynamic goals from user strings, no I/O.

:- use_module('../modules/normalizer', [normalize_string/2, strip_fillers/2]).
:- use_module('../kb/intents').

%% ============================================================
%% Entry point
%% ============================================================

resolve_frames(Text, State, Context, Frames) :-
    atom_string(TextAtom, Text),
    normalize_string(TextAtom, RawTokens),
    ( Context = partial_frame(Frame0, _) ->
        resolve_follow_up(RawTokens, Frame0, Frames)
    ; resolve_fresh(RawTokens, State, Frames)
    ),
    !.

%% ============================================================
%% Fresh resolution
%% ============================================================

resolve_fresh(RawTokens, _State, [Frame]) :-
    cancellation_tokens(RawTokens, _),
    !,
    cancel_frame(Frame).
resolve_fresh(RawTokens, State, [Frame]) :-
    state_control_frame(State, RawTokens, Frame),
    !.
resolve_fresh(RawTokens, _State, Frames) :-
    correction_tokens(RawTokens, Tail),
    !,
    strip_fillers(Tail, Core),
    frame_for_core(Core, correction, Frames).
resolve_fresh(RawTokens, _State, Frames) :-
    strip_fillers(RawTokens, Core),
    frame_for_core(Core, follow_up, Frames).

%% ============================================================
%% Cancellation (contract section 4; host closes the open frame)
%% ============================================================

cancel_phrase("never mind").
cancel_phrase("nevermind").
cancel_phrase("forget it").
cancel_phrase("cancel").
cancel_phrase("cancel that").

cancellation_tokens(Tokens, cancel) :-
    append([never, mind], _, Tokens).
cancellation_tokens(Tokens, cancel) :-
    append([nevermind], _, Tokens).
cancellation_tokens(Tokens, cancel) :-
    append([forget, it], _, Tokens).
cancellation_tokens(Tokens, cancel) :-
    append([cancel], _, Tokens).
cancellation_tokens(Tokens, cancel) :-
    append([cancel, that], _, Tokens).

cancel_frame(frame(intent(ns(conversation), name(cancel)), [], complete)).

%% ============================================================
%% State gates (legacy state_control semantics, projected)
%% ============================================================

state_control_frame(passive, [stop|_],
    frame(intent(ns(conversation), name(end)), [], complete)).
state_control_frame(passive, [end|_],
    frame(intent(ns(conversation), name(end)), [], complete)).
state_control_frame(conversation, [Word|_], Frame) :-
    memberchk(Word, [stop, end]),
    Frame = frame(intent(ns(conversation), name(end)), [], complete).
state_control_frame(dictation, [Word|_], Frame) :-
    memberchk(Word, [stop, end, disable, deactivate]),
    Frame = frame(intent(ns(dictation), name(stop)), [], complete).

%% ============================================================
%% Correction markers (detected pre-filler-strip; "actually" is a filler)
%% ============================================================

correction_tokens([actually|Tail], Tail) :- Tail = [_|_].
correction_tokens([no|Tail], Tail) :- Tail = [_|_].
correction_tokens([make, that|Tail], Tail) :- Tail = [_|_].
correction_tokens([make, it|Tail], Tail) :- Tail = [_|_].
correction_tokens([change, that, to|Tail], Tail) :- Tail = [_|_].
correction_tokens([change, it, to|Tail], Tail) :- Tail = [_|_].

%% ============================================================
%% Verb head selection (KB single verb authority; verb_intent heads
%% outrank python_skill_intent heads so "set a timer" never becomes a
%% schedule_todo hijack)
%% ============================================================

frame_head(Tokens, head(Word, Intent, Arity), Rest) :-
    scan_verb_head(Tokens, Word, Intent, Arity, Rest),
    !.
frame_head(Tokens, head(Word, python(Skill), rest), Rest) :-
    scan_skill_head(Tokens, Word, Skill, Rest),
    !.

scan_verb_head([Word|Rest], Word, Intent, Arity, Rest) :-
    kb_intents:verb_intent(Word, Intent, Arity).
scan_verb_head([_|Tokens], Word, Intent, Arity, Rest) :-
    scan_verb_head(Tokens, Word, Intent, Arity, Rest).

scan_skill_head([Word|Rest], Word, Skill, Rest) :-
    kb_intents:python_skill_intent(Word, Skill, _).
scan_skill_head([_|Tokens], Word, Skill, Rest) :-
    scan_skill_head(Tokens, Word, Skill, Rest).

frame_head_([Word|_]) :- kb_intents:verb_intent(Word, _, _).
frame_head_([Word|_]) :- kb_intents:python_skill_intent(Word, _, _).
frame_head_([_|Tokens]) :- frame_head_(Tokens).

%% ============================================================
%% Core -> frame
%% ============================================================

frame_for_core([], _Correction, []).
frame_for_core(Core, Correction, [Frame]) :-
    append(_, [timer|Rest], Core),
    !,
    frame_from_args(timer, Rest, Correction, Frame).
frame_for_core(Core, Correction, [Frame]) :-
    frame_head_(Core),
    !,
    frame_head(Core, head(Head, Intent, _Arity), Rest),
    project_frame(Head, Intent, Rest, Correction, Frame).
frame_for_core([_|_], _Correction, []).
project_frame(_Head, Intent, Rest, Correction, Frame) :-
    frame_from_args(Intent, Rest, Correction, Frame).

%% ============================================================
%% Frame construction per intent family
%% ============================================================

% python(Skill) intents unwrap to their skill name.
frame_from_args(python(Skill), Rest, Correction, Frame) :-
    frame_from_args(Skill, Rest, Correction, Frame).

% No-arg intents.
frame_from_args(lock, [], _Correction,
    frame(intent(ns(device), name(lock)), [], complete)).
frame_from_args(unlock, [], _Correction,
    frame(intent(ns(device), name(unlock)), [], complete)).
frame_from_args(screenshot, [], _Correction,
    frame(intent(ns(device), name('screen.capture')), [], complete)).
frame_from_args(pause, [], _Correction,
    frame(intent(ns(media), name(pause)), [], complete)).
frame_from_args(resume, [], _Correction,
    frame(intent(ns(media), name(resume)), [], complete)).
frame_from_args(next, [], _Correction,
    frame(intent(ns(media), name(next)), [], complete)).
frame_from_args(skip, [], _Correction,
    frame(intent(ns(media), name(skip)), [], complete)).
frame_from_args(dictation_start, [], _Correction,
    frame(intent(ns(dictation), name(start)), [], complete)).
frame_from_args(greet, [], _Correction,
    frame(intent(ns(conversation), name(greet)), [], complete)).

% rest-arg intents with a single required slot.
frame_from_args(search, Rest, _Correction, Frame) :-
    ( member(TodoWord, Rest),
        kb_todo_word(TodoWord) ->
        omit_first(Rest, TodoWord, QueryTokens),
        bounded_atom(QueryTokens, QueryAtom),
        Frame = frame(intent(ns(skill), name(search_todos)),
            [slot(name(query), value(text(QueryAtom)), origin(utterance))],
            complete)
    ; rest_slot_frame(search, web, query, text, Rest, Frame)
    ).
frame_from_args(ask, Rest, _Correction, Frame) :-
    rest_slot_frame(ask, conversation, query, text, Rest, Frame).
frame_from_args(alarm, Rest, _Correction, Frame) :-
    rest_slot_frame(alarm, device, time, text, Rest, Frame).
frame_from_args(navigate, Rest, _Correction, Frame) :-
    rest_slot_frame(navigate, web, destination, text, Rest, Frame).
frame_from_args(schedule_todo, Rest, _Correction, Frame) :-
    rest_slot_frame(schedule_todo, skill, task, text, Rest, Frame).
frame_from_args(play, Rest, _Correction, Frame) :-
    rest_slot_frame(play, media, target, ref(media_alias), Rest, Frame).

% open: rest tokens are the target alias.
frame_from_args(open, Rest, _Correction, Frame) :-
    rest_slot_frame(open, app, target, ref(app_alias), Rest, Frame).

kb_todo_word(todo).
kb_todo_word(todos).
kb_todo_word(task).
kb_todo_word(tasks).

omit_first([], _Word, []).
omit_first([Word|Rest], Word, Rest) :- !.
omit_first([Other|Rest], Word, [Other|RestOmitted]) :-
    Other \= Word,
    omit_first(Rest, Word, RestOmitted).

% timer: bare (empty rest) -> typed missing-duration frame.
frame_from_args(timer, [], _Correction,
    frame(intent(ns(device), name('timer.set')), [], missing([duration]))).

% timer with args: duration + optional label.
frame_from_args(timer, Rest, _Correction, Frame) :-
    Rest = [_|_],
    timer_duration_tokens(Rest, Amount, Unit),
    !,
    ( Amount >= 0 ->
        normalize_unit(Unit, UnitNorm),
        unit_seconds(UnitNorm, Amount, Seconds),
        DurationSlot = slot(name(duration),
                            value(duration(Seconds)),
                            origin(utterance)),
        ( timer_label_slot(Rest, LabelSlot) ->
            ( LabelSlot = none ->
                Frame = frame(intent(ns(device), name('timer.set')),
                              [DurationSlot], complete)
            ; Frame = frame(intent(ns(device), name('timer.set')),
                            [DurationSlot, LabelSlot], complete)
            )
        ; Frame = frame(intent(ns(device), name('timer.set')),
                        [DurationSlot], complete)
        )
    ; Frame = frame(intent(ns(device), name('timer.set')), [],
                    invalid(value(duration), negative))
    ).

% text/send: contact ref + message text.
frame_from_args(text, Rest, _Correction, Frame) :-
    send_slots(Rest, Slots, Missing),
    ( Missing = [] ->
        Frame = frame(intent(ns(message), name(send)), Slots, complete)
    ; Frame = frame(intent(ns(message), name(send)), Slots, missing(Missing))
    ).

%% ============================================================
%% Timer duration/label helpers
%% ============================================================

timer_duration_tokens(Tokens, Amount, Unit) :-
    append(_, [AmountToken, UnitToken|_], Tokens),
    number_token(AmountToken, Amount),
    normalize_unit(UnitToken, Unit).

timer_label_slot(Rest, none) :-
    timer_duration_tokens(Rest, _, _),
    \+ label_rest(Rest, [_|_]).
timer_label_slot(Rest, slot(name(label), value(text(Atom)), origin(utterance))) :-
    label_rest(Rest, LabelTokens),
    LabelTokens = [_|_],
    bounded_atom(LabelTokens, Atom).

label_rest(Rest, LabelTokens) :-
    append(_, [called|LabelTokens], Rest),
    LabelTokens = [_|_],
    !.
label_rest(Rest, LabelTokens) :-
    append(_, [named|LabelTokens], Rest),
    LabelTokens = [_|_],
    !.
label_rest([_Amount, _Unit|LabelTokens0], LabelTokens) :-
    drop_label_markers(LabelTokens0, LabelTokens).

drop_label_markers([for|T], T) :- !.
drop_label_markers(T, T).

%% ============================================================
%% Send (text) slots
%% ============================================================

send_slots([], [], [contact, message]).
send_slots([ContactToken|Rest], Slots, Missing) :-
    ( Rest = [] ->
        Slots = [slot(name(contact),
                      value(ref(kind(contact), id(ContactToken))),
                      origin(utterance))],
        Missing = [message]
    ; bounded_atom(Rest, MessageAtom),
      Slots = [slot(name(contact),
                    value(ref(kind(contact), id(ContactToken))),
                    origin(utterance)),
               slot(name(message),
                    value(text(MessageAtom)),
                    origin(utterance))],
      Missing = []
    ).

%% ============================================================
%% Single rest-arg slot family
%% ============================================================

bounded_atom(Tokens, Atom) :-
    atomic_list_concat(Tokens, ' ', Atom),
    atom_length(Atom, Length),
    Length =< 512.

rest_slot_frame(IntentName, NS, SlotName, text, Rest, Frame) :-
    ( Rest = [] ->
        Frame = frame(intent(ns(NS), name(IntentName)), [], missing([SlotName]))
    ; bounded_atom(Rest, ValueAtom),
      Frame = frame(intent(ns(NS), name(IntentName)),
          [slot(name(SlotName), value(text(ValueAtom)), origin(utterance))],
          complete)
    ).
rest_slot_frame(IntentName, NS, SlotName, ref(Kind), Rest, Frame) :-
    ( Rest = [] ->
        Frame = frame(intent(ns(NS), name(IntentName)), [], missing([SlotName]))
    ; bounded_atom(Rest, IdAtom),
      Frame = frame(intent(ns(NS), name(IntentName)),
          [slot(name(SlotName), value(ref(kind(Kind), id(IdAtom))), origin(utterance))],
          complete)
    ).

%% ============================================================
%% Follow-up resolution (context mode)
%% ============================================================

resolve_follow_up(RawTokens, _Frame0, [Frame]) :-
    cancellation_tokens(RawTokens, _),
    !,
    cancel_frame(Frame).
resolve_follow_up(RawTokens, Frame0, [Frame]) :-
    correction_tokens(RawTokens, Tail0),
    !,
    strip_fillers(Tail0, Core),
    follow_up_value(Frame0, Core, correction, Frame).
resolve_follow_up(RawTokens, Frame0, [Frame]) :-
    strip_fillers(RawTokens, Core),
    follow_up_value(Frame0, Core, follow_up, Frame).

follow_up_value(frame(Intent0, Slots0, ambiguous(Alternatives)), Core,
        _DefaultOrigin, Frame) :-
    Alternatives = [_|_],
    !,
    atomic_list_concat(Core, ' ', AnswerAtom),
    downcase_atom(AnswerAtom, Answer),
    ( member(Alt, Alternatives),
      downcase_atom(Alt, Answer),
      Intent0 = intent(ns(_NS), name(IntentName)),
      first_unfilled_required(IntentName, Slots0, SlotName),
      slot_value_type(SlotName, SlotType),
      parse_slot_value(SlotType, Core, ok(Value)),
      merge_slot_values(Slots0, SlotName, Value, follow_up, Slots),
      missing_required(IntentName, Slots, Missing),
      ( Missing = [] -> Status = complete ; Status = missing(Missing) ),
      Frame = frame(Intent0, Slots, Status)
    ; Frame = frame(Intent0, Slots0, ambiguous(Alternatives))
    ),
    !.
follow_up_value(frame(Intent0, Slots0, ambiguous(Alternatives)), _Core,
        _DefaultOrigin, frame(Intent0, Slots0, ambiguous(Alternatives))) :-
    Alternatives = [_|_],
    !.
follow_up_value(frame(Intent0, Slots0, _Status0), Core, DefaultOrigin, Frame) :-
    Intent0 = intent(ns(_NS), name(IntentName)),
    first_unfilled_required(IntentName, Slots0, SlotName),
    !,
    slot_value_type(SlotName, SlotType),
    ( parse_slot_value(SlotType, Core, ok(Value)) ->
        ( slot_member(Slots0, SlotName) ->
            Origin = correction
        ; DefaultOrigin == correction ->
            Origin = correction
        ; Origin = follow_up
        ),
        merge_slot_values(Slots0, SlotName, Value, Origin, Slots),
        missing_required(IntentName, Slots, Missing),
        ( Missing = [] -> Status = complete ; Status = missing(Missing) ),
        Frame = frame(Intent0, Slots, Status)
    ; slot_value_type(SlotName, SlotType),
      parse_slot_value(SlotType, Core, bad(Reason)),
      Frame = frame(Intent0, Slots0, invalid(value(SlotName), Reason))
    ).
follow_up_value(frame(Intent0, Slots0, complete), Core, correction, Frame) :-
    Intent0 = intent(ns(_NS), name(IntentName)),
    frame_slot_spec(IntentName, SlotName, SlotType, _),
    slot_member(Slots0, SlotName),
    parse_slot_value(SlotType, Core, ok(Value)),
    replace_slot(Slots0, SlotName, Value, correction, Slots),
    \+ (   frame_slot_spec(IntentName, Required, _, required),
           \+ slot_member(Slots, Required)
       ),
    Frame = frame(Intent0, Slots, complete),
    !.
follow_up_value(Frame0, _Core, _DefaultOrigin, Frame0).

first_unfilled_required(IntentName, Slots, SlotName) :-
    frame_slot_spec(IntentName, SlotName, _, required),
    \+ slot_member(Slots, SlotName),
    !.

frame_slot_spec('timer.set', duration, duration, required).
frame_slot_spec('timer.set', label, text, optional).
frame_slot_spec(open, target, ref(app_alias), required).
frame_slot_spec(send, contact, ref(contact), required).
frame_slot_spec(send, message, text, required).
frame_slot_spec(search, query, text, required).
frame_slot_spec(schedule_todo, task, text, required).
frame_slot_spec(alarm, time, text, required).
frame_slot_spec(navigate, destination, text, required).
frame_slot_spec(play, target, ref(media_alias), required).
frame_slot_spec(ask, query, text, required).

slot_value_type(duration, duration).
slot_value_type(label, text).
slot_value_type(target, ref(app_alias)).
slot_value_type(contact, ref(contact)).
slot_value_type(message, text).
slot_value_type(query, text).
slot_value_type(task, text).
slot_value_type(time, text).
slot_value_type(destination, text).

parse_slot_value(duration, Tokens, Result) :-
    ( timer_duration_tokens(Tokens, Amount, Unit) ->
        ( Amount >= 0 ->
            normalize_unit(Unit, UnitNorm),
            unit_seconds(UnitNorm, Amount, Seconds),
            Result = ok(duration(Seconds))
        ; Result = bad(negative)
        )
    ; Result = bad(unparseable)
    ).
parse_slot_value(text, Tokens, Result) :-
    ( Tokens = [] ->
        Result = bad(empty)
    ; atomic_list_concat(Tokens, ' ', Atom),
      atom_length(Atom, Length),
      Length =< 512,
      Result = ok(text(Atom))
    ).
parse_slot_value(ref(Kind), Tokens, Result) :-
    ( Tokens = [] ->
        Result = bad(empty)
    ; length(Tokens, N), N =< 2,
      atomic_list_concat(Tokens, ' ', IdAtom),
      Result = ok(ref(kind(Kind), id(IdAtom)))
    ).

merge_slot_values(Slots0, SlotName, Value, Origin, Slots) :-
    ( slot_member(Slots0, SlotName) ->
        replace_slot(Slots0, SlotName, Value, Origin, Slots)
    ; append(Slots0, [slot(name(SlotName), value(Value), origin(Origin))], Slots)
    ).

slot_member([slot(name(SlotName), _, _)|_], SlotName).
slot_member([slot(name(Other), _, _)|Rest], SlotName) :-
    Other \= SlotName,
    slot_member(Rest, SlotName).

replace_slot([_|Rest], SlotName, Value, Origin,
        [slot(name(SlotName), value(Value), origin(Origin))|Rest]).

missing_required(IntentName, Slots, Missing) :-
    findall(Name,
        ( frame_slot_spec(IntentName, Name, _, required),
          \+ slot_member(Slots, Name)
        ),
        Missing).

%% ============================================================
%% Flat projection for marshalling (pyswip stringifies nested compounds;
%% only atoms, integers and lists of atoms survive the boundary reliably)
%% ============================================================

frame_head_row(Frames, Idx, NS, Name, StatusKind, Missing, Alternatives, InvalidSlot, InvalidReason) :-
    nth0(Idx, Frames, frame(intent(ns(NS), name(Name)), _Slots, Status)),
    status_fields(Status, StatusKind, Missing, Alternatives, InvalidSlot, InvalidReason).

status_fields(complete, complete, [], [], none, none).
status_fields(missing(M), missing, M, [], none, none).
status_fields(ambiguous(A), ambiguous, [], A, none, none).
status_fields(invalid(value(S), R), invalid, [], [], S, R).
status_fields(cancelled, cancelled, [], [], none, none).
status_fields(superseded(_), superseded, [], [], none, none).

frame_slot_row(Frames, Idx, SlotIdx, SlotName, Origin, ValueKind, A1, A2) :-
    nth0(Idx, Frames, frame(_Intent, Slots, _Status)),
    nth0(SlotIdx, Slots, slot(name(SlotName), value(Value), origin(Origin))),
    ( Value = text(A1) -> ValueKind = text, A2 = none
    ; Value = duration(A1) -> ValueKind = duration, A2 = none
    ; Value = number(A1) -> ValueKind = number, A2 = none
    ; Value = boolean(A1) -> ValueKind = boolean, A2 = none
    ; Value = ref(kind(A1), id(A2)) -> ValueKind = ref
    ; Value = datetime(Y, Mo, D, H, Mi, S) ->
        ValueKind = datetime, A1 = [Y, Mo, D, H, Mi, S], A2 = none
    ).

%% ============================================================
%% Shared token/number/unit helpers (portable subset; legacy copies in
%% intent_resolver stay until the legacy path retires in #157/#160)
%% ============================================================

number_token(Number, Number) :-
    number(Number), !.
number_token(Token, Number) :-
    atom(Token),
    catch(atom_number(Token, Number), _, fail).

normalize_unit(minutes, minutes).
normalize_unit(minute, minutes).
normalize_unit(min, minutes).
normalize_unit(seconds, seconds).
normalize_unit(second, seconds).
normalize_unit(sec, seconds).
normalize_unit(s, seconds).
normalize_unit(hours, hours).
normalize_unit(hour, hours).
normalize_unit(hr, hours).
normalize_unit(hrs, hours).
normalize_unit(h, hours).

unit_seconds(seconds, Amount, Amount).
unit_seconds(minutes, Amount, Seconds) :-
    Seconds is Amount * 60.
unit_seconds(hours, Amount, Seconds) :-
    Seconds is Amount * 3600.
