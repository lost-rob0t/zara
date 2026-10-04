:- module(gf_frontend, [gf_canonical_turn/4, gf_reply_tree/2]).

:- use_module('../modules/symbolic_dialogue_turn', [dialogue_turn/4]).

% The canonical conversation owner supplies context. GF supplies text, never
% an executable Prolog goal. Do not call this with unparsed user text as a
% substitute for GF parsing, and do not use it to dispatch device actions.
gf_canonical_turn(Canonical, State, Context, Result) :-
    atom(Canonical),
    atom_length(Canonical, Length),
    Length > 0,
    Length =< 512,
    symbolic_dialogue_turn:dialogue_turn(Canonical, State, Context, Turn),
    Turn = turn(_Frames, Act, _NextContext),
    gf_reply_tree(Act, Tree),
    Result = gf_turn(Turn, Tree,
        evidence(frontend('gf-pilot/v1'), model_calls(0), provider_calls(0))).

gf_reply_tree(greeting, 'GreetingReply') :- !.
gf_reply_tree(answer(expert, _, evidence('builtin-identity/v1')), 'IdentityReply') :- !.
gf_reply_tree(help, 'HelpReply') :- !.
gf_reply_tree(acknowledgement(thanks), 'ThanksReply') :- !.
gf_reply_tree(acknowledgement(acknowledged), 'AcknowledgedReply') :- !.
gf_reply_tree(cancelled, 'CancelledReply') :- !.
gf_reply_tree(clarify(slot(duration)), 'DurationReply') :- !.
gf_reply_tree(clarify(slot(target)), 'TargetReply') :- !.
gf_reply_tree(dispatch_required(frame(intent(ns(device), name('timer.set')), Slots, complete)), Tree) :-
    member(slot(name(duration), value(duration(Seconds)), _), Slots),
    integer(Seconds), Seconds >= 0, Seconds =< 604800,
    gf_duration_unit(Seconds, Count, Unit),
    number_codes(Count, Codes),
    gf_digits_tree(Codes, Digits),
    atomic_list_concat(['TimerPendingReply (DurationOf (', Digits, ') ', Unit, ')'], Tree),
    !.
gf_reply_tree(dispatch_required(_), 'PendingReply') :- !.
gf_reply_tree(_, 'UnsupportedReply').

% Render a verified semantic quantity, never infer execution from parsing.
gf_duration_unit(Seconds, Count, 'Hours') :-
    Seconds > 0, 0 is Seconds mod 3600, !, Count is Seconds // 3600.
gf_duration_unit(Seconds, Count, 'Minutes') :-
    Seconds > 0, 0 is Seconds mod 60, !, Count is Seconds // 60.
gf_duration_unit(Seconds, Seconds, 'Seconds').

% Only decimal digits from a bounded nonnegative integer become constructors.
% Arbitrary user text is never parsed as a GF expression or Prolog goal.
gf_digits_tree([Code], Tree) :-
    gf_digit_constructor(Code, Digit),
    atom_concat('IDig ', Digit, Tree), !.
gf_digits_tree([Code|Codes], Tree) :-
    gf_digit_constructor(Code, Digit),
    gf_digits_tree(Codes, Tail),
    atomic_list_concat(['IIDig ', Digit, ' (', Tail, ')'], Tree).

gf_digit_constructor(Code, Digit) :-
    integer(Code), Code >= 48, Code =< 57,
    atom_codes(Character, [Code]),
    atom_concat('D_', Character, Digit).
