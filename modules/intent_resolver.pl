:- module(intent_resolver, [
    resolve/3,
    canonicalize_tokens/2,
    convert_number_atoms/2
]).
% todo_capture deprecated; Python skills handle todo capture.
:- use_module('../modules/normalizer', [strip_fillers/2]).
:- use_module(library(lists)).
:- use_module(normalizer).
:- use_module('../kb/intents').
:- use_module('object_intents').

%% ============================================================
%% PUBLIC API
%% ============================================================

resolve(Raw, Intent, Args) :-
    normalizer:normalize_string(Raw, Toks0),
    strip_fillers(Toks0, Core0),
    ( memberchk(timer, Core0)
    -> parse_timer_command(Core0, Args),
       Intent = timer
    ; memberchk(alarm, Core0)
    -> Intent = alarm,
       Args = []
    ; canonicalize_tokens(Toks0, Toks),
      try_exact(Toks, Intent0, Args0),
      object_intents:refine_intent(Intent0, Toks, Intent),
      Args = Args0
    ),
    !.

parse_timer_command(Tokens, [Seconds, Name]) :-
    append(_, [timer|TimerTokens], Tokens),
    append(NameBefore, [AmountToken, UnitToken|NameAfter], TimerTokens),
    number_token(AmountToken, Amount),
    Amount >= 0,
    normalize_unit(UnitToken, Unit),
    unit_seconds(Unit, Amount, Seconds),
    append(NameBefore, NameAfter, NameTokens0),
    exclude(timer_name_label, NameTokens0, NameTokens),
    timer_name(NameTokens, Name),
    !.

number_token(Number, Number) :-
    number(Number), !.
number_token(Token, Number) :-
    atom(Token),
    catch(atom_number(Token, Number), _, fail).

unit_seconds(seconds, Amount, Amount).
unit_seconds(minutes, Amount, Seconds) :-
    Seconds is Amount * 60.
unit_seconds(hours, Amount, Seconds) :-
    Seconds is Amount * 3600.

timer_name_label(called).
timer_name_label(named).
timer_name_label(for).

timer_name([], '').
timer_name(Tokens, Name) :-
    atomic_list_concat(Tokens, ' ', Name).

canonicalize_tokens(Toks0, Toks) :-
    strip_fillers(Toks0, Core0),
    ( select_verb_head(Core0, Verb, Rest)
    -> ( member(Verb, [why, what, how, when, where, who]) ->
        % For question words, keep them in args too
        Toks = [Verb|Rest]
    ; Toks = [Verb|Rest]
    )
    ; Toks = Core0
    ).


%% ============================================================
%% VERB MATCHING
%% ============================================================

try_exact([Word|Rest], Intent, Args) :-
    ( kb_intents:verb_intent(Word, Intent, Arity) ->
        ( Arity = rest, member(Word, [why, what, how, when, where, who]) ->
            % For question words, include them in the args
            Args = [Word|Rest]
        ; extract_args(Arity, Rest, Intent, Args)
        )
    ; fail ).



%% ============================================================
%% VERB SELECTION
%% ============================================================

select_verb_head([W|Rs], W, Rs) :-
    kb_intents:verb_intent(W, _, _), !.
select_verb_head([_|Rs], Verb, Tail) :-
    select_verb_head(Rs, Verb, Tail).


%%============================================================
%%integer Extraction
%%============================================================
convert_number_atoms([], []).

convert_number_atoms([Tok|Rest], [Num|ParsedRest]) :-
    atom(Tok),
    catch(atom_number(Tok, Num), _, fail),
    convert_number_atoms(Rest, ParsedRest).

convert_number_atoms([Tok|Rest], [Tok|ParsedRest]) :-
    convert_number_atoms(Rest, ParsedRest).


%=============================================================
% Units
%=============================================================
/* normalize_unit(+Raw, -Unit)
   Converts different spellings of units to a canonical form.
*/

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

%% ============================================================
%% ARGUMENT EXTRACTION
%% ============================================================

extract_args(0, _, _, []).
extract_args(1, Rest, Intent, [Arg]) :-
    arg1(Intent, Rest, Arg), !.
extract_args(2, Rest, Intent, [A,B]) :-
    arg2(Intent, Rest, A, B), !.
extract_args(rest, Rest, _Intent, Args) :-
    Args = Rest.
extract_args(_, Rest, _Intent, Rest).

arg1(play, Rest, Media) :-
    drop_preps([of,for,to,me,some], Rest, R1),
    head_atom(R1, Media).
arg1(call, Rest, Contact) :-
    drop_preps([to], Rest, R),
    head_atom(R, Contact).
arg1(open, Rest, App) :-
    head_atom(Rest, App).
arg1(resume, _, _) :-
    fail.
arg1(_, Rest, X) :-
    head_atom(Rest, X).

arg2(text, [Contact|MsgParts], Contact, Message) :-
    atomic_list_concat(MsgParts, ' ', Message), !.
arg2(_, Rest, A, B) :-
    Rest = [A|Tail],
    head_atom(Tail, B).

%% ============================================================
%% UTILITY PREDICATES
%% ============================================================

head_atom([], '').
head_atom([H|_], H).

drop_preps([], Rest, Rest).
drop_preps([Prep|Preps], [H|T], Rest) :-
    (H = Prep -> drop_preps(Preps, T, Rest) ; Rest = [H|T]).
drop_preps(Preps, [], []) :-
    Preps = [_|_].
