:- module(intent_resolver, [
    resolve/3,
    canonicalize_tokens/2,
    convert_number_atoms/2   % <-- add this
]).
:- use_module('../modules/todo_capture').
:- use_module('../modules/normalizer', [strip_fillers/2]).
:- use_module(library(lists)).
:- use_module(normalizer).
:- use_module('../kb/intents').
:- use_module('../kb/config').
:- use_module('object_intents').

%% ============================================================
%% PUBLIC API
%% ============================================================

resolve(Raw, Intent, Args) :-
    normalizer:normalize_string(Raw, Toks0),
    strip_fillers(Toks0, Core0),
    (   stop_phrase(Core0)
    ->  Intent = dictation_stop,
        Args = []
    ;   canonicalize_tokens(Toks0, Toks),
        (
            try_exact(Toks, Intent0, Args0)
        ;   try_fuzzy(Toks, Intent0, Args0)
        ),
        object_intents:refine_intent(Intent0, Toks, Intent),
        Args = Args0
    ),
    !.

canonicalize_tokens(Toks0, Toks) :-
    strip_fillers(Toks0, Core0),
    ( select_verb_head(Core0, Verb, Rest)
    -> Toks = [Verb|Rest]
    ; Toks = Core0
    ).

stop_phrase(Tokens) :-
    kb_config:dictation_stop_phrase(Phrase),
    normalizer:normalize_string(Phrase, PhraseTokens),
    Tokens == PhraseTokens,
    !.

%% ============================================================
%% VERB MATCHING
%% ============================================================

try_exact([Word|Rest], Intent, Args) :-
    ( kb_intents:verb_intent(Word, Intent, Arity) ->
        extract_args(Arity, Rest, Intent, Args)
    ; fail ).

try_fuzzy([Word|Rest], Intent, Args) :-
    findall(Surf-Int-Ar, kb_intents:verb_intent(Surf,Int,Ar), VS),
    maplist(score_word(Word), VS, Scored),
    sort(1, @=<, Scored, [Dist-(Surf-Intent-Arity)|_]),
    Dist =< 2,
    extract_args(Arity, Rest, Intent, Args).

score_word(Word, Surf-Int-Ar, Dist-(Surf-Int-Ar)) :-
    edit_distance(Word, Surf, Dist).

%% ============================================================
%% VERB SELECTION
%% ============================================================

select_verb_head([W|Rs], W, Rs) :-
    kb_intents:verb_intent(W, _, _), !.
select_verb_head([W|Rs], Verb, Tail) :-
    findall(Surf, kb_intents:verb_intent(Surf,_,_), Surfs),
    nearest(W, Surfs, Best, D),
    D =< 1, !,
    Verb = Best, Tail = Rs.
select_verb_head([_|Rs], Verb, Tail) :-
    select_verb_head(Rs, Verb, Tail).

nearest(W, [S|Ss], Best, D) :-
    edit_distance(W,S,D0),
    nearest_(W,Ss,S,D0,Best,D).

nearest_(_, [], Best, D, Best, D).
nearest_(W, [S|Ss], CurBest, CurD, Best, D) :-
    edit_distance(W,S,D1),
    (D1 < CurD -> nearest_(W,Ss,S,D1,Best,D)
                ; nearest_(W,Ss,CurBest,CurD,Best,D)).


%%============================================================
%%integer Extraction
%%============================================================
convert_number_atoms([], []).

convert_number_atoms([Tok|Rest], [Num|ParsedRest]) :-
    atom(Tok),
    atom_number(Tok, Num),
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
normalize_unit(h, hours).

%% ============================================================
%% ARGUMENT EXTRACTION
%% ============================================================

extract_args(0, _, _, []).
extract_args(1, Rest, Intent, [Arg]) :-
    arg1(Intent, Rest, Arg), !.
extract_args(2, Rest, Intent, [A,B]) :-
    arg2(Intent, Rest, A, B), !.
extract_args(rest, Rest, _, Rest).
extract_args(_, Rest, _, Rest).

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
% Timer
arg1(timer, Rest, Duration) :-
    extract_number(Rest, Duration), !.
arg1(alarm, Rest, Duration) :-
    extract_number(Rest, Duration), !.

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

%% ============================================================
%% EDIT DISTANCE (Levenshtein)
%% ============================================================

edit_distance(S1, S2, Distance) :-
    atom_chars(S1, L1),
    atom_chars(S2, L2),
    length(L1, Len1),
    length(L2, Len2),
    levenshtein(L1, L2, Len1, Len2, Distance).

levenshtein([], _, 0, Len2, Len2) :- !.
levenshtein(_, [], Len1, 0, Len1) :- !.
levenshtein([H|T1], [H|T2], Len1, Len2, Distance) :- !,
    Len1_1 is Len1 - 1,
    Len2_1 is Len2 - 1,
    levenshtein(T1, T2, Len1_1, Len2_1, Distance).
levenshtein([H1|T1], [H2|T2], Len1, Len2, Distance) :-
    H1 \= H2,
    Len1_1 is Len1 - 1,
    Len2_1 is Len2 - 1,
    levenshtein(T1, T2, Len1_1, Len2_1, D1),
    levenshtein(T1, [H2|T2], Len1_1, Len2, D2),
    levenshtein([H1|T1], T2, Len1, Len2_1, D3),
    Distance is min(D1, min(D2, D3)) + 1.
