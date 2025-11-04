:- module(intent_resolver,
    [ resolve/3                 % +Raw:string, -Intent, -Args:list(atom)
    , canonicalize_tokens/2     % +Tokens, -CanonTokens (verb first)
    ]).
:- use_module('../modules/todo_capture').
:- use_module('../modules/normalizer', [strip_fillers/2]).
:- use_module(library(lists)).
:- use_module(normalizer).
:- use_module('../kb/intents').  % uses verb_intent/3 facts

% resolve/3:
% 1) normalize + strip fillers
% 2) try direct verb match
% 3) fuzzy verb match (edit distance <= 2)
% 4) heuristic argument extraction patterns
resolve(Raw, Intent, Args) :-
    normalizer:normalize_string(Raw, Toks0),
    canonicalize_tokens(Toks0, Toks),
    ( try_exact(Toks, Intent, Args)
    ; try_fuzzy(Toks, Intent, Args)
    ), !.

% Put a likely verb first: drop leading fillers then promote first known/similar verb
canonicalize_tokens(Toks0, Toks) :-
    strip_fillers(Toks0, Core0),
    ( select_verb_head(Core0, Verb, Rest)
    -> Toks = [Verb|Rest]
    ; Toks = Core0
    ).

% --- Stage 1: exact verb match ---

try_exact([Word|Rest], Intent, Args) :-
    ( kb_intents:verb_intent(Word, Intent, Arity) ->
        extract_args(Arity, Rest, Intent, Args)
    ; fail ).

% --- Stage 2: fuzzy verb match ---

try_fuzzy([Word|Rest], Intent, Args) :-
    findall(Surf-Int-Ar, kb_intents:verb_intent(Surf,Int,Ar), VS),
    maplist(score_word(Word), VS, Scored),
    sort(1, @=<, Scored, [Dist-(Surf-Intent-Arity)|_]),
    Dist =< 2,                        % tolerance
    extract_args(Arity, Rest, Intent, Args).

score_word(Word, Surf-Int-Ar, Dist-(Surf-Int-Ar)) :-
    edit_distance(Word, Surf, Dist).

% --- Verb selection in stream ---

select_verb_head([W|Rs], W, Rs) :-
    kb_intents:verb_intent(W, _, _), !.
select_verb_head([W|Rs], Verb, Tail) :-
    findall(Surf, kb_intents:verb_intent(Surf,_,_), Surfs),
    nearest(W, Surfs, Best, D),
    D =< 1, !,
    Verb = Best, Tail = Rs.
select_verb_head([_|Rs], Verb, Tail) :- select_verb_head(Rs, Verb, Tail).

nearest(W, [S|Ss], Best, D) :- edit_distance(W,S,D0), nearest_(W,Ss,S,D0,Best,D).
nearest_(_, [], Best, D, Best, D).
nearest_(W, [S|Ss], CurBest, CurD, Best, D) :-
    edit_distance(W,S,D1),
    (D1 < CurD -> nearest_(W,Ss,S,D1,Best,D)
                ; nearest_(W,Ss,CurBest,CurD,Best,D)).

% --- Argument extraction patterns ---

extract_args(0, _, _, []).
extract_args(1, Rest, Intent, [Arg]) :-
    arg1(Intent, Rest, Arg), !.
extract_args(2, Rest, Intent, [A,B]) :-
    arg2(Intent, Rest, A, B), !.
extract_args(rest, Rest, _, Rest).
extract_args(_, Rest, _, Rest).  % fallback

% Heuristics per intent
arg1(play, Rest, Media) :-
    drop_preps([of,for,to,me,some], Rest, R1),
    head_atom(R1, Media).
arg1(call, Rest, Contact) :-
    drop_preps([to], Rest, R),
    head_atom(R, Contact).
arg1(open, Rest, App) :- head_atom(Rest, App).
arg1(resume, _, _) :- fail.
arg1(_, Rest, X) :- head_atom(Rest, X).

arg2(text, [Contact|MsgParts], Contact, Message) :-
    atomic_list_concat(MsgParts, ' ', Message), !.
arg2(_, Rest, A, B) :-
    Rest = [A|Tail],
    head_atom(Tail, B).

% Helper predicates
head_atom([], '').
head_atom([H|_], H).

drop_preps([], Rest, Rest).
drop_preps([Prep|Preps], [H|T], Rest) :-
    (H = Prep -> drop_preps(Preps, T, Rest) ; Rest = [H|T]).
drop_preps(Preps, [], []) :- Preps = [_|_].


% Edit distance calculation (Levenshtein distance)
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
    levenshtein(T1, T2, Len1_1, Len2_1, D1),     % substitute
    levenshtein(T1, [H2|T2], Len1_1, Len2, D2),  % delete
    levenshtein([H1|T1], T2, Len1, Len2_1, D3),  % insert
    Distance is min(D1, min(D2, D3)) + 1.

