:- module(normalizer,
    [ normalize_string/2        % +Raw:string, -Tokens:list(atom)
    , strip_fillers/2           % +Tokens, -Core
    , detokenize/2              % +Tokens, -String
    , is_filler/1               % +Token
    ]).

:- use_module(library(dcg/basics)).
:- use_module(library(lists)).

% ============================================================
% KNOWLEDGE BASE: Stop words / Fillers
% ============================================================
:- dynamic filler/1.
:- discontiguous normalizer:slang_map/2.
filler(please).
filler(could).
filler(would).
filler(kindly).
filler(just).
filler(yo).
filler(hey).
filler(hi).
% filler(hello).
filler(uh).
filler(um).
filler(like).
filler(actually).
filler(basically).
filler(you).
filler(ya).
filler(u).
filler(me).
filler(for).
filler(to).
filler(the).
filler(a).
filler(an).
filler(that).
filler(pls).
filler(plz).
filler(rn).
filler(now).
filler(kinda).
filler(sorta).

% ============================================================
% KNOWLEDGE BASE: Slang/Contraction mappings
% ============================================================
:- dynamic slang_map/2.

filler(please).
filler(could).
filler(would).
filler(kindly).
filler(just).
filler(yo).
filler(hey).
filler(hi).
% filler(hello).
filler(uh).
filler(um).
filler(like).
filler(actually).
filler(basically).
filler(you).
filler(ya).
filler(u).
filler(me).
filler(for).
filler(to).
filler(the).
filler(a).
filler(an).
filler(that).
filler(pls).
filler(plz).
filler(rn).
filler(now).
filler(kinda).
filler(sorta).

filler(hmm).
filler(hey).
filler(yo).
filler(so).
filler(well).
filler(ok).
filler(okay).
filler(ay).
filler(alright).
filler(anyway).
filler(literally).
filler(seriously).
filler(really).
filler(omg).
filler(uhh).
filler(umm).
filler(uhhh).
filler(probably).
filler(maybe).
filler(perhaps).
filler(guess).
filler(sortof).
filler(kindof).
filler(technically).
filler(fr).
filler(lowkey).
filler(highkey).
filler(tbh).
filler(honestly).
filler(obviously).
filler(lmao).
filler(lmfao).
filler(rofl).
filler(tho).
filler(though).
filler(also).
filler(eh).
filler(ah).
filler(oh).
filler(oof).
filler(bruh).
filler(bro).
filler(dude).
filler(man).
filler(girl).
filler(lady).
filler(sir).
filler(madam).

% ============================================================
% KNOWLEDGE BASE: Slang/Contraction mappings (Expanded)
% ============================================================

:- dynamic slang_map/2.

slang_map("can't", "can not").
slang_map("won't", "will not").
slang_map("don't", "do not").
slang_map("i'm", "i am").
slang_map("you're", "you are").
slang_map("u ", "you ").
slang_map(" u", " you").
slang_map("pls", "please").
slang_map("plz", "please").
slang_map(" gonna ", " going to ").
slang_map(" wanna ", " want to ").
slang_map(" gimme ", " give me ").
slang_map(" rn ", " now ").

% Dictation / voice mode phrases (collapsed to tokens our intent KB already knows)
% NOTE: include both "with spaces" and bare phrase forms so it matches
% at string boundaries too (start/end of input).
slang_map(" voice mode ", " voicemode ").
slang_map("voice mode", "voicemode").
slang_map(" mic mode ", " micmode ").
slang_map("mic mode", "micmode").

% Start
slang_map(" start voice mode ", " dictate ").
slang_map("start voice mode", "dictate").
slang_map(" start voice ", " dictate ").
slang_map("start voice", "dictate").
slang_map(" start dictation ", " dictate ").
slang_map("start dictation", "dictate").
slang_map(" start voice input ", " dictate ").
slang_map("start voice input", "dictate").

slang_map(" enable voice mode ", " dictate ").
slang_map("enable voice mode", "dictate").
slang_map(" enable voice ", " dictate ").
slang_map("enable voice", "dictate").
slang_map(" enable dictation ", " dictate ").
slang_map("enable dictation", "dictate").
slang_map(" turn on voice mode ", " dictate ").
slang_map("turn on voice mode", "dictate").
slang_map(" turn on voice ", " dictate ").
slang_map("turn on voice", "dictate").
slang_map(" turn on dictation ", " dictate ").
slang_map("turn on dictation", "dictate").

% Stop
slang_map(" stop voice mode ", " stopvoice ").
slang_map("stop voice mode", "stopvoice").
slang_map(" stop dictation ", " stopdictation ").
slang_map("stop dictation", "stopdictation").
slang_map(" stop voice ", " stopvoice ").
slang_map("stop voice", "stopvoice").
slang_map(" disable dictation ", " stopdictation ").
slang_map("disable dictation", "stopdictation").
slang_map(" disable voice ", " stopvoice ").
slang_map("disable voice", "stopvoice").
slang_map(" turn off voice mode ", " stopvoice ").
slang_map("turn off voice mode", "stopvoice").
slang_map(" turn off dictation ", " stopdictation ").
slang_map("turn off dictation", "stopdictation").




% ============================================================
% PUBLIC API
% ============================================================

% Public: normalize_string/2
% - lowercase
% - strip punctuation except intra-word apostrophes
% - expand common slang/contractions
% - tokenize into atoms
normalize_string(Raw, Tokens) :-
    string_lower(Raw, Lwr),
    map_all_slang(Lwr, SlangFixed),
    remove_punct(SlangFixed, Clean),
    split_string(Clean, " \t\n", " \t\n", Parts),
    maplist(string_to_atom, Parts, Tokens0),
    exclude(==(''), Tokens0, Tokens).

% Remove soft fillers using KB
strip_fillers(Toks, Core) :-
    exclude(is_filler, Toks, Core).

detokenize(Toks, S) :-
    atomic_list_concat(Toks, ' ', S).

% ============================================================
% HELPERS
% ============================================================

% Check if token is a filler
is_filler(Token) :- filler(Token).

% Apply all slang mappings from KB
map_all_slang(S, Out) :-
    findall([A,B], slang_map(A,B), Repls),
    foldl(repl, Repls, S, Out).

repl([A,B], In, Out) :-
    atom_string(In_atom, In),
    atom_string(A_atom, A),
    atom_string(B_atom, B),
    atomic_list_concat(Parts, A_atom, In_atom),
    atomic_list_concat(Parts, B_atom, Out_atom),
    atom_string(Out_atom, Out).

% Keep letters, digits and spaces; drop other punctuation
remove_punct(In, Out) :-
    string_codes(In, Cs),
    include(keep_code, Cs, Ks),
    string_codes(Out, Ks).

keep_code(C) :-
    ( C >= 0'a, C =< 0'z
    ; C >= 0'0, C =< 0'9
    ; C =:= 0'  % space
    ; C =:= 0'\' % keep apostrophe for contractions already expanded
    ).
