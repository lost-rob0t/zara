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
filler(hello).
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
filler(hello).
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

% core contractions
slang_map("it's", "it is").
slang_map("that's", "that is").
slang_map("there's", "there is").
slang_map("what's", "what is").
slang_map("who's", "who is").
slang_map("let's", "let us").
slang_map("we're", "we are").
slang_map("they're", "they are").
slang_map("isn't", "is not").
slang_map("aren't", "are not").
slang_map("doesn't", "does not").
slang_map("didn't", "did not").
slang_map("hasn't", "has not").
slang_map("haven't", "have not").
slang_map("hadn't", "had not").
slang_map("shouldn't", "should not").
slang_map("wouldn't", "would not").
slang_map("couldn't", "could not").

% texting slang (clean expansions)
slang_map("idk", "i do not know").
slang_map("imo", "in my opinion").
slang_map("imho", "in my humble opinion").
slang_map("irl", "in real life").
slang_map("btw", "by the way").
slang_map("ftw", "for the win").
slang_map("fyi", "for your information").
slang_map("asap", "as soon as possible").
slang_map("nvm", "never mind").
slang_map("omw", "on my way").
slang_map("brb", "be right back").
slang_map("bbl", "be back later").
slang_map("bfn", "bye for now").
slang_map("tbh", "to be honest").
slang_map("rn", "right now").
slang_map("thx", "thanks").
slang_map("ty", "thank you").
slang_map("yw", "you are welcome").

% phonetic/internet
slang_map("cuz", "because").
slang_map("bc", "because").
slang_map("tho", "though").
slang_map("tho.", "though").
slang_map("w/", "with").
slang_map("w/o", "without").
slang_map("msg", "message").
slang_map("dm", "message").
slang_map("dms", "messages").
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
