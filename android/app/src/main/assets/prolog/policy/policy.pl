:- module(zara_policy, [advise_codes/2, advise_terms/3, user_rule/6,
                       disabled/1, suppress/2, option/2]).
% Portable runtime for the pinned desktop KB. Operator extensions are trusted code.
:- use_module(library(lists)).
:- if(current_prolog_flag(dialect, swi)).
:- use_module(library(time), [call_with_time_limit/2]).
:- else.
:- use_module(library(iso_ext), [call_with_time_limit/2]).
:- endif.
:- multifile user_rule/6, disabled/1, suppress/2, option/2.
:- dynamic user_rule/6, disabled/1, suppress/2, option/2.
:- include('defaults.pl').

option(mode, advice).
option(max_findings, 8).
option(disabled_categories, [style]).

setting(Key, Value) :- findall(V, option(Key,V), Values), last(Values,Value).
rule(I,C,P,M,A,S) :- user_rule(I,C,P,M,A,S).
rule(I,C,P,M,A,S) :- default_rule(I,C,P,M,A,S), \+ user_rule(I,_,_,_,_,_).

advise_codes(Codes, Result) :-
    call_with_time_limit(1, advise_codes_bounded(Codes, Result)).

advise_codes_bounded(Codes, Result) :-
    advise_terms(Codes, [], Rows), setting(mode, Mode),
    (Mode == off -> Result = [0] ; guidance(Rows, Guidance), Result = [1|Guidance]).

advise_terms(Codes, Context, Rows) :-
    checked_codes(Codes,32768), ground(Context), is_list(Context),
    findall(r(I,C,P,M,A,S),rule(I,C,P,M,A,S),Rules),
    length(Rules,Count), Count =< 512, valid_rules(Rules),
    findall(I,member(r(I,_,_,_,_,_),Rules),Ids), sort(Ids,Unique), length(Unique,Count),
    setting(mode,Mode), memberchk(Mode,[off,advice]),
    setting(max_findings,Maximum), integer(Maximum), Maximum >= 1, Maximum =< 32,
    setting(disabled_categories,Disabled), is_list(Disabled),
    (Mode == off -> Rows = []
    ; prose(Codes,Segments),
      findall(Key-finding(I,C,P,A,S),
          (member(r(I,C,P,M,A,S),Rules), \+ disabled(I), \+ memberchk(C,Disabled),
           once((member(Segment,Segments),tokens(Segment,Tokens),matches(M,Tokens,Context),
                 \+ (suppress(I,Except),matches(Except,Tokens,Context)))),
           Negative is -P, Key = Negative-I), Pairs),
      keysort(Pairs,Sorted), values(Sorted,All), take(Maximum,All,Rows)
    ).

valid_rules([]).
valid_rules([r(I,C,P,M,A,S)|Rest]) :-
    atom(I), atom_length(I,N), N > 0, N =< 64, atom(C),
    integer(P), P >= 0, P =< 100,
    text_codes(A,AC), checked_codes(AC,1024), AC \= [],
    is_list(S), S \= [], length(S,SN), SN =< 8, atoms(S),
    ground(M), valid_matcher(M,0), valid_rules(Rest).
atoms([]).
atoms([A|As]) :- atom(A), atoms(As).
valid_matcher(M,D) :- D =< 8,
    (M = phrase(P) -> valid_phrase(P)
    ; M = any(Ms) -> valid_matchers(Ms,D)
    ; M = all(Ms) -> valid_matchers(Ms,D)
    ; M = unless(A,B) -> Next is D+1,valid_matcher(A,Next),valid_matcher(B,Next)
    ; M = count(P,N) -> valid_phrase(P),integer(N),N >= 2,N =< 16
    ; M = flag(K,V) -> atom(K),ground(V)
    ; valid_phrase(M)).
valid_matchers(Ms,D) :- is_list(Ms),Ms \= [],length(Ms,N),N =< 32,
    Next is D+1,valid_matcher_list(Ms,Next).
valid_matcher_list([],_).
valid_matcher_list([M|Ms],D) :- valid_matcher(M,D),valid_matcher_list(Ms,D).
valid_phrase(P) :- text_codes(P,C),checked_codes(C,160),tokens(C,T),T \= [].

matches(phrase(P),Tokens,_) :- !,text_codes(P,C),tokens(C,Needle),positive(Needle,Tokens).
matches(any(Ms),Tokens,C) :- !,member(M,Ms),matches(M,Tokens,C).
matches(all(Ms),Tokens,C) :- !,all_matches(Ms,Tokens,C).
matches(unless(M,E),Tokens,C) :- !,matches(M,Tokens,C),\+ matches(E,Tokens,C).
matches(count(P,N),Tokens,_) :- !,text_codes(P,C),tokens(C,Needle),
    findall(1,positive(Needle,Tokens),Hits),length(Hits,Count),Count >= N.
matches(flag(K,V),_,Context) :- !,memberchk(K-Actual,Context),Actual == V.
matches(P,Tokens,C) :- text_codes(P,_),matches(phrase(P),Tokens,C).
all_matches([],_,_).
all_matches([M|Ms],T,C) :- matches(M,T,C),all_matches(Ms,T,C).
positive(Needle,Tokens) :-
    append(Before,Rest,Tokens),append(Needle,_,Rest),
    reverse(Before,Reversed),take(6,Reversed,Window),
    \+ (member(N,[not,never,cannot,false,avoid,without]),memberchk(N,Window)),
    \+ append(_,[t,don|_],Window), \+ append(_,[t,can|_],Window).

tokens(Codes,Tokens) :- normalize(Codes,Clean),split(Clean,[32],Parts),token_atoms(Parts,Tokens).
normalize([],[]).
normalize([C|Cs],[L|Ls]) :-
    (C >= 65,C =< 90 -> L is C+32
    ; (C >= 97,C =< 122 ; C >= 48,C =< 57 ; C >= 128) -> L=C
    ; L=32),normalize(Cs,Ls).
token_atoms([],[]).
token_atoms([[]|Parts],Tokens) :- !,token_atoms(Parts,Tokens).
token_atoms([Codes|Parts],[Atom|Tokens]) :- atom_codes(Atom,Codes),token_atoms(Parts,Tokens).

prose(Codes,Segments) :- split(Codes,[10],Lines),prose_lines(Lines,none,Kept),
    join_lines(Kept,Joined),visible(Joined,Visible),split(Visible,[46,33,63,10],Segments).
prose_lines([],_,[]).
prose_lines([Line|Lines],Fence,[Kept|Rest]) :-
    trim_start(Line,Trim),
    (fence(Trim,Marker) -> Kept=[46],
        (Fence == none -> Next=Marker ; Fence == Marker -> Next=none ; Next=Fence)
    ; Fence \== none -> Kept=[46],Next=Fence
    ; Trim=[62|_] -> Kept=[46],Next=none
    ; Line=[32,32,32,32|_] -> Kept=[46],Next=none
    ; Kept=Line,Next=none),prose_lines(Lines,Next,Rest).
trim_start([C|Cs],Rest) :- memberchk(C,[9,13,32]),!,trim_start(Cs,Rest).
trim_start(Codes,Codes).
fence([96,96,96|_],backtick) :- !.
fence([126,126,126|_],tilde).
join_lines([],[]).
join_lines([Line|Lines],Codes) :- append(Line,[10|Rest],Codes),join_lines(Lines,Rest).
visible([],[]).
visible([C|Cs],[46|Rest]) :- quote_end(C,End),!,skip_quote(Cs,End,Tail),visible(Tail,Rest).
visible([C|Cs],[C|Rest]) :- visible(Cs,Rest).
quote_end(34,34).
quote_end(96,96).
quote_end(8220,8221).
skip_quote([],_,[]).
skip_quote([End|Rest],End,Rest) :- !.
skip_quote([_|Cs],End,Rest) :- skip_quote(Cs,End,Rest).

split(Codes,Separators,Parts) :- split(Codes,Separators,[],Parts).
split([],_,Reversed,[Part]) :- reverse(Reversed,Part).
split([C|Cs],Seps,Reversed,[Part|Parts]) :- memberchk(C,Seps),!,
    reverse(Reversed,Part),split(Cs,Seps,[],Parts).
split([C|Cs],Seps,Reversed,Parts) :- split(Cs,Seps,[C|Reversed],Parts).
text_codes(Text,Codes) :- atom(Text),!,atom_codes(Text,Codes).
text_codes(Text,Codes) :- is_list(Text),!,list_codes(Text,Codes).
text_codes(Text,Codes) :- string_codes(Text,Codes).
list_codes([],[]).
list_codes([X|Xs],[C|Cs]) :- (integer(X) -> C=X ; atom(X),atom_length(X,1),char_code(X,C)),list_codes(Xs,Cs).
checked_codes(Codes,Max) :- is_list(Codes),length(Codes,N),N =< Max,unicode_codes(Codes).
unicode_codes([]).
unicode_codes([C|Cs]) :- integer(C),C >= 0,C =< 1114111,
    (C < 55296 ; C > 57343),unicode_codes(Cs).
values([],[]).
values([_-V|Pairs],[V|Values]) :- values(Pairs,Values).
take(0,_,[]) :- !.
take(_,[],[]) :- !.
take(N,[H|T],[H|Rest]) :- Next is N-1,take(Next,T,Rest).
guidance(Rows,Codes) :- guidance(Rows,0,Codes).
guidance([],_,[]).
guidance([finding(I,_,_,Advice,_)|Rows],Used,Codes) :-
    atom_codes(I,IC),text_codes(Advice,AC),append(IC,[58,32|AC],Line),
    length(Line,N),Next is Used+N+1,
    (Next > 8192 -> Codes=[]
    ; append(Line,[10|Rest],Codes),guidance(Rows,Next,Rest)).
