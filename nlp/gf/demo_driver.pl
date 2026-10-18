% Host-only JSON test/demo harness. The production adapter stays portable.
:- use_module(library(http/json)).
:- use_module(library(error)).
:- use_module('../../modules/gf_frontend').
:- initialization(main, main).

main :-
    catch(run, Error, (print_message(error, Error), halt(1))).

run :-
    json_read_dict(current_input, Inputs),
    must_be(list, Inputs),
    length(Inputs, Count),
    ( Count >= 1, Count =< 16 -> true ; domain_error(gf_turn_count, Count) ),
    session(Inputs, [], Results),
    json_write_dict(current_output, Results, [width(0)]),
    nl.

session([], _, []).
session([Text|Texts], Context0, [Result|Results]) :-
    must_be(string, Text),
    string_length(Text, Length),
    ( Length =< 512 -> true ; domain_error(gf_input_length, Length) ),
    ( Text == "" ->
        % An uncovered utterance is not sent through the legacy permissive
        % resolver. No semantic transition occurred, so retain the input context.
        Context1 = Context0,
        Turn = turn([], unsupported, Context0),
        Tree = 'UnsupportedReply'
    ; atom_string(Canonical, Text),
      gf_canonical_turn(Canonical, passive, Context0, gf_turn(Turn, Tree, _)),
      Turn = turn(_, _, Context1)
    ),
    term_string(Turn, Semantic, [quoted(true)]),
    Result = _{reply_tree:Tree, semantic_turn:Semantic},
    session(Texts, Context1, Results).
