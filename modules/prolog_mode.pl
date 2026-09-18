:- module(prolog_mode, [run_json/3, allow_principal/1, allow_goal/2]).

:- use_module(library(apply)).
:- use_module(library(error)).
:- use_module(library(http/json)).
:- use_module(library(sandbox)).
:- use_module(library(solution_sequences)).
:- use_module(library(time)).

:- multifile allow_principal/1, allow_goal/2.
:- dynamic allow_principal/1, allow_goal/2.

allow_principal("local").

run_json(Text, Principal, JSON) :-
    catch(
        call_with_time_limit(2,
            ( call_with_inference_limit(run(Text, Principal, Payload), 100000, Limit),
              ( Limit == inference_limit_exceeded -> throw(inference_limit_exceeded) ; true ) )),
        Error,
        error_payload(Error, Payload)),
    atom_json_dict(JSON, Payload, [as(string)]).

run(Text, Principal, Payload) :-
    must_be(string, Text), string_length(Text, Length), between(1, 16384, Length),
    must_be(string, Principal), string_length(Principal, PrincipalLength),
    between(1, 256, PrincipalLength),
    ( once(allow_principal(Principal)) -> true ; throw(permission_denied) ),
    read_goal(Text, Goal, Names),
    ( once(allow_goal(Principal, Goal)) -> true ; sandbox:safe_goal(user:Goal) ),
    once(findnsols(64, Names, user:Goal, Solutions)),
    maplist(bindings_dict, Solutions, Bindings),
    length(Bindings, Count),
    ( Count =:= 64 -> LimitReached = true ; LimitReached = false ),
    ( Bindings == [] -> Status = "false" ; Status = "ok" ),
    Payload = _{status:Status, bindings:Bindings, limit_reached:LimitReached}.

read_goal(Text, Goal, Names) :-
    setup_call_cleanup(open_string(Text, Stream),
        ( read_term(Stream, Goal, [variable_names(Names), module(user)]),
          read_term(Stream, Rest, [module(user)]),
          ( Rest == end_of_file, Goal \== end_of_file, callable(Goal)
          -> true ; throw(error(syntax_error(single_callable_goal), _)) ) ),
        close(Stream)).

bindings_dict(Names, Dict) :-
    maplist(binding_pair, Names, Pairs),
    dict_create(Dict, bindings, Pairs).

binding_pair(Name=Value, Name-Text) :-
    term_string(Value, Rendered, [quoted(true), max_depth(20), cycles(true), attributes(ignore)]),
    string_length(Rendered, Length),
    ( Length =< 4096 -> Text = Rendered
    ; sub_string(Rendered, 0, 4093, _, Prefix), string_concat(Prefix, "...", Text) ).

error_payload(Error, _{status:"error", error:Code}) :-
    error_code(Error, Code).

error_code(permission_denied, "permission_denied") :- !.
error_code(error(permission_error(_, _, _), _), "permission_denied") :- !.
error_code(error(syntax_error(_), _), "syntax_error") :- !.
error_code(time_limit_exceeded, "time_limit_exceeded") :- !.
error_code(inference_limit_exceeded, "inference_limit_exceeded") :- !.
error_code(_, "execution_failed").
