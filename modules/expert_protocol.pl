:- module(zara_expert_protocol,
    [ validate_invocation/2,
      limits_within/2,
      activation_matches/4
    ]).

:- use_module('../kb/expert_protocol').
:- use_module(library(lists)).

/** <module> Pure ZARA-EXPERT/1 contract checks

These predicates validate inert JSON and compare host-supplied binding snapshots.
They neither register nor activate nor invoke an expert. A matching binding is
not authorization: the host still owns identity, revocation, domain schemas,
deadlines, resource reservations and the actual effect-admission boundary.

JSON uses the existing SWI json_read/3 representation: json([Key=Value]), lists,
atoms, numbers and @(true)/@(false)/@(null). No payload is consulted or called.
*/

%! validate_invocation(+Request, -Result) is det.
%
% Result is valid or invalid_request. Integer fields follow JSON Schema's
% mathematical integer semantics; booleans and strings are never coerced.
validate_invocation(Request, Result) :-
    ( valid_json(Request), invocation_fields(Request)
    -> Result = valid
    ;  Result = invalid_request
    ).

%! limits_within(+Requested, +Ceilings) is semidet.
%
% Compare finite limits without clamping, reserving or resetting budgets.
limits_within(Requested, Ceilings) :-
    valid_json(Requested), valid_json(Ceilings),
    valid_limits(Requested), valid_limits(Ceilings),
    forall(expert_protocol_limit(Key, _, _),
        (field(Requested, Key, Value), field(Ceilings, Key, Ceiling), Value =< Ceiling)).

%! activation_matches(+Request, +Binding, +Context, -Result) is det.
%
% Binding and Context must come from the host, never from invocation payloads.
% This is snapshot consistency only; the caller must recheck current admission.
activation_matches(Request, Binding, Context, Result) :-
    ( ground(Binding-Context), acyclic_term(Binding-Context),
      validate_invocation(Request, valid), matching_binding(Request, Binding, Context)
    -> Result = matching_binding
    ;  Result = invalid_binding
    ).

invocation_fields(Request) :-
    expert_invocation_fields(Keys), exact_fields(Request, Keys),
    field(Request, protocol, Protocol), expert_protocol_version(Protocol),
    field(Request, operation, 'expert.invoke'),
    field(Request, request_id, RequestId), valid_token(RequestId, 128),
    field(Request, activation_id, ActivationId), valid_token(ActivationId, 128),
    field(Request, expert_id, ExpertId), valid_symbol(ExpertId),
    field(Request, expert_operation, Operation), valid_token(Operation, 64),
    field(Request, expected_registry_generation, Registry), valid_generation(Registry),
    field(Request, expected_runtime_generation, Runtime), valid_generation(Runtime),
    field(Request, input, json(_)),
    field(Request, limits, Limits), valid_limits(Limits).

valid_limits(Limits) :-
    findall(Key, expert_protocol_limit(Key, _, _), Keys),
    exact_fields(Limits, Keys),
    forall(expert_protocol_limit(Key, Minimum, Maximum),
        (field(Limits, Key, Value), integer_between(Value, Minimum, Maximum))).

matching_binding(Request,
        activation(Id, Expert, Principal, Scope, Registry, Runtime, active),
        context(CurrentPrincipal, CurrentScope, CurrentRegistry, CurrentRuntime)) :-
    valid_token(Principal, 128), valid_token(Scope, 128),
    valid_generation(Registry), valid_generation(Runtime),
    valid_generation(CurrentRegistry), valid_generation(CurrentRuntime),
    Principal == CurrentPrincipal, Scope == CurrentScope,
    field(Request, activation_id, RequestId), Id == RequestId,
    field(Request, expert_id, RequestExpert), Expert == RequestExpert,
    field(Request, expected_registry_generation, ExpectedRegistry),
    field(Request, expected_runtime_generation, ExpectedRuntime),
    Registry =:= ExpectedRegistry, Registry =:= CurrentRegistry,
    Runtime =:= ExpectedRuntime, Runtime =:= CurrentRuntime.

field(json(Pairs), Key, Value) :- memberchk(Key=Value, Pairs).

exact_fields(json(Pairs), Expected) :-
    pair_keys(Pairs, Keys), msort(Keys, Sorted), msort(Expected, Sorted).

pair_keys([], []).
pair_keys([Key=_|Rest], [Key|Keys]) :- pair_keys(Rest, Keys).

valid_generation(Value) :- integer_between(Value, 1, 9007199254740991).

integer_between(Value, Minimum, Maximum) :-
    json_number(Value), Value >= Minimum, Value =< Maximum,
    Value =:= truncate(Value).

json_number(Value) :- integer(Value), !.
json_number(Value) :-
    float(Value), float_class(Value, Class), memberchk(Class, [zero, subnormal, normal]).

valid_json(Value) :-
    ground(Value), acyclic_term(Value), expert_json_bound(nodes, Budget),
    json_value(Value, 0, Budget, _).

json_value(Value, Depth, Before, After) :-
    expert_json_bound(depth, MaxDepth), Depth =< MaxDepth,
    Before > 0, Remaining is Before - 1,
    json_body(Value, Depth, Remaining, After).

json_body(json(Pairs), Depth, Before, After) :- !,
    NextDepth is Depth + 1, expert_json_bound(object_members, Count),
    json_pairs(Pairs, NextDepth, Count, [], Before, After).
json_body([], _, Budget, Budget) :- !.
json_body([Head|Tail], Depth, Before, After) :- !,
    NextDepth is Depth + 1, expert_json_bound(array_items, Count),
    json_array([Head|Tail], NextDepth, Count, Before, After).
json_body(@(Value), _, Budget, Budget) :- !,
    memberchk(Value, [true, false, null]).
json_body(Value, _, Budget, Budget) :-
    atom(Value), !, expert_json_bound(string_chars, Limit),
    atom_length(Value, Length), Length =< Limit.
json_body(Value, _, Budget, Budget) :- json_number(Value).

json_pairs([], _, _, _, Budget, Budget).
json_pairs([Key=Value|Rest], Depth, Count, Seen, Before, After) :-
    Count > 0, atom(Key), expert_json_bound(key_chars, Limit),
    atom_length(Key, Length), Length =< Limit, \+ memberchk(Key, Seen),
    json_value(Value, Depth, Before, Remaining),
    NextCount is Count - 1,
    json_pairs(Rest, Depth, NextCount, [Key|Seen], Remaining, After).

json_array([], _, _, Budget, Budget).
json_array([Value|Rest], Depth, Count, Before, After) :-
    Count > 0, json_value(Value, Depth, Before, Remaining),
    NextCount is Count - 1,
    json_array(Rest, Depth, NextCount, Remaining, After).

valid_token(Value, Limit) :-
    atom(Value), atom_length(Value, Length), Length > 0, Length =< Limit,
    atom_codes(Value, Codes), Remaining is Limit - 1,
    phrase(token_codes(Remaining), Codes).

valid_symbol(Value) :-
    atom(Value), atom_length(Value, Length), Length =< 256,
    atom_codes(Value, Codes), phrase(symbol_codes, Codes).

symbol_codes --> lower_segment, [58], lower_segment, symbol_tail(7).
lower_segment --> [Code], {lower_code(Code)}, token_tail(63).
token_codes(Remaining) --> [Code], {alnum_code(Code)}, token_tail(Remaining).

symbol_tail(Remaining) -->
    [47], {Remaining > 0}, token_codes(63),
    {Next is Remaining - 1}, symbol_tail(Next).
symbol_tail(_) --> [].

token_tail(Remaining) -->
    [Code], {Remaining > 0, token_code(Code)}, !,
    {Next is Remaining - 1}, token_tail(Next).
token_tail(_) --> [].

lower_code(Code) :- Code >= 97, Code =< 122.
alnum_code(Code) :- lower_code(Code), !.
alnum_code(Code) :- Code >= 48, Code =< 57.
token_code(Code) :- alnum_code(Code), !.
token_code(Code) :- memberchk(Code, [45, 46, 95]).
