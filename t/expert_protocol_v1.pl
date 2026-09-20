:- begin_tests(expert_protocol_v1).

:- use_module('../kb/expert_protocol').
:- use_module('../modules/expert_protocol').
:- use_module(library(http/json)).
:- use_module(library(lists)).

request(json([
    protocol='ZARA-EXPERT/1', request_id='req-42', operation='expert.invoke',
    activation_id='act-7', expert_id='zara:expert/android-troubleshooting',
    expert_operation=diagnose, expected_registry_generation=12,
    expected_runtime_generation=4, input=json([symptom_id=adb_no_devices]),
    limits=json([timeout_ms=3000, max_results=8, max_output_bytes=65536,
                 max_model_calls=0])
])).

replace_field(json(Pairs), Key, Value, json(Updated)) :-
    select(Key=_, Pairs, Rest),
    Updated = [Key=Value|Rest].

binding(activation('act-7', 'zara:expert/android-troubleshooting',
                   operator, workspace, 12, 4, active)).
context(context(operator, workspace, 12, 4)).

test(valid_request) :-
    request(Request),
    validate_invocation(Request, Result),
    assertion(Result == valid).

test(operation_catalog) :-
    findall(Name, expert_protocol_operation(Name, _), Names),
    assertion(Names == ['expert.list', 'expert.describe', 'expert.match',
                        'expert.activate', 'expert.status', 'expert.invoke',
                        'expert.explain', 'expert.cancel', 'expert.deactivate']).

test(lifecycle_is_not_an_implicit_activation) :-
    assertion(expert_activation_transition(inactive, activate, activating)),
    assertion(\+ expert_activation_transition(inactive, invoke, active)),
    assertion(\+ expert_activation_transition(failed, invoke, active)).

test(version_mismatch) :-
    request(Request), replace_field(Request, protocol, 'ZARA-EXPERT/2', Bad),
    validate_invocation(Bad, Result), assertion(Result == invalid_request).

test(extra_field) :-
    request(json(Pairs)),
    validate_invocation(json([principal_id=admin|Pairs]), Result),
    assertion(Result == invalid_request).

test(duplicate_field) :-
    request(json(Pairs)),
    validate_invocation(json([request_id=other|Pairs]), Result),
    assertion(Result == invalid_request).

test(missing_field) :-
    request(json(Pairs)), select(activation_id=_, Pairs, Rest),
    validate_invocation(json(Rest), Result), assertion(Result == invalid_request).

test(non_ground_does_not_bind_input) :-
    request(Request), replace_field(Request, request_id, Unknown, Bad),
    validate_invocation(Bad, Result),
    assertion(Result == invalid_request), assertion(var(Unknown)).

test(cyclic_term) :-
    Cycle = [Cycle], request(Request), replace_field(Request, input, Cycle, Bad),
    validate_invocation(Bad, Result), assertion(Result == invalid_request).

test(callable_is_not_json) :-
    request(Request), replace_field(Request, input, json([payload=halt(0)]), Bad),
    validate_invocation(Bad, Result), assertion(Result == invalid_request).

test(code_text_remains_data) :-
    request(Request),
    replace_field(Request, input, json([text=':- initialization(halt).']), Safe),
    validate_invocation(Safe, Result), assertion(Result == valid).

test(json_scalars) :-
    request(Request),
    replace_field(Request, input,
        json([values=[@(true), @(false), @(null), 0, -2, 1.25, 'Ω', json([]), []]]), Safe),
    validate_invocation(Safe, Result), assertion(Result == valid).

test(boolean_is_not_generation) :-
    request(Request), replace_field(Request, expected_registry_generation, @(true), Bad),
    validate_invocation(Bad, Result), assertion(Result == invalid_request).

test(integral_json_number_generation) :-
    request(Request), replace_field(Request, expected_registry_generation, 12.0, Candidate),
    validate_invocation(Candidate, Result), assertion(Result == valid),
    binding(Binding), context(Context),
    activation_matches(Candidate, Binding, Context, Match),
    assertion(Match == matching_binding).

test(fractional_generation) :-
    request(Request), replace_field(Request, expected_runtime_generation, 1.25, Bad),
    validate_invocation(Bad, Result), assertion(Result == invalid_request).

test(zero_generation) :-
    request(Request), replace_field(Request, expected_runtime_generation, 0, Bad),
    validate_invocation(Bad, Result), assertion(Result == invalid_request).

test(identifier_injection) :-
    request(Request), replace_field(Request, expert_operation, 'user:shell', Bad),
    validate_invocation(Bad, Result), assertion(Result == invalid_request).

test(unknown_domain_operation_is_only_structurally_valid) :-
    request(Request), replace_field(Request, expert_operation, not_registered, Candidate),
    validate_invocation(Candidate, Result), assertion(Result == valid).

test(unexpected_top_level_operation) :-
    request(Request), replace_field(Request, operation, 'expert.activate', Bad),
    validate_invocation(Bad, Result), assertion(Result == invalid_request).

test(invalid_symbol) :-
    forall(member(Id, ['', 'zara://expert/test', 'zara:expert/../test',
                      'zara:expert/test/', 'zara:expert/Test']),
        (request(Request), replace_field(Request, expert_id, Id, Bad),
         validate_invocation(Bad, Result), assertion(Result == invalid_request))).

test(duplicate_input_key) :-
    request(Request), replace_field(Request, input, json([key=1, key=2]), Bad),
    validate_invocation(Bad, Result), assertion(Result == invalid_request).

test(improper_array) :-
    request(Request), replace_field(Request, input, json([values=[one|not_list]]), Bad),
    validate_invocation(Bad, Result), assertion(Result == invalid_request).

test(depth_limit) :-
    nested(18, Value), request(Request), replace_field(Request, input, json([value=Value]), Bad),
    validate_invocation(Bad, Result), assertion(Result == invalid_request).

test(array_limit) :-
    length(Values, 257), maplist(=(0), Values), request(Request),
    replace_field(Request, input, json([values=Values]), Bad),
    validate_invocation(Bad, Result), assertion(Result == invalid_request).

test(limits_are_narrowed_not_reset) :-
    Small = json([timeout_ms=10,max_results=1,max_output_bytes=128,max_model_calls=0]),
    Large = json([timeout_ms=20,max_results=2,max_output_bytes=256,max_model_calls=1]),
    assertion(limits_within(Small, Large)),
    assertion(\+ limits_within(Large, Small)).

test(binding_matches) :-
    request(Request), binding(Binding), context(Context),
    activation_matches(Request, Binding, Context, Result),
    assertion(Result == matching_binding).

test(binding_cross_principal) :-
    request(Request), binding(Binding),
    activation_matches(Request, Binding, context(other,workspace,12,4), Result),
    assertion(Result == invalid_binding).

test(binding_cross_scope) :-
    request(Request), binding(Binding),
    activation_matches(Request, Binding, context(operator,other,12,4), Result),
    assertion(Result == invalid_binding).

test(binding_stale_registry) :-
    request(Request), binding(Binding),
    activation_matches(Request, Binding, context(operator,workspace,13,4), Result),
    assertion(Result == invalid_binding).

test(binding_stale_runtime) :-
    request(Request), binding(Binding),
    activation_matches(Request, Binding, context(operator,workspace,12,5), Result),
    assertion(Result == invalid_binding).

test(binding_inactive) :-
    request(Request), context(Context),
    activation_matches(Request,
        activation('act-7','zara:expert/android-troubleshooting',operator,workspace,12,4,inactive),
        Context, Result),
    assertion(Result == invalid_binding).

test(binding_wrong_handle) :-
    request(Request), binding(Binding), context(Context),
    replace_field(Request, activation_id, 'act-other', Bad),
    activation_matches(Bad, Binding, Context, Result),
    assertion(Result == invalid_binding).

test(binding_wrong_expert) :-
    request(Request), binding(Binding), context(Context),
    replace_field(Request, expert_id, 'zara:expert/other', Bad),
    activation_matches(Bad, Binding, Context, Result),
    assertion(Result == invalid_binding).

test(binding_unknown_context_is_not_unified) :-
    request(Request), binding(Binding),
    activation_matches(Request, Binding, Context, Result),
    assertion(Result == invalid_binding), assertion(var(Context)).

test(shared_json_fixture) :-
    setup_call_cleanup(
        open('contracts/zara-expert-v1/invoke.example.json', read, Stream, [encoding(utf8)]),
        json_read(Stream, Request),
        close(Stream)),
    validate_invocation(Request, Result), assertion(Result == valid).

test(schema_limit_vocabulary_matches_prolog) :-
    setup_call_cleanup(
        open('contracts/zara-expert-v1/expert-invoke.schema.json', read, Stream, [encoding(utf8)]),
        json_read(Stream, json(Schema)),
        close(Stream)),
    memberchk('$defs'=json(Definitions), Schema),
    memberchk(limits=json(LimitSchema), Definitions),
    memberchk(properties=json(Properties), LimitSchema),
    findall(Key, expert_protocol_limit(Key, _, _), FactKeys),
    findall(Key, member(Key=_, Properties), SchemaKeys),
    msort(FactKeys, Sorted), msort(SchemaKeys, Sorted),
    forall(expert_protocol_limit(Key, Minimum, Maximum),
        (memberchk(Key=json(Spec), Properties),
         memberchk(minimum=ActualMinimum, Spec),
         memberchk(maximum=ActualMaximum, Spec),
         assertion(ActualMinimum =:= Minimum),
         assertion(ActualMaximum =:= Maximum))).

nested(0, leaf) :- !.
nested(Count, [Value]) :- Next is Count - 1, nested(Next, Value).

:- end_tests(expert_protocol_v1).
