:- module(kb_expert_protocol,
    [ expert_protocol_version/1,
      expert_protocol_operation/2,
      expert_activation_transition/3,
      expert_protocol_limit/3,
      expert_json_bound/2,
      expert_invocation_fields/1
    ]).

expert_protocol_version('ZARA-EXPERT/1').

expert_protocol_operation('expert.list', inspect).
expert_protocol_operation('expert.describe', inspect).
expert_protocol_operation('expert.match', inspect).
expert_protocol_operation('expert.activate', lifecycle).
expert_protocol_operation('expert.status', inspect).
expert_protocol_operation('expert.invoke', invocation).
expert_protocol_operation('expert.explain', inspect).
expert_protocol_operation('expert.cancel', lifecycle).
expert_protocol_operation('expert.deactivate', lifecycle).

expert_activation_transition(inactive, activate, activating).
expert_activation_transition(activating, ready, active).
expert_activation_transition(activating, failure, failed).
expert_activation_transition(active, deactivate, draining).
expert_activation_transition(draining, drained, inactive).
expert_activation_transition(active, backend_lost, unavailable).
expert_activation_transition(unavailable, deactivate, draining).
expert_activation_transition(failed, deactivate, inactive).

expert_protocol_limit(timeout_ms, 1, 3600000).
expert_protocol_limit(max_results, 1, 1024).
expert_protocol_limit(max_output_bytes, 1, 1048576).
expert_protocol_limit(max_model_calls, 0, 1024).

expert_json_bound(depth, 16).
expert_json_bound(nodes, 4096).
expert_json_bound(object_members, 128).
expert_json_bound(array_items, 256).
expert_json_bound(string_chars, 4096).
expert_json_bound(key_chars, 128).
expert_json_bound(wire_bytes, 262144).

expert_invocation_fields([
    protocol, request_id, operation, activation_id, expert_id, expert_operation,
    expected_registry_generation, expected_runtime_generation, input, limits
]).
