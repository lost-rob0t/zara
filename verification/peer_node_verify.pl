:- module(peer_node_verify,
          [ verify/0,
            device_capability/1,
            security_capability/1,
            authority_source/1,
            non_authority_source/1,
            node_binding_valid/7
          ]).

/*
 * Formal authority kernel for lost-rob0t/zara#1220.
 *
 * The runtime remains authoritative.  This proof mirrors only the small closed
 * security contract that must never drift accidentally while peer-node work is
 * split across Android, desktop, hosted Zara, discovery, and relay slices.
 */

device_capability(open_app).
device_capability(open_uri).

security_capability('session.basic').
security_capability('runtime.status').
security_capability('turn.submit').
security_capability('turn.cancel').
security_capability('tool.approve').
security_capability('context.read').
security_capability('context.write').
security_capability('memory.read').
security_capability('memory.write').
security_capability('daemon.admin').

/* Authentication authority is deliberately narrower than reachability data. */
authority_source(curve_zap).
authority_source(security_registry).

non_authority_source(route_identity).
non_authority_source(advertised_endpoint).
non_authority_source(discovery_metadata).
non_authority_source(relay_identity).
non_authority_source(payload_node_id).
non_authority_source(payload_curve_key).
non_authority_source(payload_generation).

/*
 * A node descriptor becomes session metadata only when all identity-bearing
 * fields match the already authenticated, active registry record exactly.
 */
node_binding_valid(AdvertisedNode,
                   AuthenticatedNode,
                   AdvertisedKey,
                   AuthenticatedKey,
                   AdvertisedGeneration,
                   AuthenticatedGeneration,
                   EnrollmentActive) :-
    EnrollmentActive == true,
    AdvertisedNode == AuthenticatedNode,
    AdvertisedKey == AuthenticatedKey,
    integer(AdvertisedGeneration),
    integer(AuthenticatedGeneration),
    AdvertisedGeneration > 0,
    AdvertisedGeneration == AuthenticatedGeneration.

unique_facts(Predicate, Arity) :-
    functor(Template, Predicate, Arity),
    findall(Template, call(Template), Facts),
    sort(Facts, Unique),
    same_length(Facts, Unique).

disjoint_capability_namespaces :-
    \+ (device_capability(Capability), security_capability(Capability)).

non_authorities_cannot_authenticate :-
    \+ (non_authority_source(Source), authority_source(Source)).

negative_binding_examples_hold :-
    \+ node_binding_valid(phone_alice, phone_mallory, key_a, key_a, 2, 2, true),
    \+ node_binding_valid(phone_alice, phone_alice, key_a, key_b, 2, 2, true),
    \+ node_binding_valid(phone_alice, phone_alice, key_a, key_a, 1, 2, true),
    \+ node_binding_valid(phone_alice, phone_alice, key_a, key_a, 2, 2, false),
    \+ node_binding_valid(phone_alice, phone_alice, key_a, key_a, 0, 0, true).

verify :-
    unique_facts(device_capability, 1),
    unique_facts(security_capability, 1),
    unique_facts(authority_source, 1),
    unique_facts(non_authority_source, 1),
    disjoint_capability_namespaces,
    non_authorities_cannot_authenticate,
    node_binding_valid(phone_alice, phone_alice, key_a, key_a, 2, 2, true),
    negative_binding_examples_hold,
    findall(Source, authority_source(Source), Sources),
    sort(Sources, [curve_zap, security_registry]),
    format('PEER_NODE_VERIFY_OK authority=curve_zap+security_registry~n', []).
