"""Machine-readable threat model for Zara's authenticated remote daemon boundary.

Every applicable threat maps to at least one deterministic pytest oracle.  The
companion meta-test enforces complete STRIDE coverage across every modeled
trust boundary and verifies that referenced tests still exist.
"""

from __future__ import annotations

STRIDE = (
    "spoofing",
    "tampering",
    "repudiation",
    "information_disclosure",
    "denial_of_service",
    "elevation_of_privilege",
)

BOUNDARIES = (
    "public_curve_tcp",
    "zap_principal_binding",
    "router_route_binding",
    "zara1_protocol",
    "quota_runtime_dispatch",
    "owner_unix_admin",
    "persistent_security_state",
    "crash_restart_recovery",
)

ACTORS = (
    "remote_unauthenticated",
    "remote_authenticated_limited",
    "compromised_or_revoked_client",
    "network_mitm",
    "different_uid_local",
    "concurrent_owner_processes",
    "malformed_persisted_state",
    "resource_exhaustion_attacker",
    "crash_or_restart_fault",
)


def threat(
    threat_id: str,
    boundary: str,
    stride: str,
    actor: str,
    scenario: str,
    *tests: str,
    sources: tuple[str, ...],
    disposition: str = "tested",
    rationale: str | None = None,
) -> dict[str, object]:
    return {
        "id": threat_id,
        "boundary": boundary,
        "stride": stride,
        "actor": actor,
        "scenario": scenario,
        "tests": tests,
        "sources": sources,
        "disposition": disposition,
        "rationale": rationale,
    }


THREATS = (
    # Public TCP / CURVE boundary -------------------------------------------------
    threat(
        "TCP-S-01", "public_curve_tcp", "spoofing", "remote_unauthenticated",
        "Unknown client key or a client pinned to the wrong server key must never reach ZARA/1.",
        "t/test_daemon_security_gateway.py::test_unknown_curve_key_never_reaches_application_handshake",
        "t/test_security_threat_gateway.py::test_wrong_server_pin_never_reaches_application_handshake",
        sources=("ZEROMQ-RFC26", "ZEROMQ-RFC27", "CWE-287"),
    ),
    threat(
        "TCP-T-01", "public_curve_tcp", "tampering", "network_mitm",
        "CURVE key material must be valid Z85 and public/secret pairs must be internally consistent.",
        "t/test_security_threat_transport.py::test_curve_decoder_normalizes_invalid_z85_alphabet_to_value_error",
        "t/test_security_threat_transport.py::test_curve_server_config_rejects_mismatched_public_secret_pair",
        "t/test_security_threat_transport.py::test_curve_client_config_rejects_mismatched_public_secret_pair",
        sources=("ZEROMQ-RFC26", "CWE-345"),
    ),
    threat(
        "TCP-R-01", "public_curve_tcp", "repudiation", "remote_authenticated_limited",
        "Allowed and denied authenticated application actions must leave bounded identity/decision audit metadata.",
        "t/test_daemon_security_gateway.py::test_security_audit_records_closed_metadata_without_request_secrets",
        sources=("OWASP-STRIDE", "CWE-778"),
    ),
    threat(
        "TCP-I-01", "public_curve_tcp", "information_disclosure", "remote_unauthenticated",
        "Daemon secret key material must remain owner-private and must never be printed by initialization.",
        "t/test_security_threat_management.py::test_security_init_prints_public_identity_but_never_server_secret",
        "t/test_daemon_security.py::test_secret_key_file_requires_owner_only_permissions",
        sources=("ZEROMQ-RFC26", "CWE-532"),
    ),
    threat(
        "TCP-D-01", "public_curve_tcp", "denial_of_service", "resource_exhaustion_attacker",
        "Known-vulnerable libzmq CURVE/ZAP versions are rejected and unknown-client storms must not starve an enrolled peer.",
        "t/test_security_threat_transport.py::test_public_curve_runtime_rejects_known_vulnerable_libzmq_versions",
        "t/test_security_threat_gateway.py::test_unknown_curve_client_storm_does_not_starve_enrolled_client",
        sources=("CVE-2019-13132", "CVE-2020-15166", "CWE-770"),
    ),
    threat(
        "TCP-E-01", "public_curve_tcp", "elevation_of_privilege", "remote_unauthenticated",
        "No TCP listener may bypass CURVE/ZAP or substitute a custom unauthenticated gateway.",
        "t/test_daemon_security.py::test_remote_or_wildcard_tcp_requires_secure_auth",
        "t/test_production_secure_listener.py::test_server_rejects_tcp_without_explicit_security_state",
        sources=("ZEROMQ-RFC27", "CWE-285"),
    ),

    # ZAP -> principal binding ---------------------------------------------------
    threat(
        "ZAP-S-01", "zap_principal_binding", "spoofing", "remote_authenticated_limited",
        "Application payload identity claims must never override the ZAP-selected registry principal.",
        "t/test_daemon_security_gateway.py::test_hello_payload_identity_cannot_override_authenticated_principal",
        "t/test_daemon_security_transport.py::test_authenticated_user_id_reads_zap_metadata_not_payload_identity",
        sources=("ZEROMQ-RFC27", "CWE-287"),
    ),
    threat(
        "ZAP-T-01", "zap_principal_binding", "tampering", "remote_unauthenticated",
        "Malformed ZAP identity/key representations must fail closed with a stable exception surface.",
        "t/test_security_threat_transport.py::test_authenticated_user_id_obeys_bounded_ascii_wire_contract",
        "t/test_security_threat_transport.py::test_credentials_provider_invalid_40_byte_z85_alphabet_fails_closed",
        sources=("ZEROMQ-RFC27", "CWE-20"),
    ),
    threat(
        "ZAP-R-01", "zap_principal_binding", "repudiation", "remote_authenticated_limited",
        "Audit records must identify the registry-selected principal and device for application decisions.",
        "t/test_daemon_security_gateway.py::test_security_audit_records_closed_metadata_without_request_secrets",
        sources=("OWASP-STRIDE", "ZEROMQ-RFC27"),
    ),
    threat(
        "ZAP-I-01", "zap_principal_binding", "information_disclosure", "remote_authenticated_limited",
        "Internal ZAP User-Id values are opaque bounded ASCII and are not exposed by owner client listings.",
        "t/test_security_threat_transport.py::test_generated_zap_user_id_is_opaque_ascii_even_for_unicode_device_label",
        "t/test_security_threat_management.py::test_security_list_exposes_no_server_secret_or_zap_internal_user_id",
        sources=("ZEROMQ-RFC27", "CWE-200"),
    ),
    threat(
        "ZAP-D-01", "zap_principal_binding", "denial_of_service", "remote_unauthenticated",
        "Missing, empty, malformed, non-ASCII or overlong ZAP User-Id metadata must be rejected without dispatch.",
        "t/test_daemon_security_transport.py::test_authenticated_user_id_fails_closed_when_metadata_is_missing_or_empty",
        "t/test_security_threat_transport.py::test_authenticated_user_id_obeys_bounded_ascii_wire_contract",
        sources=("ZEROMQ-RFC27", "CWE-400"),
    ),
    threat(
        "ZAP-E-01", "zap_principal_binding", "elevation_of_privilege", "remote_authenticated_limited",
        "Authentication alone must not grant runtime capabilities the enrolled key does not possess.",
        "t/test_daemon_security_gateway.py::test_authenticated_gateway_denies_missing_capability_before_supervisor_dispatch",
        sources=("CWE-285", "CWE-862"),
    ),

    # ROUTER route -> authenticated identity binding -----------------------------
    threat(
        "ROUTE-S-01", "router_route_binding", "spoofing", "remote_authenticated_limited",
        "A second authenticated principal reusing the same client-selected ROUTER id cannot inherit the prior session.",
        "t/test_security_threat_gateway.py::test_router_identity_handover_cannot_transfer_authenticated_session",
        sources=("ZEROMQ-ROUTER", "CWE-287"),
    ),
    threat(
        "ROUTE-T-01", "router_route_binding", "tampering", "remote_authenticated_limited",
        "Route/address metadata and hello body fields never become principal authority.",
        "t/test_daemon_security_gateway.py::test_hello_payload_identity_cannot_override_authenticated_principal",
        "t/test_security_threat_gateway.py::test_router_identity_handover_cannot_transfer_authenticated_session",
        sources=("ZEROMQ-RFC27", "CWE-345"),
    ),
    threat(
        "ROUTE-R-01", "router_route_binding", "repudiation", "remote_authenticated_limited",
        "Per-route actions are audited against authenticated principal/device rather than raw routing ids.",
        "t/test_daemon_security_gateway.py::test_security_audit_records_closed_metadata_without_request_secrets",
        sources=("OWASP-STRIDE", "ZEROMQ-RFC27"),
    ),
    threat(
        "ROUTE-I-01", "router_route_binding", "information_disclosure", "remote_authenticated_limited",
        "Principal-scoped idempotency prevents one principal from receiving another principal's cached response.",
        "t/test_security_threat_gateway.py::test_same_request_id_is_isolated_by_authenticated_principal",
        sources=("CWE-639", "CWE-200"),
    ),
    threat(
        "ROUTE-D-01", "router_route_binding", "denial_of_service", "resource_exhaustion_attacker",
        "Connection quotas are charged per authenticated principal so one principal cannot consume every peer's allowance.",
        "t/test_daemon_security_gateway.py::test_connection_quota_is_per_authenticated_principal_not_global",
        sources=("CWE-770", "OWASP-DOS"),
    ),
    threat(
        "ROUTE-E-01", "router_route_binding", "elevation_of_privilege", "remote_authenticated_limited",
        "Cross-principal request-id collisions and route-id handover cannot transfer command ownership or session authority.",
        "t/test_security_threat_gateway.py::test_same_request_id_is_isolated_by_authenticated_principal",
        "t/test_security_threat_gateway.py::test_router_identity_handover_cannot_transfer_authenticated_session",
        sources=("CWE-639", "CWE-441"),
    ),

    # ZARA/1 protocol -------------------------------------------------------------
    threat(
        "PROTO-S-01", "zara1_protocol", "spoofing", "remote_authenticated_limited",
        "Identity-looking fields inside ZARA/1 are data only and cannot impersonate another principal.",
        "t/test_daemon_security_gateway.py::test_hello_payload_identity_cannot_override_authenticated_principal",
        sources=("CWE-287", "ZEROMQ-RFC27"),
    ),
    threat(
        "PROTO-T-01", "zara1_protocol", "tampering", "remote_authenticated_limited",
        "Malformed authenticated frames are rejected before runtime dispatch.",
        "t/test_security_threat_gateway.py::test_malformed_authenticated_messages_consume_rate_budget_without_dispatch",
        sources=("CWE-20", "OWASP-STRIDE"),
    ),
    threat(
        "PROTO-R-01", "zara1_protocol", "repudiation", "remote_authenticated_limited",
        "Conflicting reuse of a request id is rejected rather than silently changing the already-recorded command.",
        "t/test_security_threat_gateway.py::test_conflicting_replay_same_principal_request_id_fails_closed",
        sources=("CWE-294", "OWASP-BUSINESS-LOGIC"),
    ),
    threat(
        "PROTO-I-01", "zara1_protocol", "information_disclosure", "remote_authenticated_limited",
        "Authorization errors are closed/generic and audit records exclude request bodies and secrets.",
        "t/test_daemon_security_gateway.py::test_authenticated_gateway_denies_missing_capability_before_supervisor_dispatch",
        "t/test_daemon_security.py::test_audit_is_bounded_and_has_no_arbitrary_payload_surface",
        sources=("CWE-200", "CWE-532"),
    ),
    threat(
        "PROTO-D-01", "zara1_protocol", "denial_of_service", "resource_exhaustion_attacker",
        "Malformed authenticated traffic consumes the same principal rate budget instead of receiving a free parser-abuse lane.",
        "t/test_security_threat_gateway.py::test_malformed_authenticated_messages_consume_rate_budget_without_dispatch",
        sources=("CWE-770", "OWASP-DOS"),
    ),
    threat(
        "PROTO-E-01", "zara1_protocol", "elevation_of_privilege", "remote_authenticated_limited",
        "Future or unknown message types fail closed when no explicit capability mapping exists.",
        "t/test_daemon_security_policy.py::test_security_gateway_capability_policy_fails_closed_for_unmapped_message_type",
        sources=("CWE-285", "CWE-862"),
    ),

    # Quota + runtime dispatch ----------------------------------------------------
    threat(
        "RUN-S-01", "quota_runtime_dispatch", "spoofing", "remote_authenticated_limited",
        "Runtime dispatch receives the registry principal, never a static fallback or caller-supplied principal.",
        "t/test_daemon_security_gateway.py::test_authenticated_gateway_dispatches_with_registry_principal_not_static_fallback",
        sources=("CWE-287", "CWE-441"),
    ),
    threat(
        "RUN-T-01", "quota_runtime_dispatch", "tampering", "remote_authenticated_limited",
        "Same-principal replay with a changed command is an idempotency conflict and cannot mutate the original operation.",
        "t/test_security_threat_gateway.py::test_conflicting_replay_same_principal_request_id_fails_closed",
        sources=("CWE-294", "OWASP-BUSINESS-LOGIC"),
    ),
    threat(
        "RUN-R-01", "quota_runtime_dispatch", "repudiation", "remote_authenticated_limited",
        "Allowed runtime dispatch records action, principal, device, request id and decision without arbitrary body data.",
        "t/test_daemon_security_gateway.py::test_security_audit_records_closed_metadata_without_request_secrets",
        sources=("OWASP-STRIDE", "CWE-778"),
    ),
    threat(
        "RUN-I-01", "quota_runtime_dispatch", "information_disclosure", "remote_authenticated_limited",
        "Audit exhaustion cannot turn the audit ring into an unbounded transcript/secret store.",
        "t/test_daemon_security.py::test_audit_is_bounded_and_has_no_arbitrary_payload_surface",
        sources=("CWE-532", "CWE-770"),
    ),
    threat(
        "RUN-D-01", "quota_runtime_dispatch", "denial_of_service", "resource_exhaustion_attacker",
        "Connection, rate and concurrent-request quotas are bounded per principal and released when work completes.",
        "t/test_daemon_security_gateway.py::test_request_rate_quota_is_per_principal_and_does_not_starve_healthy_peer",
        "t/test_daemon_security_gateway.py::test_concurrent_runtime_quota_releases_when_runtime_future_finishes",
        "t/test_daemon_security.py::test_quota_is_per_principal_and_release_restores_capacity",
        sources=("CWE-770", "OWASP-DOS"),
    ),
    threat(
        "RUN-E-01", "quota_runtime_dispatch", "elevation_of_privilege", "remote_authenticated_limited",
        "Capability checks run before supervisor dispatch, including privileged runtime-status separation.",
        "t/test_daemon_security_gateway.py::test_authenticated_gateway_denies_missing_capability_before_supervisor_dispatch",
        "t/test_daemon_security_gateway.py::test_runtime_status_capability_separates_normal_and_privileged_clients",
        sources=("CWE-285", "CWE-862"),
    ),

    # Owner-local AF_UNIX administration -----------------------------------------
    threat(
        "ADMIN-S-01", "owner_unix_admin", "spoofing", "different_uid_local",
        "AF_UNIX admin requests from a different uid are rejected using SO_PEERCRED.",
        "t/test_security_threat_transport.py::test_owner_admin_rejects_different_uid_peer",
        sources=("LINUX-unix(7)", "CWE-287"),
    ),
    threat(
        "ADMIN-T-01", "owner_unix_admin", "tampering", "different_uid_local",
        "Socket-path replacement, permission tampering and malformed framing cannot create a second mutable authority.",
        "t/test_security_adversarial.py::test_regular_file_at_admin_endpoint_is_never_unlinked_as_stale_socket",
        "t/test_security_adversarial.py::test_client_refuses_permission_tampered_admin_socket_then_recovers",
        "t/test_security_adversarial.py::test_admin_framing_rejects_invalid_utf8_extra_frames_and_oversize",
        sources=("LINUX-unix(7)", "CWE-367"),
    ),
    threat(
        "ADMIN-R-01", "owner_unix_admin", "repudiation", "concurrent_owner_processes",
        "Cryptographic non-repudiation between same-uid owner processes is not a meaningful security boundary: the uid can edit its own state and logs.",
        sources=("LINUX-unix(7)", "OWASP-STRIDE"),
        disposition="out_of_scope",
        rationale="Zara authenticates the local control plane to the Unix uid, not to individual same-uid processes. Per-process non-repudiation requires a separate stronger operator identity/audit design.",
    ),
    threat(
        "ADMIN-I-01", "owner_unix_admin", "information_disclosure", "different_uid_local",
        "Admin socket and state live under owner-private permissions, and management listings omit server secret/internal user ids.",
        "t/test_security_admin_permissions_placeholder.py::test_security_admin_permission_contract_is_covered",
        sources=("LINUX-unix(7)", "CWE-732"),
    ),
    threat(
        "ADMIN-D-01", "owner_unix_admin", "denial_of_service", "different_uid_local",
        "Truncated/oversized/malformed admin connections cannot poison the bounded owner control loop.",
        "t/test_security_chaos.py::test_partial_admin_request_does_not_poison_next_owner_request",
        "t/test_security_adversarial.py::test_malformed_disconnect_storm_does_not_poison_next_owner_request",
        "t/test_security_adversarial.py::test_admin_framing_rejects_invalid_utf8_extra_frames_and_oversize",
        sources=("CWE-400", "CWE-770"),
    ),
    threat(
        "ADMIN-E-01", "owner_unix_admin", "elevation_of_privilege", "different_uid_local",
        "Only closed enroll/revoke/list request shapes are accepted; unknown fields/actions cannot mutate authority.",
        "t/test_security_fuzz.py::test_security_admin_request_shape_fuzz_cannot_mutate_registry",
        "t/test_security_threat_transport.py::test_owner_admin_rejects_different_uid_peer",
        sources=("CWE-285", "CWE-20"),
    ),

    # Persistent security state --------------------------------------------------
    threat(
        "STATE-S-01", "persistent_security_state", "spoofing", "malformed_persisted_state",
        "Symlinked state roots/files and non-owner-private key files are rejected instead of trusted as daemon identity.",
        "t/test_security_threat_transport.py::test_security_state_directory_symlink_is_rejected",
        "t/test_security_threat_transport.py::test_server_identity_file_symlink_and_broad_permissions_fail_closed",
        sources=("CWE-59", "CWE-732"),
    ),
    threat(
        "STATE-T-01", "persistent_security_state", "tampering", "malformed_persisted_state",
        "Corrupt JSON, structured record mutations, mismatched server keypairs and failed atomic replacement all fail closed.",
        "t/test_security_fuzz.py::test_persistent_security_state_json_fuzz_has_closed_exception_surface",
        "t/test_security_fuzz.py::test_persisted_client_record_mutation_fuzz_fails_closed",
        "t/test_security_threat_management.py::test_persisted_mismatched_server_keypair_fails_closed",
        "t/test_security_adversarial.py::test_failed_atomic_replace_does_not_mutate_live_authority",
        sources=("CWE-345", "CWE-20", "CWE-367"),
    ),
    threat(
        "STATE-R-01", "persistent_security_state", "repudiation", "concurrent_owner_processes",
        "Durable enroll/revoke state must reconstruct the same allow/deny decision after restart.",
        "t/test_production_secure_listener.py::test_security_registry_enrollment_and_revocation_survive_restart",
        sources=("OWASP-STRIDE", "NIST-SP800-154"),
    ),
    threat(
        "STATE-I-01", "persistent_security_state", "information_disclosure", "different_uid_local",
        "Daemon secret and client registry files are owner-private and management output excludes the daemon secret.",
        "t/test_production_secure_listener.py::test_security_state_initializes_stable_server_identity_with_private_permissions",
        "t/test_security_threat_management.py::test_security_init_prints_public_identity_but_never_server_secret",
        sources=("CWE-732", "CWE-200"),
    ),
    threat(
        "STATE-D-01", "persistent_security_state", "denial_of_service", "malformed_persisted_state",
        "State byte size and client count are bounded before expensive record construction.",
        "t/test_security_adversarial.py::test_oversized_registry_is_rejected_before_json_parsing",
        "t/test_security_threat_management.py::test_registry_client_count_limit_rejects_resource_bomb_before_entry_validation",
        sources=("CWE-770", "CWE-400"),
    ),
    threat(
        "STATE-E-01", "persistent_security_state", "elevation_of_privilege", "malformed_persisted_state",
        "Persisted unknown/duplicate capabilities and duplicate active identities cannot synthesize stronger live authority.",
        "t/test_security_fuzz.py::test_persisted_client_record_mutation_fuzz_fails_closed",
        "t/test_daemon_security.py::test_registry_rejects_duplicate_active_key_ownership",
        sources=("CWE-285", "CWE-862"),
    ),

    # Crash/restart recovery ------------------------------------------------------
    threat(
        "CRASH-S-01", "crash_restart_recovery", "spoofing", "crash_or_restart_fault",
        "Concurrent first boot cannot create competing daemon identities; all processes converge on one persisted CURVE identity.",
        "t/test_security_process_chaos.py::test_real_process_first_boot_race_converges_on_one_curve_identity",
        sources=("CWE-362", "LINUX-flock(2)"),
    ),
    threat(
        "CRASH-T-01", "crash_restart_recovery", "tampering", "crash_or_restart_fault",
        "Lost-update interleavings and death before persistence/replace preserve one canonical valid registry.",
        "t/test_security_process_chaos.py::test_real_process_barrier_forces_lost_update_interleaving_but_lock_serializes_it",
        "t/test_security_process_chaos.py::test_process_death_before_persistence_leaves_no_enrollment",
        "t/test_security_process_chaos.py::test_process_death_before_atomic_replace_preserves_old_canonical_registry",
        sources=("CWE-362", "CWE-367", "LINUX-flock(2)"),
    ),
    threat(
        "CRASH-R-01", "crash_restart_recovery", "repudiation", "crash_or_restart_fault",
        "A process death after a durable enroll/revoke commit reconstructs that committed authority on restart.",
        "t/test_security_process_chaos.py::test_process_death_after_enrollment_durable_commit_recovers_new_authority",
        "t/test_security_process_chaos.py::test_process_death_after_revocation_durable_commit_recovers_revoked_authority",
        sources=("NIST-SP800-154", "LINUX-flock(2)"),
    ),
    threat(
        "CRASH-I-01", "crash_restart_recovery", "information_disclosure", "crash_or_restart_fault",
        "Abandoned atomic-write temp files are never parsed or promoted as authoritative state after restart.",
        "t/test_security_threat_management.py::test_orphan_atomic_temp_files_are_never_treated_as_authoritative_state",
        sources=("CWE-459", "CWE-367"),
    ),
    threat(
        "CRASH-D-01", "crash_restart_recovery", "denial_of_service", "crash_or_restart_fault",
        "Process death releases the flock and concurrent process storms complete without permanently wedging security state.",
        "t/test_security_process_chaos.py::test_process_death_after_enrollment_durable_commit_recovers_new_authority",
        "t/test_security_process_chaos.py::test_real_process_enrollment_storm_preserves_every_committed_update",
        sources=("LINUX-flock(2)", "CWE-770"),
    ),
    threat(
        "CRASH-E-01", "crash_restart_recovery", "elevation_of_privilege", "compromised_or_revoked_client",
        "A revocation committed before daemon death remains revoked after restart and cannot resurrect the old key.",
        "t/test_security_process_chaos.py::test_process_death_after_revocation_durable_commit_recovers_revoked_authority",
        "t/test_daemon_security.py::test_registry_revocation_is_immediate_for_both_lookup_paths",
        sources=("CWE-672", "CWE-613"),
    ),

    # Explicit residual/out-of-scope assumptions --------------------------------
    threat(
        "RESIDUAL-01", "owner_unix_admin", "elevation_of_privilege", "different_uid_local",
        "A malicious process already running as the exact daemon owner uid can read/replace owner files and signal/debug peer processes.",
        sources=("LINUX-unix(7)",),
        disposition="out_of_scope",
        rationale="Same-uid compromise is equivalent to compromise of Zara's local owner trust root. Zara defends different-uid peers; same-uid isolation requires an OS sandbox/service-account boundary.",
    ),
    threat(
        "RESIDUAL-02", "persistent_security_state", "tampering", "malformed_persisted_state",
        "Root/kernel compromise can bypass owner/mode/symlink checks and alter process memory or files directly.",
        sources=("NIST-SP800-154",),
        disposition="out_of_scope",
        rationale="Root/kernel integrity is below the target of evaluation; this PR assumes the kernel enforces Unix ownership, file descriptors and SO_PEERCRED correctly.",
    ),
    threat(
        "RESIDUAL-03", "public_curve_tcp", "denial_of_service", "resource_exhaustion_attacker",
        "Upstream volumetric saturation can exhaust the host/network before Zara or libzmq receives traffic.",
        sources=("OWASP-DOS",),
        disposition="out_of_scope",
        rationale="Application quotas cannot mitigate link/provider saturation; this requires network-edge capacity, filtering or upstream DDoS protection.",
    ),
    threat(
        "RESIDUAL-04", "public_curve_tcp", "spoofing", "network_mitm",
        "A cryptanalytic break of Curve25519/NaCl or compromised libzmq/pyzmq supply chain defeats transport assumptions.",
        sources=("ZEROMQ-RFC26",),
        disposition="out_of_scope",
        rationale="The implementation validates configuration and rejects historically vulnerable runtimes but does not reimplement or formally verify the cryptographic library.",
    ),
    threat(
        "RESIDUAL-05", "crash_restart_recovery", "tampering", "crash_or_restart_fault",
        "Power loss or storage hardware/filesystem that lies about successful persistence can violate rename durability beyond current process-crash guarantees.",
        sources=("LINUX-fsync(2)",),
        disposition="out_of_scope",
        rationale="Current acceptance covers process death and atomic file replacement. Full power-loss durability requires a separately designed directory-fsync/journal protocol with explicit uncertain-commit semantics.",
    ),
)
