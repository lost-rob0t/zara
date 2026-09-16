handshake_timeout_possible_cause(no_listener).
handshake_timeout_possible_cause(wrong_endpoint).
handshake_timeout_possible_cause(curve_rejection).
android_connection_diagnostic(server_hello_timeout, hello_stage).
android_connection_diagnostic(capability_negotiation_timeout, capability_stage).
android_server_trust_change(explicit_user_action).
android_server_trust_change(preserves_client_identity).
android_server_trust_change(invalidates_session_and_conversation).
startup_blocker(security_init, runtime_lease_held).
diagnostic_command(runtime_lease_holder, lslocks).
diagnostic_command(daemon_status, 'systemctl --user status zara-server.service').
diagnostic_command(daemon_logs, 'journalctl --user -u zara-server.service').
remote_listener(default_endpoint, 'tcp://0.0.0.0:6060').
remote_listener(primary_transport, owner_private_ipc).
remote_listener(authentication, curve_zap).
remote_listener(client_authorization, enrolled_public_key).
remote_listener(home_manager_profile, 'unseen@flake').
remote_listener(security_state, '$HOME/.local/state/zarathushtra/security').
recovery_invariant(runtime_lease_held, stop_confirmed_owner_not_unlink_lock).
desktop_start_completion_owner('zara/desktop/controller.py', client_start_completed).
desktop_start_invariant(future_completion, queued_qt_delivery).
desktop_start_invariant(daemon_attachment, does_not_require_runtime_started_event).
desktop_start_regression_test('t/test_desktop_shell.py').
