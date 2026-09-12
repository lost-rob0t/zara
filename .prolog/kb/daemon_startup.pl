handshake_timeout_possible_cause(no_listener).
handshake_timeout_possible_cause(wrong_endpoint).
handshake_timeout_possible_cause(curve_rejection).
startup_blocker(security_init, runtime_lease_held).
diagnostic_command(runtime_lease_holder, lslocks).
diagnostic_command(daemon_status, 'systemctl --user status zara-server.service').
diagnostic_command(daemon_logs, 'journalctl --user -u zara-server.service').
recovery_invariant(runtime_lease_held, stop_confirmed_owner_not_unlink_lock).
