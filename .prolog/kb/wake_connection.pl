configuration_owner(daemon_endpoint, 'zara/daemon_client.py', resolve_daemon_endpoint).
configuration_owner(daemon_curve, 'zara/daemon_client.py', curve_client_config).
configuration_consumer(wake, 'zara/wake.py', 'zara/daemon_client.py').
endpoint_precedence([environment, config, private_ipc_default]).
credential_invariant(partial_set, reject_before_transport).
regression_test(wake_configuration, 't/test_wake_import.py').
regression_test(wake_environment_curve_handshake, 't/test_wake_principal_isolation.py::test_listener_connects_with_environment_only_credentials').
