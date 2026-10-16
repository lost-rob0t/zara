% Verified against Prolog-RLM eba3f90 and Zara focused tests.
prolog_rlm_protocol('ZARA-RUNTIME/1').
prolog_rlm_endpoint(discover, '/zara-runtime/v1/discover').
prolog_rlm_endpoint(generate, '/zara-runtime/v1/generate').
prolog_rlm_endpoint(cancel, '/zara-runtime/v1/cancel').
prolog_rlm_sidecar_capability(host_tools, absent, eba3f90).
prolog_rlm_sidecar_capability(buffered_rlm_generation, present, eba3f90).
repository_code_task_authority(status_transitions, 'zara/tasks/store.py').
repository_code_task_state(sqlite, '.zara/task-state.sqlite3').
repository_code_task_state(json_projection, '.zara/tasks/<sha256-principal>.json').
