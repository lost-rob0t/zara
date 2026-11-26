desktop_prolog_ide_surface(settings_category, 'Prolog IDE').
desktop_prolog_ide_feature(source_navigation).
desktop_prolog_ide_feature(syntax_highlighting).
desktop_prolog_ide_feature(dirty_and_cursor_state).
desktop_prolog_ide_feature(bounded_find_replace).
desktop_prolog_ide_feature(validation_diagnostics).
desktop_prolog_ide_feature(managed_facts).
desktop_prolog_ide_feature(query_history_and_results).

desktop_prolog_ide_invariant(single_engine_authority).
desktop_prolog_ide_invariant(no_qt_thread_prolog).
desktop_prolog_ide_invariant(admin_gated_remote_query).
desktop_prolog_ide_invariant(late_query_results_are_fenced).

desktop_prolog_query_path(settings_window, zara_client).
desktop_prolog_query_path(zara_client, runtime_host).
desktop_prolog_query_path(runtime_host, langgraph_runtime_backend).
desktop_prolog_query_path(langgraph_runtime_backend, agent_manager_prolog_engine).

desktop_prolog_query_limit(max_goal_bytes, 4096).
desktop_prolog_query_limit(max_solutions, 100).
desktop_prolog_query_limit(ui_requested_solutions, 50).
desktop_prolog_query_limit(query_history, 20).
desktop_prolog_query_limit(rendered_output_chars, 64000).

% Desktop inspector drawer (council calm-console, planning/council-desktop-ade).
desktop_prolog_ide_feature(inspector_drawer).
desktop_ade_query_path(inspector_drawer, zara_client_run_prolog_query).
desktop_ade_query_path(settings_console, zara_client_run_prolog_query).
desktop_ade_projection(desktop_ade, kb_module_summary, 2).
desktop_ade_projection(desktop_ade, fact_sample, 3).
desktop_ade_projection(desktop_ade, intent_catalog, 3).
desktop_ade_projection(desktop_ade, wake_words, 1).
desktop_ade_invariant(drawer_issues_only_allowlisted_desktop_ade_goals).
desktop_ade_invariant(drawer_never_mutates_engine).
% Engine occupancy honesty (C6): drawer queries serialize on the process-wide
% PrologEngine._runtime_lock; a lazy 50-solution query can delay live turns
% until drained. No server-side deadline exists yet; this stall window is the
% documented mitigation until the phase-2 deadline lands.
desktop_ade_occupancy(drawer_query, serialized_runtime_lock, documented_stall_window).
