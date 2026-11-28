expert_tools_module('zara/agent/tools/expert_tools.py').
expert_tools_protocol('ZARA-EXPERT/1').
expert_tools_registry_owner('zara.experts.ExpertRegistry').

expert_protocol_tool(expert_list, 'expert.list').
expert_protocol_tool(expert_describe, 'expert.describe').
expert_protocol_tool(expert_match, 'expert.match').
expert_protocol_tool(expert_activate, 'expert.activate').
expert_protocol_tool(expert_status, 'expert.status').
expert_protocol_tool(expert_invoke, 'expert.invoke').
expert_protocol_tool(expert_explain, 'expert.explain').
expert_protocol_tool(expert_deactivate, 'expert.deactivate').

expert_pack_tool(expert_read_file).
expert_pack_tool(expert_write_file).
expert_pack_tool(expert_diff_file).
expert_pack_tool(expert_list_dir).

expert_tool_gate(config_section, 'experts').
expert_tool_gate(enabled_key, enabled).
expert_tool_gate(default_enabled, true).

expert_pack_root(project, '<repo-root>/.zara/experts').
expert_pack_root(user_xdg, '$XDG_CONFIG_HOME/zarathushtra/experts').
expert_pack_root(extra_config, '[experts] extra_roots').
expert_pack_default_extra('~/.dotfiles/.zara/experts').

expert_tool_invariant(invoke_requires_prior_activation).
expert_tool_invariant(incompatible_protocol_majors_fail_closed).
expert_tool_invariant(absent_experts_excluded_from_catalog).
expert_tool_invariant(pack_tools_scoped_to_expert_roots_only).
expert_tool_invariant(missing_or_symlinked_roots_skipped_at_startup).
expert_tool_invariant(handlers_report_usage_model_calls).

expert_descriptor_fixture('contracts/zara-expert-v1/descriptors.tsv').
expert_fixture_todo_handler('zara.todo_skills.todo_brief').

expert_tools_test_gate('t/test_expert_tools.py').
