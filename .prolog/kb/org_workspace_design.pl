% Org knowledge-workspace design authority and executable evidence map.

org_workspace_authority('android/DESIGN.md', frozen_product_design).
org_workspace_authority('android/design/reference-2026-09-07.svg', visual_language).
org_workspace_authority('https://docs.logseq.com/', interaction_reference).

org_design_requirement(lossless_source_authority).
org_design_requirement(rendered_block_workspace).
org_design_requirement(full_page_raw_mode).
org_design_requirement(user_editable_config_pl).
org_design_requirement(effect_authority_fence).
org_design_requirement(compact_mobile_navigation).
org_design_requirement(system_bar_safety).
org_design_requirement(exact_head_visual_evidence).

org_design_evidence(lossless_source_authority,
                    implementation,
                    'android/org-core/src/main/java/ai/zara/org/core/OrgWorkspaceDesign.kt').
org_design_evidence(lossless_source_authority,
                    test,
                    'android/org-core/src/test/java/ai/zara/org/core/OrgWorkspaceDesignTest.kt').
org_design_evidence(rendered_block_workspace,
                    implementation,
                    'android/org-surfaces/src/main/java/ai/zara/org/surfaces/EditorSurface.kt').
org_design_evidence(rendered_block_workspace,
                    test,
                    'android/org-surfaces/src/test/java/ai/zara/org/surfaces/OrgEditorSessionTest.kt').
org_design_evidence(full_page_raw_mode,
                    implementation,
                    'android/org-surfaces/src/main/java/ai/zara/org/surfaces/EditorSurface.kt').
org_design_evidence(full_page_raw_mode,
                    test,
                    'android/org-surfaces/src/test/java/ai/zara/org/surfaces/OrgDesignContractTest.kt').
org_design_evidence(user_editable_config_pl,
                    implementation,
                    'android/org-core/src/main/java/ai/zara/org/core/OrgWorkspaceDesign.kt').
org_design_evidence(user_editable_config_pl,
                    test,
                    'android/org-core/src/test/java/ai/zara/org/core/OrgWorkspaceDesignTest.kt').
org_design_evidence(effect_authority_fence,
                    implementation,
                    'android/org-core/src/main/java/ai/zara/org/core/OrgWorkspaceDesign.kt').
org_design_evidence(effect_authority_fence,
                    test,
                    'android/org-core/src/test/java/ai/zara/org/core/OrgWorkspaceDesignTest.kt').
org_design_evidence(compact_mobile_navigation,
                    implementation,
                    'android/org-surfaces/src/main/java/ai/zara/org/surfaces/OrgWorkspaceScreen.kt').
org_design_evidence(compact_mobile_navigation,
                    test,
                    'android/org-surfaces/src/test/java/ai/zara/org/surfaces/OrgDesignContractTest.kt').
org_design_evidence(system_bar_safety,
                    implementation,
                    'android/org-surfaces/src/main/java/ai/zara/org/surfaces/OrgWorkspaceScreen.kt').
org_design_evidence(system_bar_safety,
                    test,
                    'android/org-surfaces/src/test/java/ai/zara/org/surfaces/OrgDesignContractTest.kt').
org_design_evidence(exact_head_visual_evidence,
                    harness,
                    'android/integration/org_device_acceptance.py').
org_design_evidence(exact_head_visual_evidence,
                    validator,
                    'scripts/validate-org-ui-evidence.py').

org_design_requirement_covered(Requirement) :-
    org_design_requirement(Requirement),
    org_design_evidence(Requirement, implementation, _),
    org_design_evidence(Requirement, test, _).
org_design_requirement_covered(exact_head_visual_evidence) :-
    org_design_evidence(exact_head_visual_evidence, harness, _),
    org_design_evidence(exact_head_visual_evidence, validator, _).

org_design_evidence_map_complete :-
    forall(org_design_requirement(Requirement),
           org_design_requirement_covered(Requirement)).

org_document_mode(page, rendered_blocks_with_one_active_editor).
org_document_mode(raw, full_page_canonical_markup).
org_document_mode(config, typed_user_policy_facts).

org_policy_domain(appearance_theme,
                  [outrun, starintel, midnight, terminal, light, system]).
org_policy_domain(appearance_density, [compact, comfortable]).
org_policy_domain(editor_mode, [blocks, raw]).
org_policy_domain(save_mode, [explicit, on_blur]).
org_policy_domain(sync_mode, [manual, automatic, off]).
org_policy_domain(automation_policy, [approval_required, disabled]).
org_policy_domain(plugin_policy, [approval_required, disabled]).
org_policy_domain(runtime_mode, [local, remote, auto]).

org_effectful_policy(automation_policy).
org_effectful_policy(plugin_policy).

org_policy_value_safe(Key, Value) :-
    org_policy_domain(Key, Values),
    memberchk(Value, Values),
    ( org_effectful_policy(Key) ->
        memberchk(Value, [approval_required, disabled])
    ; true
    ).

org_visual_scenario(page_rendered).
org_visual_scenario(block_editing).
org_visual_scenario(raw_markup).
org_visual_scenario(config_policy).
org_visual_scenario(drawer_open).
org_visual_scenario(narrow_large_font).
org_visual_scenario(alternate_theme).
org_visual_scenario(external_change_refusal).
