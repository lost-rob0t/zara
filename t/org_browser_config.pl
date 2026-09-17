:- begin_tests(org_browser_config).

:- use_module('../kb/config').
:- use_module('../modules/config_loader').
:- use_module('../modules/zara_hooks').

:- dynamic seen/1.

record(Event) :- assertz(seen(Event)).
reset_seen :- retractall(seen(_)).


test(default_browser_settings_are_queryable) :-
    kb_config:org_browser_setting(enabled, true),
    kb_config:org_browser_setting(base_font_pt, Font),
    number(Font), Font > 0,
    kb_config:org_browser_setting(search_limit, Limit),
    integer(Limit), Limit > 0,
    kb_config:org_browser_heading_scale(1, Scale),
    number(Scale), Scale > 1.0,
    kb_config:org_browser_help_source("README.org").


test(valid_user_browser_setting_is_accepted) :-
    config_loader:validate_user_fact(
        org_browser_setting(base_font_pt, 14.0),
        kb_config,
        org_browser_setting(base_font_pt, 14.0)
    ),
    config_loader:validate_user_fact(
        org_browser_root("~/org"),
        kb_config,
        org_browser_root("~/org")
    ),
    config_loader:validate_user_fact(
        org_browser_heading_scale(2, 1.5),
        kb_config,
        org_browser_heading_scale(2, 1.5)
    ),
    config_loader:validate_user_fact(
        org_browser_help_source("wiki/android.org"),
        kb_config,
        org_browser_help_source("wiki/android.org")
    ).


test(invalid_browser_setting_is_rejected, [fail]) :-
    config_loader:validate_user_fact(
        org_browser_setting(search_limit, -5),
        _, _
    ).


test(unknown_browser_setting_is_rejected, [fail]) :-
    config_loader:validate_user_fact(
        org_browser_setting(unknown_setting, 1),
        _, _
    ).


test(browser_lifecycle_hook_stages_are_registered) :-
    zara_hooks:clear_hook_owner(org_browser_test),
    reset_seen,
    zara_hooks:register_hook(
        org_browser_before_index,
        org_browser_test,
        10,
        org_browser_config_tests:record(before_index),
        _
    ),
    zara_hooks:register_hook(
        org_browser_node_selected,
        org_browser_test,
        20,
        org_browser_config_tests:record(node_selected),
        _
    ),
    zara_hooks:register_hook(
        org_browser_after_memory_sync,
        org_browser_test,
        30,
        org_browser_config_tests:record(after_memory_sync),
        _
    ),
    zara_hooks:run_hook(org_browser_before_index, workspace(main)),
    zara_hooks:run_hook(org_browser_node_selected, node("abc")),
    zara_hooks:run_hook(org_browser_after_memory_sync, sync(1, 0, 0, 3)),
    findall(Event, seen(Event), Seen),
    assertion(Seen == [before_index, node_selected, after_memory_sync]),
    zara_hooks:clear_hook_owner(org_browser_test).

:- end_tests(org_browser_config).
