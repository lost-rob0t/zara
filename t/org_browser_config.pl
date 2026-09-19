:- begin_tests(org_browser_config).

:- use_module('../kb/config').
:- use_module('../modules/zara_hooks').

:- dynamic seen/1.

record(Event) :- assertz(seen(Event)).
record(Event, _) :- record(Event).
reset_seen :- retractall(seen(_)).
reset_browser :- kb_config:reset_org_browser_config.


test(default_browser_settings_are_queryable_without_becoming_overrides,
     [setup(reset_browser)]) :-
    kb_config:org_browser_effective_setting(enabled, true),
    kb_config:org_browser_effective_setting(base_font_pt, Font),
    number(Font), Font > 0,
    kb_config:org_browser_effective_setting(search_limit, Limit),
    integer(Limit), Limit > 0,
    kb_config:org_browser_effective_heading_scale(1, Scale),
    number(Scale), Scale > 1.0,
    kb_config:org_browser_default_help_source("README.org"),
    \+ kb_config:org_browser_setting(base_font_pt, _).


test(executable_prolog_api_sets_all_browser_configuration,
     [setup(reset_browser), cleanup(reset_browser)]) :-
    kb_config:set_org_browser_setting(base_font_pt, 14.0),
    kb_config:set_org_browser_setting(show_backlinks, false),
    kb_config:add_org_browser_root("~/org"),
    kb_config:set_org_browser_heading_scale(2, 1.5),
    kb_config:clear_org_browser_help_sources,
    kb_config:add_org_browser_help_source("wiki/android.org"),
    kb_config:org_browser_setting(base_font_pt, 14.0),
    kb_config:org_browser_setting(show_backlinks, false),
    kb_config:org_browser_root("~/org"),
    kb_config:org_browser_heading_scale(2, 1.5),
    kb_config:org_browser_help_source("wiki/android.org").


test(invalid_browser_setting_is_rejected,
     [setup(reset_browser), throws(error(domain_error(org_browser_setting, _), _))]) :-
    kb_config:set_org_browser_setting(search_limit, -5).


test(unknown_browser_setting_is_rejected,
     [setup(reset_browser), throws(error(domain_error(org_browser_setting, _), _))]) :-
    kb_config:set_org_browser_setting(unknown_setting, 1).


test(browser_lifecycle_hook_stages_are_registered) :-
    zara_hooks:clear_hook_owner(org_browser_test),
    reset_seen,
    zara_hooks:register_hook(
        org_browser_before_index,
        org_browser_test,
        10,
        plunit_org_browser_config:record(before_index),
        _
    ),
    zara_hooks:register_hook(
        org_browser_node_selected,
        org_browser_test,
        20,
        plunit_org_browser_config:record(node_selected),
        _
    ),
    zara_hooks:register_hook(
        org_browser_after_memory_sync,
        org_browser_test,
        30,
        plunit_org_browser_config:record(after_memory_sync),
        _
    ),
    zara_hooks:run_hook(org_browser_before_index, workspace(main)),
    zara_hooks:run_hook(org_browser_node_selected, node("abc")),
    zara_hooks:run_hook(org_browser_after_memory_sync, sync(1, 0, 0, 3)),
    findall(Event, seen(Event), Seen),
    assertion(Seen == [before_index, node_selected, after_memory_sync]),
    zara_hooks:clear_hook_owner(org_browser_test).

:- end_tests(org_browser_config).
