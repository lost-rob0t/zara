:- module(zara_hooks_registry_tests, []).

:- use_module(library(plunit)).
:- use_module('../modules/zara_hooks').
:- use_module('../modules/hooks_loader').

:- dynamic seen/1.

record(Name, _Event) :-
    assertz(seen(Name)).

explode(_Event) :-
    throw(error(test_hook_failure, _)).

reset_seen :-
    retractall(seen(_)).

:- begin_tests(zara_hooks_registry).

test(priority_order) :-
    zara_hooks:clear_hook_owner(test_priority),
    reset_seen,
    zara_hooks:register_hook(before_reply, test_priority, 20,
                             zara_hooks_registry_tests:record(late), _),
    zara_hooks:register_hook(before_reply, test_priority, 10,
                             zara_hooks_registry_tests:record(early), _),
    zara_hooks:run_hook(before_reply, sample),
    findall(Name, seen(Name), Seen),
    assertion(Seen == [early, late]),
    zara_hooks:clear_hook_owner(test_priority).

test(exception_isolated) :-
    zara_hooks:clear_hook_owner(test_exception),
    reset_seen,
    zara_hooks:register_hook(before_reply, test_exception, 10,
                             zara_hooks_registry_tests:explode, _),
    zara_hooks:register_hook(before_reply, test_exception, 20,
                             zara_hooks_registry_tests:record(after_error), _),
    zara_hooks:run_hook(before_reply, sample),
    assertion(seen(after_error)),
    zara_hooks:clear_hook_owner(test_exception).

test(unregister_only_target) :-
    zara_hooks:clear_hook_owner(test_unregister),
    reset_seen,
    zara_hooks:register_hook(before_reply, test_unregister, 10,
                             zara_hooks_registry_tests:record(removed), Removed),
    zara_hooks:register_hook(before_reply, test_unregister, 20,
                             zara_hooks_registry_tests:record(kept), _),
    assertion(zara_hooks:unregister_hook(Removed)),
    zara_hooks:run_hook(before_reply, sample),
    findall(Name, seen(Name), Seen),
    assertion(Seen == [kept]),
    zara_hooks:clear_hook_owner(test_unregister).

test(clear_owner_preserves_other_owner) :-
    zara_hooks:clear_hook_owner(owner_a),
    zara_hooks:clear_hook_owner(owner_b),
    zara_hooks:register_hook(after_reply, owner_a, 10,
                             zara_hooks_registry_tests:record(a), _),
    zara_hooks:register_hook(after_reply, owner_b, 10,
                             zara_hooks_registry_tests:record(b), _),
    zara_hooks:clear_hook_owner(owner_a),
    zara_hooks:list_hooks(Hooks),
    assertion(\+ member(hook(_, after_reply, owner_a, _, _), Hooks)),
    assertion(member(hook(_, after_reply, owner_b, 10, _), Hooks)),
    zara_hooks:clear_hook_owner(owner_b).

test(invalid_stage_rejected,
     [throws(error(domain_error(zara_hook_stage, not_a_stage), _))]) :-
    zara_hooks:register_hook(not_a_stage, test, 10,
                             zara_hooks_registry_tests:record(nope), _).

test(user_hooks_disabled_by_default) :-
    setup_call_cleanup(
        setup_user_hooks_fixture(Root, _HookPath),
        ( hooks_loader:reset_hook_policy,
          hooks_loader:load_user_hooks,
          zara_hooks:list_hooks(Hooks),
          include(is_user_hook, Hooks, UserHooks),
          assertion(UserHooks == [])
        ),
        cleanup_user_hooks_fixture(Root)
    ).

test(user_hooks_load_only_when_enabled) :-
    setup_call_cleanup(
        setup_user_hooks_fixture(Root, _HookPath),
        ( hooks_loader:set_hook_policy(true, false),
          hooks_loader:load_user_hooks,
          zara_hooks:list_hooks(Hooks),
          include(is_user_hook, Hooks, UserHooks),
          assertion(UserHooks = [_])
        ),
        cleanup_user_hooks_fixture(Root)
    ).

test(disabling_hooks_clears_loaded_user_hooks) :-
    setup_call_cleanup(
        setup_user_hooks_fixture(Root, _HookPath),
        ( hooks_loader:set_hook_policy(true, false),
          hooks_loader:load_user_hooks,
          hooks_loader:set_hook_policy(false, false),
          zara_hooks:list_hooks(Hooks),
          include(is_user_hook, Hooks, UserHooks),
          assertion(UserHooks == [])
        ),
        cleanup_user_hooks_fixture(Root)
    ).

test(override_gate_cannot_enable_hooks,
     [throws(error(permission_error(enable, hook_override, hooks_disabled), _))]) :-
    hooks_loader:set_hook_policy(false, true).

test(user_hooks_path_and_reload_is_idempotent) :-
    setup_call_cleanup(
        setup_user_hooks_fixture(Root, HookPath),
        ( hooks_loader:set_hook_policy(true, false),
          hooks_loader:user_hooks_path(Resolved),
          assertion(Resolved == HookPath),
          hooks_loader:reload_user_hooks,
          hooks_loader:reload_user_hooks,
          zara_hooks:list_hooks(Hooks),
          include(is_user_hook, Hooks, UserHooks),
          assertion(UserHooks = [_])
        ),
        cleanup_user_hooks_fixture(Root)
    ).

test(missing_user_hooks_file_is_not_created) :-
    setup_call_cleanup(
        setup_empty_xdg(Root, HookPath),
        ( hooks_loader:set_hook_policy(true, false),
          assertion(\+ exists_file(HookPath)),
          hooks_loader:load_user_hooks,
          assertion(\+ exists_file(HookPath))
        ),
        cleanup_user_hooks_fixture(Root)
    ).

:- end_tests(zara_hooks_registry).

is_user_hook(hook(_, _, user, _, _)).

setup_empty_xdg(Root, HookPath) :-
    tmp_file(zara_hooks_xdg, RootBase),
    atom_concat(RootBase, '_dir', Root),
    make_directory_path(Root),
    setenv('XDG_CONFIG_HOME', Root),
    hooks_loader:user_hooks_path(HookPath).

setup_user_hooks_fixture(Root, HookPath) :-
    setup_empty_xdg(Root, HookPath),
    file_directory_name(HookPath, ConfigDir),
    make_directory_path(ConfigDir),
    setup_call_cleanup(
        open(HookPath, write, Stream),
        format(Stream,
               ':- zara_hooks:register_hook(before_reply, user, 25, writeln, _).~n',
               []),
        close(Stream)
    ).

cleanup_user_hooks_fixture(Root) :-
    hooks_loader:reset_hook_policy,
    zara_hooks:clear_hook_owner(user),
    unsetenv('XDG_CONFIG_HOME'),
    delete_directory_and_contents(Root).
