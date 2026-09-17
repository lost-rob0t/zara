:- begin_tests(config_api).

:- use_module('../modules/config_api').

:- dynamic hook_probe/1.
:- dynamic command_probe/1.

reset_state :-
    config_api:reset_config_api,
    retractall(hook_probe(_)),
    retractall(command_probe(_)).

record_hook :-
    ( retract(hook_probe(N0)) -> true ; N0 = 0 ),
    N is N0 + 1,
    assertz(hook_probe(N)).

record_command(A, B) :-
    assertz(command_probe(A-B)).

test(defvar_does_not_clobber_setq,
     [setup(reset_state), cleanup(reset_state)]) :-
    defvar(theme, outrun),
    setq(theme, red),
    defvar(theme, blue),
    symbol_value(theme, red).

test(defcustom_keeps_existing_value_and_exposes_metadata,
     [setup(reset_state), cleanup(reset_state)]) :-
    setq(model, local),
    defcustom(model, cloud,
        [doc("Preferred inference target"), type(atom), group(llm)]),
    symbol_value(model, local),
    variable_doc(model, "Preferred inference target"),
    variable_options(model, Options),
    memberchk(type(atom), Options),
    memberchk(group(llm), Options).

test(makunbound_preserves_definition,
     [setup(reset_state), cleanup(reset_state)]) :-
    defcustom(foo, default, [doc("foo")]),
    makunbound(foo),
    \+ boundp(foo),
    variable_doc(foo, "foo").

test(hooks_are_reload_idempotent,
     [setup(reset_state), cleanup(reset_state)]) :-
    add_hook(after_reload, record_hook),
    add_hook(after_reload, record_hook),
    hook_functions(after_reload, Goals),
    length(Goals, 1),
    run_hooks(after_reload),
    hook_probe(1),
    remove_hook(after_reload, record_hook),
    hook_functions(after_reload, []).

test(named_commands_accept_arguments,
     [setup(reset_state), cleanup(reset_state)]) :-
    defcommand('capture-pair', record_command, "Capture two arguments"),
    commandp('capture-pair'),
    command_doc('capture-pair', "Capture two arguments"),
    call_command('capture-pair', [left, right]),
    command_probe(left-right),
    commands(['capture-pair']),
    undefcommand('capture-pair'),
    \+ commandp('capture-pair').

test(feature_registry_is_idempotent,
     [setup(reset_state), cleanup(reset_state)]) :-
    provide(clipboard_config),
    provide(clipboard_config),
    featurep(clipboard_config),
    require_feature(clipboard_config),
    unprovide(clipboard_config),
    \+ featurep(clipboard_config).

test(missing_command_is_typed_error,
     [ setup(reset_state),
       cleanup(reset_state),
       throws(error(existence_error(config_command, missing), _))
     ]) :-
    call_command(missing).

test(missing_symbol_is_typed_error,
     [ setup(reset_state),
       cleanup(reset_state),
       throws(error(existence_error(config_variable, missing), _))
     ]) :-
    symbol_value(missing, _).

:- end_tests(config_api).
