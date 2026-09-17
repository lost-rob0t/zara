:- begin_tests(exec_config).

:- use_module('../modules/config_loader').
:- use_module('../modules/exec_config_loader').

write_text(Path, Text) :-
    file_directory_name(Path, Dir),
    make_directory_path(Dir),
    setup_call_cleanup(
        open(Path, write, Stream),
        write(Stream, Text),
        close(Stream)
    ).

remove_if_exists(Path) :-
    ( exists_file(Path) -> delete_file(Path) ; true ).

cleanup_exec_probe :-
    ( current_predicate(user:exec_probe/1)
    -> retractall(user:exec_probe(_))
    ; true
    ).

cleanup_server_probe :-
    ( current_predicate(user:server_exec_probe/1)
    -> retractall(user:server_exec_probe(_))
    ; true
    ).

test(exec_config_is_not_auto_created) :-
    exec_config_loader:user_exec_config_path(Path),
    remove_if_exists(Path),
    exec_config_loader:load_user_exec_config,
    \+ exists_file(Path).

test(unqualified_exec_code_loads_into_user_module) :-
    exec_config_loader:user_exec_config_path(Path),
    write_text(Path, 'exec_plain_rule(ok).\n'),
    exec_config_loader:load_user_exec_config,
    user:exec_plain_rule(ok),
    source_file(user:exec_plain_rule(_), Source),
    same_file(Path, Source).

test(exec_config_executes_arbitrary_prolog,
     [cleanup(cleanup_exec_probe)]) :-
    exec_config_loader:user_exec_config_path(Path),
    write_text(Path, ':- assertz(user:exec_probe(ran)).\n'),
    exec_config_loader:load_user_exec_config,
    user:exec_probe(ran).

test(exec_config_reload_reexecutes_directives,
     [cleanup(cleanup_exec_probe)]) :-
    exec_config_loader:user_exec_config_path(Path),
    write_text(Path, ':- retractall(user:exec_probe(_)).\n:- assertz(user:exec_probe(first)).\n'),
    exec_config_loader:load_user_exec_config,
    user:exec_probe(first),
    write_text(Path, ':- retractall(user:exec_probe(_)).\n:- assertz(user:exec_probe(second)).\n'),
    exec_config_loader:reload_user_exec_config,
    \+ user:exec_probe(first),
    user:exec_probe(second).

test(server_config_never_loads_exec_code,
     [cleanup(cleanup_server_probe)]) :-
    exec_config_loader:user_exec_config_path(ExecPath),
    config_loader:user_config_path(BasePath),
    config_loader:user_local_config_path(LocalPath),
    remove_if_exists(BasePath),
    remove_if_exists(LocalPath),
    write_text(ExecPath, ':- assertz(user:server_exec_probe(leaked)).\n'),
    config_loader:load_server_config,
    \+ current_predicate(user:server_exec_probe/1).

:- end_tests(exec_config).
