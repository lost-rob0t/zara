:- begin_tests(clipboard).

:- use_module('../modules/clipboard').

:- multifile clipboard:clipboard_provider_available/1.
:- multifile clipboard:clipboard_provider_read/2.
:- multifile clipboard:clipboard_provider_write/2.

clipboard:clipboard_provider_available(test).
clipboard:clipboard_provider_read(test, Text) :-
    nb_current(zara_test_clipboard, Text).
clipboard:clipboard_provider_write(test, Text) :-
    nb_setval(zara_test_clipboard, Text).

reset_clipboard :-
    nb_setval(zara_test_clipboard, ""),
    retractall(clipboard:selected_clipboard_backend(_)),
    retractall(clipboard:kill_ring_state(_)),
    retractall(clipboard:configured_kill_ring_max(_)),
    set_clipboard_backend(test).

cleanup_clipboard :-
    retractall(clipboard:selected_clipboard_backend(_)),
    retractall(clipboard:kill_ring_state(_)),
    retractall(clipboard:configured_kill_ring_max(_)),
    nb_delete(zara_test_clipboard).

test(provider_round_trip,
     [setup(reset_clipboard), cleanup(cleanup_clipboard)]) :-
    current_clipboard_backend(test),
    clipboard_write("hello"),
    clipboard_read("hello"),
    clipboard_clear,
    clipboard_read("").

test(kill_new_tracks_clipboard_and_history,
     [setup(reset_clipboard), cleanup(cleanup_clipboard)]) :-
    kill_new("first"),
    kill_new("second"),
    clipboard_read("second"),
    kill_ring(["second", "first"]),
    current_kill(0, "second"),
    current_kill(1, "first").

test(kill_append_matches_emacs_direction,
     [setup(reset_clipboard), cleanup(cleanup_clipboard)]) :-
    kill_new("core"),
    kill_append("-tail", false),
    current_kill(0, "core-tail"),
    kill_append("head-", true),
    current_kill(0, "head-core-tail"),
    clipboard_read("head-core-tail").

test(kill_ring_max_trims_existing_state,
     [setup(reset_clipboard), cleanup(cleanup_clipboard)]) :-
    kill_new("one"),
    kill_new("two"),
    kill_new("three"),
    set_kill_ring_max(2),
    kill_ring(["three", "two"]),
    kill_ring_max(2).

test(invalid_kill_index_is_typed_error,
     [ setup(reset_clipboard),
       cleanup(cleanup_clipboard),
       throws(error(domain_error(kill_ring_index, 1), _))
     ]) :-
    kill_new("only"),
    current_kill(1, _).

test(unavailable_backend_is_rejected,
     [ setup(reset_clipboard),
       cleanup(cleanup_clipboard),
       throws(error(permission_error(use, clipboard_backend, definitely_missing), _))
     ]) :-
    set_clipboard_backend(definitely_missing).

:- end_tests(clipboard).
