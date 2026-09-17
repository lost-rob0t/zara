:- module(clipboard,
    [ clipboard_read/1,
      clipboard_write/1,
      clipboard_clear/0,
      clipboard_available/0,
      clipboard_backend/1,
      current_clipboard_backend/1,
      set_clipboard_backend/1,
      clear_clipboard_backend/0,
      clipboard_backend_available/1,
      clipboard_provider_available/1,
      clipboard_provider_read/2,
      clipboard_provider_write/2,
      kill_new/1,
      kill_append/2,
      current_kill/2,
      kill_ring/1,
      kill_ring_max/1,
      set_kill_ring_max/1
    ]).

:- use_module(library(error)).
:- use_module(library(process)).
:- use_module(library(readutil)).

:- dynamic selected_clipboard_backend/1.
:- dynamic kill_ring_state/1.
:- dynamic configured_kill_ring_max/1.

:- multifile clipboard_provider_available/1.
:- multifile clipboard_provider_read/2.
:- multifile clipboard_provider_write/2.

clipboard_backend(Backend) :-
    clipboard_provider_available(Backend).

clipboard_available :-
    catch(current_clipboard_backend(_), _, fail).

current_clipboard_backend(Backend) :-
    selected_clipboard_backend(Backend),
    require_backend(Backend),
    !.
current_clipboard_backend(Backend) :-
    getenv('ZARA_CLIPBOARD_BACKEND', Raw),
    Raw \== '',
    text_atom(Raw, Backend),
    require_backend(Backend),
    !.
current_clipboard_backend(Backend) :-
    first_available_backend([wayland, xclip, xsel, macos, powershell], Backend),
    !.
current_clipboard_backend(_) :-
    throw(error(existence_error(clipboard_backend, available), _)).

set_clipboard_backend(Backend) :-
    must_be(atom, Backend),
    require_backend(Backend),
    retractall(selected_clipboard_backend(_)),
    assertz(selected_clipboard_backend(Backend)).

clear_clipboard_backend :-
    retractall(selected_clipboard_backend(_)).

clipboard_backend_available(Backend) :-
    must_be(atom, Backend),
    once(clipboard_provider_available(Backend)).

clipboard_read(Text) :-
    current_clipboard_backend(Backend),
    ( once(clipboard_provider_read(Backend, Raw))
    -> text_string(Raw, Text)
    ; throw(error(existence_error(clipboard_reader, Backend), _))
    ).

clipboard_write(Text0) :-
    text_string(Text0, Text),
    current_clipboard_backend(Backend),
    ( once(clipboard_provider_write(Backend, Text))
    -> true
    ; throw(error(existence_error(clipboard_writer, Backend), _))
    ).

clipboard_clear :-
    clipboard_write("").

require_backend(Backend) :-
    ( clipboard_backend_available(Backend)
    -> true
    ; throw(error(permission_error(use, clipboard_backend, Backend),
                  context(clipboard, 'backend is unavailable')))
    ).

first_available_backend([Backend|_], Backend) :-
    clipboard_backend_available(Backend), !.
first_available_backend([_|Backends], Backend) :-
    first_available_backend(Backends, Backend).

% ---------------------------------------------------------------------------
% Built-in desktop providers. No shell is involved: arguments and clipboard
% contents are passed through pipes, so config text cannot become shell code.
% ---------------------------------------------------------------------------

clipboard_provider_available(wayland) :-
    nonempty_env('WAYLAND_DISPLAY'),
    executable_available('wl-copy'),
    executable_available('wl-paste').
clipboard_provider_read(wayland, Text) :-
    read_process('wl-paste', ['--no-newline'], Text).
clipboard_provider_write(wayland, Text) :-
    write_process('wl-copy', [], Text).

clipboard_provider_available(xclip) :-
    nonempty_env('DISPLAY'),
    executable_available(xclip).
clipboard_provider_read(xclip, Text) :-
    read_process(xclip, ['-selection', clipboard, '-o'], Text).
clipboard_provider_write(xclip, Text) :-
    write_process(xclip, ['-selection', clipboard, '-i'], Text).

clipboard_provider_available(xsel) :-
    nonempty_env('DISPLAY'),
    executable_available(xsel).
clipboard_provider_read(xsel, Text) :-
    read_process(xsel, ['--clipboard', '--output'], Text).
clipboard_provider_write(xsel, Text) :-
    write_process(xsel, ['--clipboard', '--input'], Text).

clipboard_provider_available(macos) :-
    current_prolog_flag(apple, true),
    executable_available(pbpaste),
    executable_available(pbcopy).
clipboard_provider_read(macos, Text) :-
    read_process(pbpaste, [], Text).
clipboard_provider_write(macos, Text) :-
    write_process(pbcopy, [], Text).

clipboard_provider_available(powershell) :-
    current_prolog_flag(windows, true),
    powershell_command(_).
clipboard_provider_read(powershell, Text) :-
    powershell_command(Command),
    read_process(Command,
        ['-NoProfile', '-NonInteractive', '-Command',
         '[Console]::Out.Write((Get-Clipboard -Raw))'],
        Text).
clipboard_provider_write(powershell, Text) :-
    powershell_command(Command),
    write_process(Command,
        ['-NoProfile', '-NonInteractive', '-Command',
         '$z=[Console]::In.ReadToEnd(); Set-Clipboard -Value $z'],
        Text).

powershell_command(pwsh) :-
    executable_available(pwsh), !.
powershell_command('powershell.exe') :-
    executable_available('powershell.exe').

nonempty_env(Name) :-
    getenv(Name, Value),
    Value \== ''.

executable_available(Command) :-
    absolute_file_name(path(Command), _,
        [ access(execute),
          file_errors(fail)
        ]).

read_process(Command, Args, Text) :-
    process_create(path(Command), Args,
        [ stdout(pipe(Out)),
          process(Process)
        ]),
    setup_call_cleanup(
        true,
        read_string(Out, _, Text),
        close(Out)
    ),
    process_wait(Process, Status),
    require_process_success(Command, Status).

write_process(Command, Args, Text) :-
    process_create(path(Command), Args,
        [ stdin(pipe(In)),
          process(Process)
        ]),
    setup_call_cleanup(
        true,
        format(In, '~s', [Text]),
        close(In)
    ),
    process_wait(Process, Status),
    require_process_success(Command, Status).

require_process_success(_, exit(0)) :- !.
require_process_success(Command, Status) :-
    throw(error(clipboard_process_failed(Command, Status), _)).

% ---------------------------------------------------------------------------
% Emacs-like kill ring synchronized with the system clipboard.
% ---------------------------------------------------------------------------

kill_new(Text0) :-
    text_string(Text0, Text),
    clipboard_write(Text),
    current_ring(Ring0),
    trim_ring([Text|Ring0], Ring),
    set_ring(Ring).

%% kill_append(+Text, +BeforeP) is det.
%
%  Append to the newest kill when BeforeP=false; prepend when BeforeP=true,
%  matching the shape of Emacs kill-append.
kill_append(Text0, BeforeP) :-
    text_string(Text0, Text),
    must_be(boolean, BeforeP),
    current_ring(Ring0),
    ( Ring0 = [Current|Rest]
    -> true
    ; Current = "",
      Rest = []
    ),
    ( BeforeP == true
    -> string_concat(Text, Current, Combined)
    ; string_concat(Current, Text, Combined)
    ),
    clipboard_write(Combined),
    trim_ring([Combined|Rest], Ring),
    set_ring(Ring).

%% current_kill(+Index, -Text) is det.
%
%  Index 0 is the newest kill, positive indexes walk backward through history.
current_kill(Index, Text) :-
    must_be(nonneg, Index),
    current_ring(Ring),
    ( nth0(Index, Ring, Text)
    -> true
    ; throw(error(domain_error(kill_ring_index, Index), _))
    ).

kill_ring(Ring) :-
    current_ring(Ring).

kill_ring_max(Max) :-
    ( configured_kill_ring_max(Max)
    -> true
    ; Max = 60
    ).

set_kill_ring_max(Max) :-
    must_be(integer, Max),
    ( Max >= 1, Max =< 10000
    -> true
    ; throw(error(domain_error(kill_ring_max, Max), _))
    ),
    retractall(configured_kill_ring_max(_)),
    assertz(configured_kill_ring_max(Max)),
    current_ring(Ring0),
    trim_ring(Ring0, Ring),
    set_ring(Ring).

current_ring(Ring) :-
    ( kill_ring_state(Ring)
    -> true
    ; Ring = []
    ).

set_ring(Ring) :-
    retractall(kill_ring_state(_)),
    assertz(kill_ring_state(Ring)).

trim_ring(Ring0, Ring) :-
    kill_ring_max(Max),
    take(Max, Ring0, Ring).

take(0, _, []) :- !.
take(_, [], []) :- !.
take(N, [Item|Items], [Item|Taken]) :-
    N > 0,
    N1 is N - 1,
    take(N1, Items, Taken).

text_atom(Value, Value) :-
    atom(Value), !.
text_atom(Value, Atom) :-
    string(Value), !,
    atom_string(Atom, Value).
text_atom(Value, Atom) :-
    term_string(Value, Text),
    atom_string(Atom, Text).

text_string(Value, Value) :-
    string(Value), !.
text_string(Value, Text) :-
    atom(Value), !,
    atom_string(Value, Text).
text_string(Value, Text) :-
    number(Value), !,
    number_string(Value, Text).
text_string(Value, Text) :-
    is_list(Value),
    ( catch(string_codes(Text, Value), _, fail)
    -> true
    ; throw(error(type_error(text, Value), _))
    ), !.
text_string(Value, _) :-
    throw(error(type_error(text, Value), _)).
