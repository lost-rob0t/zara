:- module(zara_verify, [plan/2, evaluate/2, main/0]).
:- use_module(library(http/json)).
:- use_module(library(lists)).
:- use_module(library(apply)).

% This is a policy proof over host observations, not a proof of arbitrary code.
% JSON is data: never read_term/consult/call a goal supplied by a candidate.
protocol('ZARA-VERIFY/1').
base_gate(verifier).
base_gate(repository).
base_gate(formal).
base_gate(flake).
base_gate(package).

bounded_atom(Value, Max) :- atom(Value), atom_length(Value, N), N > 0, N =< Max.
hex_atom(Value, Length) :-
    atom(Value), atom_length(Value, Length), atom_codes(Value, Codes),
    maplist(hex_code, Codes).
hex_code(Code) :- between(0'0, 0'9, Code), !.
hex_code(Code) :- between(0'a, 0'f, Code).
commit(Value) :- hex_atom(Value, 40), !.
commit(Value) :- hex_atom(Value, 64).
boolean(true).
boolean(false).

safe_path(Path) :-
    bounded_atom(Path, 4096), \+ sub_atom(Path, 0, 1, _, '/'),
    atomic_list_concat(Parts, '/', Path), \+ memberchk('..', Parts),
    atom_codes(Path, Codes), maplist(printable, Codes).
printable(Code) :- Code >= 32.

valid_source(Source) :-
    is_dict(Source),
    get_dict(workspace, Source, Workspace), bounded_atom(Workspace, 4096),
    get_dict(head, Source, Head), commit(Head),
    get_dict(base, Source, Base), commit(Base),
    get_dict(merge_base, Source, Merge), commit(Merge),
    get_dict(worktree, Source, Tree), hex_atom(Tree, 64),
    get_dict(policy, Source, Policy), hex_atom(Policy, 64),
    get_dict(clean, Source, Clean), boolean(Clean),
    get_dict(changed_paths, Source, Paths), is_list(Paths),
    length(Paths, Count), Count =< 4096, maplist(safe_path, Paths).

prefix(Prefix, Path) :- sub_atom(Path, 0, _, _, Prefix).
suffix(Suffix, Path) :- sub_atom(Path, _, _, 0, Suffix).
changed(Source, Path) :- get_dict(changed_paths, Source, Paths), member(Path, Paths).
python_path(Path) :- suffix('.py', Path).
prolog_path(Path) :- suffix('.pl', Path), !.
prolog_path(Path) :- prefix('contracts/zara-expert', Path).
security_path(Path) :-
    downcase_atom(Path, Lower),
    member(Token, [auth, permission, security, secret, ipc]), sub_atom(Lower, _, _, _, Token).
verifier_path(Path) :-
    member(Prefix, ['verification/', 'contracts/zara-verify', '.opencode/', '.github/']),
    prefix(Prefix, Path).
release_path('version.properties').
release_path(Path) :- prefix('.github/workflows/release', Path).

known_path(Path) :-
    member(Prefix, ['zara/', 't/', 'kb/', 'modules/', 'android/', 'contracts/',
                    'verification/', 'scripts/', 'skills/', 'wiki/', 'docs/',
                    'rage/', '.opencode/', '.github/', 'nix/', 'assets/']),
    prefix(Prefix, Path), !.
known_path(Path) :-
    memberchk(Path, ['AGENTS.md', 'README.org', 'README.md', 'CHANGELOG.md',
                     'version.properties', 'flake.nix', 'flake.lock', 'pyproject.toml',
                     'setup.py', 'setup.cfg', 'requirements.txt', 'main.pl',
                     'default.nix', '.gitignore', 'LICENSE', 'DESIGN.md']).

required_gate(_, Gate) :- base_gate(Gate).
required_gate(Source, Gate) :-
    changed(Source, Path), python_path(Path),
    member(Gate, [coverage, deep_seed_1, deep_seed_31337]).
required_gate(Source, prolog_contracts) :- changed(Source, Path), prolog_path(Path).
required_gate(Source, Gate) :-
    changed(Source, Path), prefix('android/', Path), member(Gate, [android_wear, android_ui]).
required_gate(Source, desktop_ui) :- changed(Source, Path), prefix('zara/desktop/', Path).
required_gate(Source, Gate) :-
    changed(Source, Path), security_path(Path), member(Gate, [security, independent_review]).
required_gate(Source, independent_review) :- changed(Source, Path), verifier_path(Path).
required_gate(Source, release_provenance) :- changed(Source, Path), release_path(Path).
required_gate(Source, scope_review) :- changed(Source, Path), \+ known_path(Path).

plan(Source, Gates) :-
    ( valid_source(Source) -> true ; throw(error(domain_error(source, invalid), _)) ),
    findall(Gate, required_gate(Source, Gate), All), sort(All, Gates).

valid_evidence(Evidence) :-
    is_list(Evidence), length(Evidence, Count), Count =< 64,
    maplist(evidence_row, Evidence).
evidence_row(Row) :- is_dict(Row), get_dict(gate, Row, Gate), bounded_atom(Gate, 64).
row_gate(Row, Gate) :- get_dict(gate, Row, Gate).

junit_passes(Row) :-
    ( get_dict(junit, Row, Junit) ->
        is_dict(Junit),
        get_dict(state, Junit, passed),
        get_dict(tests, Junit, Tests), integer(Tests), Tests > 0,
        get_dict(failures, Junit, Failures), integer(Failures), Failures =:= 0,
        get_dict(skipped, Junit, Skipped), integer(Skipped), Skipped =:= 0
    ; true
    ).

row_passes(Row, Run, SourceDigest) :-
    get_dict(state, Row, passed),
    get_dict(exit_code, Row, Exit), integer(Exit), Exit =:= 0,
    get_dict(run_id, Row, Run), get_dict(source_digest, Row, SourceDigest),
    get_dict(artifact_sha256, Row, Digest), hex_atom(Digest, 64),
    get_dict(bytes, Row, Bytes), integer(Bytes), Bytes >= 0, Bytes =< 2097152,
    junit_passes(Row).

reason(Evidence, Gates, _, _, unexpected_gate) :-
    member(Row, Evidence), row_gate(Row, Gate), \+ memberchk(Gate, Gates).
reason(Evidence, _, _, _, duplicate_gate) :-
    maplist(row_gate, Evidence, Raw), sort(Raw, Unique),
    length(Raw, A), length(Unique, B), A =\= B.
reason(Evidence, Gates, Run, Digest, Reason) :-
    member(Gate, Gates),
    findall(Row, (member(Row, Evidence), row_gate(Row, Gate)), Rows),
    ( Rows = [Only], row_passes(Only, Run, Digest) -> fail
    ; atomic_list_concat([Gate, ':missing_failed_stale_or_untrusted'], Reason)
    ).

failed_evidence(Evidence) :-
    member(Row, Evidence), get_dict(state, Row, failed).

% Caller must be the host collector. This predicate does not authenticate JSON.
evaluate(Request, Decision) :-
    get_dict(source, Request, Source), plan(Source, Gates),
    get_dict(run_id, Request, Run), hex_atom(Run, 32),
    get_dict(source_digest, Request, Digest), hex_atom(Digest, 64),
    get_dict(evidence, Request, Evidence), valid_evidence(Evidence),
    findall(Reason, reason(Evidence, Gates, Run, Digest, Reason), Raw), sort(Raw, Reasons),
    ( Reasons == [] -> Verdict = verified
    ; failed_evidence(Evidence) -> Verdict = failed
    ; Verdict = blocked
    ),
    protocol(Protocol),
    Decision = _{protocol:Protocol, verdict:Verdict, required:Gates, reasons:Reasons,
                 scope:local, merge_authorized:false, model_calls:0, provider_calls:0}.

dispatch(Request, Reply) :-
    get_dict(operation, Request, Operation),
    ( Operation == plan ->
        get_dict(source, Request, Source), plan(Source, Gates), protocol(Protocol),
        Reply = _{protocol:Protocol, required:Gates, scope:local, merge_authorized:false}
    ; Operation == evaluate -> evaluate(Request, Reply)
    ; throw(error(domain_error(operation, Operation), _))
    ).

main :-
    catch((json_read_dict(current_input, Request, [value_string_as(atom)]),
           (dispatch(Request, Reply) -> true ; throw(error(invalid_request, _))),
           json_write_dict(current_output, Reply, [width(0)]), nl, halt(0)),
          _, (protocol(Protocol),
              json_write_dict(current_output,
                _{protocol:Protocol, verdict:blocked, reasons:[invalid_policy_input]}, [width(0)]),
              nl, halt(2))).
