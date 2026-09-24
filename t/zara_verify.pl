:- begin_tests(zara_verify).
:- use_module('../verification/zara_verify').

hash(Char, Count, Hash) :- length(Chars, Count), maplist(=(Char), Chars), atom_chars(Hash, Chars).
source(Paths, Source) :-
    hash(a, 40, Head), hash(b, 64, Tree), hash(c, 64, Policy),
    Source = _{workspace:'/repo', head:Head, base:Head, merge_base:Head,
               worktree:Tree, policy:Policy, changed_paths:Paths, clean:true}.
row(Run, Digest, Gate, Row) :-
    hash(d, 64, Artifact),
    Row = _{gate:Gate, state:passed, exit_code:0, run_id:Run,
            source_digest:Digest, artifact_sha256:Artifact, bytes:3}.
request(Request) :-
    source([], Source), zara_verify:plan(Source, Gates),
    hash(e, 32, Run), hash(f, 64, Digest), maplist(row(Run, Digest), Gates, Evidence),
    Request = _{source:Source, run_id:Run, source_digest:Digest, evidence:Evidence}.
blocked(Request) :- zara_verify:evaluate(Request, Decision), Decision.verdict \== verified.

test(no_vacuous_pass) :-
    request(R), blocked(R.put(evidence, [])).
test(valid_observations) :-
    request(R), zara_verify:evaluate(R, D), D.verdict == verified,
    D.merge_authorized == false, D.model_calls == 0, D.provider_calls == 0.
test(python_adds_ratchet_and_adversarial_seeds) :-
    source(['zara/runtime.py'], S), zara_verify:plan(S, Gates),
    forall(member(G, [coverage,deep_seed_1,deep_seed_31337]), memberchk(G,Gates)).
test(android_requires_real_evidence) :-
    source(['android/app/Editor.kt'], S), zara_verify:plan(S,G), memberchk(android_ui,G).
test(ipc_requires_independent_review) :-
    source(['android/prolog-ipc/Service.kt'],S), zara_verify:plan(S,G),
    memberchk(independent_review,G), memberchk(security,G).
test(verifier_cannot_self_approve) :-
    source(['verification/zara_verify.pl'],S), zara_verify:plan(S,G), memberchk(independent_review,G).
test(unknown_files_fail_closed) :-
    source(['mystery/thing'],S), zara_verify:plan(S,G), memberchk(scope_review,G).
test(release_requires_provenance) :-
    source(['version.properties'],S), zara_verify:plan(S,G), memberchk(release_provenance,G).
test(renamed_old_path_still_requires_prolog) :-
    source(['kb/old.pl','docs/old.txt'],S), zara_verify:plan(S,G), memberchk(prolog_contracts,G).
test(empty_diff_still_has_full_repo_gate) :-
    source([],S), zara_verify:plan(S,G), memberchk(repository,G), memberchk(package,G).
test(duplicate_evidence) :-
    request(R), R.evidence=[First|_], append(R.evidence,[First],D), blocked(R.put(evidence,D)).
test(unknown_evidence) :-
    request(R), row(R.run_id,R.source_digest,made_up,Row), append(R.evidence,[Row],D),
    blocked(R.put(evidence,D)).
test(nonterminal_states, [forall(member(State,[pending,skipped,cancelled,error,unknown,blocked,missing]))]) :-
    request(R), R.evidence=[First|Rest], Changed=First.put(state,State),
    blocked(R.put(evidence,[Changed|Rest])).
test(nonzero_exit) :-
    request(R), R.evidence=[First|Rest], Changed=First.put(exit_code,3), blocked(R.put(evidence,[Changed|Rest])).
test(boolean_is_not_exit_zero) :-
    request(R), R.evidence=[First|Rest], Changed=First.put(exit_code,false), blocked(R.put(evidence,[Changed|Rest])).
test(stale_run) :-
    request(R), hash(a,32,Other), R.evidence=[First|Rest], Changed=First.put(run_id,Other),
    blocked(R.put(evidence,[Changed|Rest])).
test(stale_source) :-
    request(R), hash(a,64,Other), R.evidence=[First|Rest], Changed=First.put(source_digest,Other),
    blocked(R.put(evidence,[Changed|Rest])).
test(missing_artifact_digest) :-
    request(R), R.evidence=[First|Rest], Changed=First.put(artifact_sha256,''),
    blocked(R.put(evidence,[Changed|Rest])).
test(junit_skip_cannot_hide_under_passed_gate) :-
    request(R), R.evidence=[First|Rest],
    Changed=First.put(junit, _{state:passed,tests:2,failures:0,skipped:1}),
    blocked(R.put(evidence,[Changed|Rest])).
test(failed_beats_blocked) :-
    request(R), R.evidence=[First|Rest], Changed=First.put(state,failed),
    zara_verify:evaluate(R.put(evidence,[Changed|Rest]),D), D.verdict == failed.
test(unsafe_path, [throws(error(domain_error(source,invalid),_))]) :-
    source(['../escape.py'],S), zara_verify:plan(S,_).
test(caller_true_is_not_evidence) :-
    request(R), blocked(R.put(_{evidence:[],verified:true})).
:- end_tests(zara_verify).
