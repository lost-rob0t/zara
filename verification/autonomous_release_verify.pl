:- dynamic program_issue/1.
:- dynamic release_candidate_sha/1.
:- dynamic release_target/2.
:- dynamic planned_slice/3.
:- dynamic required_gate/1.
:- dynamic slice_evidence/3.
:- dynamic gate_evidence/3.
:- dynamic apk_source_sha/1.
:- dynamic model_calls/1.
:- dynamic unresolved_review_threads/1.

main :-
    current_prolog_flag(argv, Args),
    (   Args = [Mode, FactsFile | _]
    ->  catch(run(Mode, FactsFile), Error, fail_with(Error))
    ;   format(user_error, 'usage: autonomous_release_verify.pl <plan|release> <facts.pl>~n', []),
        halt(2)
    ).

run(Mode, FactsFile) :-
    consult(FactsFile),
    (   Mode == plan
    ->  verify_plan
    ;   Mode == release
    ->  verify_release
    ;   throw(error(unknown_mode(Mode), run/2))
    ),
    release_candidate_sha(Candidate),
    format('AUTONOMOUS_RELEASE_VERIFY_OK mode=~w candidate=~w~n', [Mode, Candidate]),
    halt(0).

fail_with(Error) :-
    print_message(error, Error),
    halt(1).

verify_plan :-
    must(single_candidate(_), candidate_sha),
    must(program_issue(Issue), program_issue),
    must(integer(Issue), program_issue_type),
    must(Issue > 0, program_issue_range),
    must(release_target(Version, AndroidCode), release_target),
    must(atom(Version), release_target_version_type),
    must(integer(AndroidCode), release_target_code_type),
    must(AndroidCode > 0, release_target_code_range),
    findall(Id, planned_slice(Id, _, _), SliceIds),
    must(SliceIds \== [], planned_slices_missing),
    sort(SliceIds, UniqueSliceIds),
    must(same_length(SliceIds, UniqueSliceIds), duplicate_slice_id),
    forall(
        planned_slice(Id, SliceIssue, Required),
        ( must(valid_id(Id), invalid_slice_id(Id)),
          must(integer(SliceIssue), invalid_slice_issue(Id)),
          must(SliceIssue > 0, invalid_slice_issue(Id)),
          must(memberchk(Required, [true, false]), invalid_slice_required(Id))
        )
    ),
    findall(Gate, required_gate(Gate), Gates),
    must(Gates \== [], required_gates_missing),
    sort(Gates, UniqueGates),
    must(same_length(Gates, UniqueGates), duplicate_required_gate),
    forall(required_gate(Gate), must(valid_id(Gate), invalid_gate_id(Gate))).

verify_release :-
    verify_plan,
    single_candidate(Candidate),
    forall(
        planned_slice(Id, _, true),
        must(slice_evidence(Id, Candidate, passed), missing_or_failed_slice(Id))
    ),
    forall(
        required_gate(Gate),
        must(gate_evidence(Gate, Candidate, passed), missing_or_failed_gate(Gate))
    ),
    must(apk_source_sha(Candidate), apk_source_mismatch),
    must(model_calls(0), release_model_calls_nonzero),
    must(unresolved_review_threads(0), unresolved_review_threads).

single_candidate(Candidate) :-
    findall(Sha, release_candidate_sha(Sha), Shas),
    sort(Shas, Unique),
    Unique = [Candidate],
    valid_sha(Candidate).

valid_sha(Sha) :-
    atom(Sha),
    atom_chars(Sha, Chars),
    length(Chars, 40),
    maplist(hex_char, Chars).

hex_char(Char) :-
    char_type(Char, xdigit),
    \+ char_type(Char, upper).

valid_id(Id) :-
    atom(Id),
    atom_chars(Id, [First | Rest]),
    id_start(First),
    maplist(id_char, Rest).

id_start(Char) :-
    char_type(Char, lower).
id_start(Char) :-
    char_type(Char, digit).

id_char(Char) :-
    id_start(Char).
id_char('_').
id_char('-').

must(Goal, _) :-
    call(Goal),
    !.
must(_, Reason) :-
    throw(error(autonomous_release_verification_failed(Reason), _)).

:- initialization(main, main).
