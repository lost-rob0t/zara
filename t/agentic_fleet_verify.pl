:- begin_tests(agentic_fleet_verify).

:- use_module('../verification/agentic_fleet_verify').

worker_ids(Workers) :-
    findall(Worker, worker(Worker, _), Workers).

gate_states(
    [ ci-passed,
      formal_verification-passed,
      security_authority-passed,
      repository_protection-passed
    ]).

test(canonical_worker_set_is_exactly_fifteen) :-
    worker_ids(Workers),
    verify_worker_set(Workers).

test(all_workers_are_vote_eligible) :-
    worker_ids(Workers),
    findall(Voter, vote_eligible(Voter), Voters),
    verify_voter_set(Workers, Voters).

test(duplicate_worker_is_rejected, [fail]) :-
    worker_ids([First | Rest]),
    append([First | Rest], [First], WithDuplicate),
    verify_worker_set(WithDuplicate).

test(missing_voter_is_rejected, [fail]) :-
    worker_ids(Workers),
    Workers = [_ | MissingOne],
    verify_voter_set(Workers, MissingOne).

test(stats_schema_requires_every_field) :-
    findall(Field, required_stats_field(Field), Fields),
    verify_stats_schema(Fields).

test(stats_schema_missing_field_is_rejected, [fail]) :-
    findall(Field, required_stats_field(Field), Fields),
    select(status_only, Fields, MissingStatusOnly),
    verify_stats_schema(MissingStatusOnly).

test(eight_yes_votes_with_all_hard_gates_pass) :-
    worker_ids(Workers),
    length(YesWorkers, 8),
    append(YesWorkers, _, Workers),
    maplist(as_yes_vote, YesWorkers, Votes),
    gate_states(Gates),
    fleet_decision_valid(Votes, Gates).

test(seven_yes_votes_do_not_reach_quorum, [fail]) :-
    worker_ids(Workers),
    length(YesWorkers, 7),
    append(YesWorkers, _, Workers),
    maplist(as_yes_vote, YesWorkers, Votes),
    gate_states(Gates),
    fleet_decision_valid(Votes, Gates).

test(duplicate_ballot_is_rejected, [fail]) :-
    worker_ids([First | Rest]),
    length(MoreWorkers, 7),
    append(MoreWorkers, _, Rest),
    maplist(as_yes_vote, [First | MoreWorkers], Votes0),
    Votes = [vote(First, yes) | Votes0],
    gate_states(Gates),
    fleet_decision_valid(Votes, Gates).

test(failed_hard_gate_cannot_be_voted_away, [fail]) :-
    worker_ids(Workers),
    length(YesWorkers, 8),
    append(YesWorkers, _, Workers),
    maplist(as_yes_vote, YesWorkers, Votes),
    fleet_decision_valid(
        Votes,
        [ ci-passed,
          formal_verification-failed,
          security_authority-passed,
          repository_protection-passed
        ]
    ).

as_yes_vote(Worker, vote(Worker, yes)).

:- end_tests(agentic_fleet_verify).
