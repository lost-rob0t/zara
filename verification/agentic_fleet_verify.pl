:- module(agentic_fleet_verify,
          [ verify/0,
            verify_worker_set/1,
            verify_voter_set/2,
            verify_stats_schema/1,
            fleet_decision_valid/2,
            worker/2,
            vote_eligible/1,
            required_stats_field/1,
            hard_gate/1
          ]).

:- use_module(library(lists)).

/*
 * AGENTIC-15 formal policy verifier.
 *
 * This file intentionally proves a small, hard set of fleet invariants in
 * Prolog. Runtime capability/effect proofs are added as their canonical
 * manifests become machine-readable; this verifier must never substitute
 * prose or a skipped check for a proof.
 */

worker('lost-rob0t/zara#623', runtime_actor_orchestration).
worker('lost-rob0t/zara#888', desktop_agent_ux).
worker('lost-rob0t/zara#889', android_agent_ux).
worker('lost-rob0t/zara#909', automation_device_effects).
worker('lost-rob0t/zara#921', wear_edge_agents).
worker('lost-rob0t/zara#646', review_execution_governor).
worker('lost-rob0t/zara#922', ci_formal_verification_release).
worker('lost-rob0t/zara#917', acceptance_evals_release_truth).
worker('lost-rob0t/zara#919', integration_merge).
worker('lost-rob0t/zara#920', build_runtime_infrastructure).
worker('lost-rob0t/zara-plugins#505', provider_routing_economics).
worker('lost-rob0t/zara-plugins#506', voice_multimodal).
worker('lost-rob0t/zara-plugins#507', prolog_rlm_memory_context).
worker('lost-rob0t/zara-plugins#508', tools_mcp_a2a_registry).
worker('lost-rob0t/zara#645', frontier_agentic_research).

vote_eligible(Worker) :-
    worker(Worker, _).

required_stats_field(schema).
required_stats_field(worker_id).
required_stats_field(run_id).
required_stats_field(target).
required_stats_field(head_before).
required_stats_field(head_after).
required_stats_field(wall_ms).
required_stats_field(material_action).
required_stats_field(commits).
required_stats_field(files_changed).
required_stats_field(tests_run).
required_stats_field(tests_passed).
required_stats_field(tests_failed).
required_stats_field(ci_failures_fixed).
required_stats_field(reviews_submitted).
required_stats_field(merges).
required_stats_field(votes_cast).
required_stats_field(tool_calls).
required_stats_field(input_tokens).
required_stats_field(output_tokens).
required_stats_field(model_cost_usd).
required_stats_field(status_only).
required_stats_field(blocker).

hard_gate(ci).
hard_gate(formal_verification).
hard_gate(security_authority).
hard_gate(repository_protection).

stats_schema(v1,
             [ schema,
               worker_id,
               run_id,
               target,
               head_before,
               head_after,
               wall_ms,
               material_action,
               commits,
               files_changed,
               tests_run,
               tests_passed,
               tests_failed,
               ci_failures_fixed,
               reviews_submitted,
               merges,
               votes_cast,
               tool_calls,
               input_tokens,
               output_tokens,
               model_cost_usd,
               status_only,
               blocker
             ]).

verify_worker_set(Workers) :-
    length(Workers, 15),
    sort(Workers, Unique),
    length(Unique, 15).

verify_voter_set(Workers, Voters) :-
    sort(Workers, WorkerSet),
    sort(Voters, VoterSet),
    WorkerSet == VoterSet.

verify_stats_schema(Fields) :-
    forall(required_stats_field(Field), memberchk(Field, Fields)).

valid_vote(yes).
valid_vote(no).
valid_vote(abstain).

known_ballot(vote(Worker, Vote)) :-
    worker(Worker, _),
    valid_vote(Vote).

unique_ballot_workers(Votes) :-
    findall(Worker, member(vote(Worker, _), Votes), Workers),
    sort(Workers, Unique),
    same_length(Workers, Unique).

vote_count(Vote, Votes, Count) :-
    include(has_vote(Vote), Votes, Matching),
    length(Matching, Count).

has_vote(Vote, vote(_, Vote)).

non_abstain(vote(_, Vote)) :-
    Vote \== abstain.

all_hard_gates_passed(GateStates) :-
    forall(hard_gate(Gate), memberchk(Gate-passed, GateStates)).

fleet_decision_valid(Votes, GateStates) :-
    maplist(known_ballot, Votes),
    unique_ballot_workers(Votes),
    include(non_abstain, Votes, NonAbstain),
    length(NonAbstain, Quorum),
    Quorum >= 8,
    vote_count(yes, Votes, Yes),
    vote_count(no, Votes, No),
    Yes >= 8,
    Yes > No,
    all_hard_gates_passed(GateStates).

verify :-
    findall(Worker, worker(Worker, _), Workers),
    (   verify_worker_set(Workers)
    ->  true
    ;   throw(error(agentic15_worker_set_invalid, verify/0))
    ),
    findall(Voter, vote_eligible(Voter), Voters),
    (   verify_voter_set(Workers, Voters)
    ->  true
    ;   throw(error(agentic15_voter_set_invalid, verify/0))
    ),
    stats_schema(v1, StatsFields),
    (   verify_stats_schema(StatsFields)
    ->  true
    ;   throw(error(agentic15_stats_schema_invalid, verify/0))
    ),
    format('AGENTIC15_VERIFY_OK workers=15 voters=15 stats_schema=v1~n', []).
