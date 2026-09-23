:- begin_tests(default_workflows).

:- use_module('../kb/workflows').

test(version_is_positive) :-
    workflow_version(Version),
    integer(Version),
    Version > 0.

test(workflow_ids_unique) :-
    findall(Id, workflow(Id, _, _, _, _), Ids),
    sort(Ids, Unique),
    length(Ids, Count),
    length(Unique, Count).

test(all_aliases_resolve) :-
    forall(
        workflow_alias(Alias, Expected),
        resolve_workflow(Alias, Expected)
    ).

test(all_triggers_reference_workflows) :-
    forall(
        workflow_trigger(Id, _, _, _),
        workflow(Id, _, _, _, _)
    ).

test(all_conditions_reference_workflows) :-
    forall(
        workflow_condition(Id, _, _, _),
        workflow(Id, _, _, _, _)
    ).

test(all_actions_reference_workflows) :-
    forall(
        workflow_action(Id, _, _, _, _, _),
        workflow(Id, _, _, _, _)
    ).

test(all_actions_have_known_targets) :-
    forall(
        workflow_action(_, _, Target, _, _, _),
        workflow_target(Target)
    ).

test(action_sequences_positive_and_unique_per_workflow) :-
    forall(
        workflow(Id, _, _, _, _),
        (
            findall(Seq, workflow_action(Id, Seq, _, _, _, _), Seqs),
            Seqs \= [],
            forall(member(Seq, Seqs), (integer(Seq), Seq > 0)),
            sort(Seqs, Unique),
            length(Seqs, Count),
            length(Unique, Count)
        )
    ).

test(plans_are_sorted) :-
    forall(
        workflow(Id, _, _, _, _),
        (
            workflow_plan(Id, Actions),
            Actions \= []
        )
    ).

test(no_ambient_authority_actions) :-
    Forbidden = [shell, exec, eval, raw_intent, process_create],
    forall(
        workflow_action(_, _, _, Namespace, Name, _),
        (
            \+ memberchk(Namespace, Forbidden),
            \+ memberchk(Name, Forbidden)
        )
    ).

test(morning_briefing_is_multi_step) :-
    workflow_plan(morning_briefing, Actions),
    length(Actions, Count),
    Count >= 5.

test(cross_device_handoffs_exist) :-
    workflow_handoff(continue_on_desktop, initiator, linux, full_context),
    workflow_handoff(continue_on_phone, initiator, android, full_context),
    workflow_handoff(continue_on_watch, initiator, wear, compact_context).

test(wear_command_center_has_linux_target) :-
    workflow_action(watch_command_center, _, _, ui, command_center, Args),
    member(arg(target, linux), Args).

test(dynamic_targets_are_not_projected_to_every_executor) :-
    workflow_actions_for_executor(morning_briefing, android, Actions),
    \+ member(action(best, device, status, _), Actions),
    \+ member(action(initiator, conversation, present_brief, _), Actions).

test(shared_targets_still_project_to_each_executor) :-
    workflow_actions_for_executor(focus_start, android, Actions),
    member(action(all, device, focus_mode, [arg(enabled, true)]), Actions).

:- end_tests(default_workflows).
