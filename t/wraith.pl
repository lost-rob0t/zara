:- begin_tests(wraith).

:- use_module('../modules/wraith').

test(valid_agent_spec) :-
    wraith:valid_agent_spec(
        agent_spec(
            'researcher-1',
            researcher,
            'supervisor-1',
            'zara-python',
            4,
            ['web-search', 'kb-read'],
            [budget(tokens, 5000), budget('tool-calls', 20)]
        )
    ).

test(agent_cannot_parent_itself, [fail]) :-
    wraith:valid_agent_spec(
        agent_spec(
            'researcher-1',
            researcher,
            'researcher-1',
            'zara-python',
            4,
            [],
            []
        )
    ).

test(negative_budget_fails_closed, [fail]) :-
    wraith:valid_budget(budget(tokens, -1)).

test(terminal_state_cannot_resume, [fail]) :-
    wraith:task_transition(completed, running).

test(running_task_may_pause) :-
    wraith:task_transition(running, paused).

test(spawn_budget_and_child_limits_are_enforced) :-
    wraith:may_spawn(running, 1, 2, 100, 25).

test(spawn_rejects_exhausted_child_limit, [fail]) :-
    wraith:may_spawn(running, 2, 2, 100, 25).

test(spawn_rejects_insufficient_budget, [fail]) :-
    wraith:may_spawn(running, 1, 2, 20, 25).

test(message_requires_known_type_and_payload_ref) :-
    wraith:valid_message(
        wraith_message(
            'message-1',
            'supervisor-1',
            'researcher-1',
            task,
            'payload:task-1',
            'trace:run-42'
        )
    ).

test(message_rejects_authority_bearing_payload_ref, [fail]) :-
    wraith:valid_message(
        wraith_message(
            'message-1',
            'supervisor-1',
            'researcher-1',
            task,
            'shell:rm-everything',
            none
        )
    ).

:- end_tests(wraith).
