:- begin_tests(health_reasoning).

:- use_module('../modules/health_reasoning').

test(latest_observation_is_scoped_by_principal_and_metric) :-
    Observations = [
        observation(a, owner, steps, 100, 200, phone, wellness, [value(count, 20, steps)]),
        observation(b, owner, steps, 200, 300, watch, wellness, [value(count, 30, steps)]),
        observation(c, other, steps, 300, 400, phone, wellness, [value(count, 900, steps)])
    ],
    health_reasoning:latest_observation(Observations, owner, steps, Latest),
    assertion(Latest = observation(b, owner, steps, 200, 300, watch, wellness, _)).

test(daily_goal_progress_sums_matching_values_and_allows_over_goal) :-
    Goal = goal(owner, steps, 100, steps, daily, 10),
    Observations = [
        observation(a, owner, steps, 100, 200, phone, wellness, [value(count, 60, steps)]),
        observation(b, owner, steps, 201, 300, watch, wellness, [value(count, 75, steps)]),
        observation(c, owner, sleep, 201, 300, phone, biometric, [value(minutes, 480, minutes)]),
        observation(d, other, steps, 201, 300, phone, wellness, [value(count, 500, steps)])
    ],
    health_reasoning:goal_progress(Goal, Observations, 0, 1000, Progress),
    assertion(Progress == progress(135, 100, 1.35, steps)).

test(raw_biosignal_org_projection_is_denied) :-
    health_reasoning:projection_policy(raw_biosignal, org, deny),
    health_reasoning:projection_policy(biometric, org, deny),
    health_reasoning:projection_policy(wellness, org, explicit_consent).

:- end_tests(health_reasoning).
