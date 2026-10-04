:- module(health_reasoning,
    [ latest_observation/4,
      goal_progress/5,
      projection_policy/3
    ]).

:- use_module(library(error)).

latest_observation(Observations, Principal, Metric, Latest) :-
    must_be(list, Observations),
    include(observation_matches(Principal, Metric), Observations, Matching),
    Matching = [First|Rest],
    foldl(later_observation, Rest, First, Latest).

goal_progress(
    goal(Principal, Metric, Target, Unit, _Period, _Updated),
    Observations,
    WindowStart,
    WindowEnd,
    progress(Current, Target, Ratio, Unit)
) :-
    must_be(number, Target),
    Target > 0,
    must_be(number, WindowStart),
    must_be(number, WindowEnd),
    WindowStart =< WindowEnd,
    findall(
        Value,
        matching_value(
            Observations, Principal, Metric, Unit,
            WindowStart, WindowEnd, Value
        ),
        Values
    ),
    sum_list(Values, Current),
    Ratio is Current / Target.

projection_policy(wellness, org, explicit_consent).
projection_policy(biometric, org, deny).
projection_policy(profile, org, deny).
projection_policy(raw_biosignal, org, deny).
projection_policy(wellness, local_ui, allow).
projection_policy(biometric, local_ui, explicit_consent).
projection_policy(profile, local_ui, explicit_consent).
projection_policy(raw_biosignal, local_ui, explicit_consent).

observation_matches(Principal, Metric, Observation) :-
    Observation = observation(_, Principal, Metric, _, _, _, _, _).

later_observation(Candidate, Current, Latest) :-
    Candidate = observation(_, _, _, _, CandidateEnd, _, _, _),
    Current = observation(_, _, _, _, CurrentEnd, _, _, _),
    ( CandidateEnd > CurrentEnd -> Latest = Candidate ; Latest = Current ).

matching_value(
    Observations, Principal, Metric, Unit, WindowStart, WindowEnd, Value
) :-
    member(
        observation(_, Principal, Metric, _, End, _, _, Values),
        Observations
    ),
    End >= WindowStart,
    End < WindowEnd,
    member(value(_, Value, Unit), Values),
    number(Value).
