:- module(capability_plans, [
    plan_for_frame/3,
    plan_candidates/3,
    plan_explanation/3,
    plan_stale/4,
    plan_head_row/13,
    plan_arg_row/7
]).
% Typed ExecutionPlan selection (issue #157, contract
% docs/intentframe-contract.md examples 12/13).
%
% plan_for_frame(+Frame, +Environment, -Plan)
%   Frame       frame/3 with status complete (open frames fail closed;
%               the clarification layer owns them)
%   Environment environment(principal(P), auths(A), devices(D), providers(S),
%                           aliases(AL), policies(POL)) with
%               device(Id, Owner, Caps) and alias(Provider, AliasAtom) rows
%               and prefer(location(L)) / prefer(device(Id)) policies
%   Plan        execution_plan(intent(ns(NS), name(Name)),
%               provider(id(Id), location(L), device(D), side_effect(C),
%                        requires_auth(A)),
%               status(S), arguments(Args), evidence(E),
%               alternatives(Alt))
%               S in ready | unavailable(Reason) | ambiguous | denied(Reason)
%               with Reason in no_provider | provider_unavailable |
%                    alias_unsupported | provider_missing |
%                    insufficient_authorization
%
% Pure: no assert/retract, no dynamic state, no I/O, no kb_config dependency.
% Availability is environment-explicit: the server policy lists reachable
% server providers and the aliases they serve; device advertisements qualify
% only location(device) providers whose declared capability they advertise
% AND whose owner is the requesting principal. Advertisements can never
% create server or admin providers and never grant authorization.
%
% Qualification pipeline per declared provider: availability (server list or
% owned-device advertisement) -> alias gate (ref(app_alias) argument on a
% server provider requires alias(Provider, Alias) in the environment) ->
% authorization (requires_auth(nonnone) requires the principal's auths).
% Surviving candidates are narrowed by policies (preferred location/device
% first; fallback when nothing survives), ordered by descending priority,
% and a top-priority tie yields status ambiguous with sorted alternatives
% ('provider' for server candidates, 'provider@device' for device ones),
% capped at plan_alternative_cap/1. Ties never pick silently.
%
% plan_candidates/3 exposes the qualified candidate set ordered by
% descending priority (ties: provider then device ascending).
% plan_explanation/3 exposes the decision with structured evidence.
% plan_stale/4 re-derives a plan in a fresh environment and marks a
% formerly-ready now-unavailable selection unavailable(provider_missing) -
% no unsafe fallback.
%
% plan_head_row/13 and plan_arg_row/7 are flat projections for marshalling
% (pyswip stringifies nested compounds; 'none' sentinels as in #156).

:- use_module('../kb/capabilities').

plan_alternative_cap(8).

% Bound for text atoms carried into plan arguments (mirrors the resolver's
% 512-char bound; oversized frames fail closed, never truncate).
plan_text_bound(512).

%% ============================================================
%% Entry point
%% ============================================================

plan_for_frame(frame(Intent, Slots, complete), Environment, Plan) :-
    Intent = intent(ns(NS), name(Name)),
    Environment = environment(principal(Principal), auths(Auths),
        devices(Devices), providers(Providers), aliases(Aliases),
        policies(Policies)),
    all_bound_slots(Slots),
    declared_candidates(NS, Name, Principal, Auths, Devices, Providers,
        Aliases, Slots, Qualified, Denied, FailedAliases,
        FailedAvailability),
    decide(Intent, Slots, Policies, Qualified, Denied, FailedAliases,
        FailedAvailability, Plan),
    !.

plan_candidates(frame(intent(ns(NS), name(Name)), Slots, complete),
        Environment, Candidates) :-
    Environment = environment(principal(Principal), auths(Auths),
        devices(Devices), providers(Providers), aliases(Aliases),
        policies(_)),
    all_bound_slots(Slots),
    declared_candidates(NS, Name, Principal, Auths, Devices, Providers,
        Aliases, Slots, Qualified, _Denied, _FailedAliases,
        _FailedAvailability),
    order_candidates(Qualified, Candidates).

plan_explanation(Frame, Environment,
        explanation(StatusKind, Selected, Priority, Evidence, Alternatives)) :-
    plan_for_frame(Frame, Environment, Plan),
    Plan = execution_plan(
        intent(ns(NS), name(Name)),
        provider(id(ProviderId), _Location, _Device, _SideEffect, _Auth),
        Status, _Arguments, evidence(Evidence), alternatives(Alternatives)),
    explanation_fields(Status, NS, Name, ProviderId,
        StatusKind, Selected, Priority),
    !.

explanation_fields(ready, NS, Name, ProviderId, ready, ProviderId, Priority) :-
    kb_capabilities:capability_provider(NS, Name, ProviderId, Priority).
explanation_fields(unavailable(_Reason), _NS, _Name, _ProviderId,
        unavailable, none, none).
explanation_fields(ambiguous, _NS, _Name, _ProviderId, ambiguous, none, none).
explanation_fields(denied(_Reason), _NS, _Name, _ProviderId,
        denied, none, none).

plan_stale(OldPlan, Frame, Environment, FreshPlan) :-
    plan_for_frame(Frame, Environment, Fresh0),
    stale_adjust(OldPlan, Fresh0, FreshPlan).

stale_adjust(OldPlan, Fresh0, FreshPlan) :-
    OldPlan = execution_plan(_, _, ready, _, _, _),
    Fresh0 = execution_plan(Intent, Provider, unavailable(_Reason),
        Arguments, _Evidence0, Alternatives),
    !,
    FreshPlan = execution_plan(Intent, Provider,
        unavailable(provider_missing), Arguments, evidence([]),
        Alternatives).
stale_adjust(_OldPlan, Fresh0, Fresh0).

%% ============================================================
%% Declared-provider qualification pipeline
%% ============================================================

declared_candidates(NS, Name, Principal, Auths, Devices, Providers,
        Aliases, Slots, Qualified, Denied, FailedAliases,
        FailedAvailability) :-
    findall(Outcome,
        ( kb_capabilities:capability_provider(NS, Name, ProviderId, Priority),
          kb_capabilities:capability_property(ProviderId, location(Location)),
          qualify(ProviderId, Priority, Location, Principal, Auths, Devices,
              Providers, Aliases, Slots, Outcome)
        ),
        Outcomes),
    partition_outcomes(Outcomes, Qualified, Denied, FailedAliases,
        FailedAvailability).

qualify(ProviderId, Priority, server, _Principal, Auths, _Devices,
        Providers, Aliases, Slots, Outcome) :-
    ( memberchk(ProviderId, Providers) ->
        ( alias_gate(ProviderId, Slots, Aliases) ->
            Candidate = candidate(Priority, ProviderId, server, none),
            auth_outcome(ProviderId, Auths, qualified0(Candidate), Outcome)
        ; Outcome = failed(alias)
        )
    ; Outcome = failed(availability)
    ).
qualify(ProviderId, Priority, device, Principal, Auths, Devices,
        _Providers, _Aliases, _Slots, Outcome) :-
    ( has_device_candidate(ProviderId, Devices, Principal) ->
        device_candidate(ProviderId, Devices, Principal, DeviceRef),
        Candidate = candidate(Priority, ProviderId, device, DeviceRef),
        auth_outcome(ProviderId, Auths, qualified0(Candidate), Outcome)
    ; Outcome = failed(availability)
    ).

has_device_candidate(ProviderId, Devices, Principal) :-
    device_candidate(ProviderId, Devices, Principal, _).

auth_outcome(ProviderId, Auths, qualified0(Candidate), Outcome) :-
    ( auth_satisfied(ProviderId, Auths) ->
        Outcome = qualified(Candidate)
    ; Outcome = denied(ProviderId)
    ).

auth_satisfied(ProviderId, Auths) :-
    ( kb_capabilities:capability_property(ProviderId, requires_auth(none)) ->
        true
    ; kb_capabilities:capability_property(ProviderId, requires_auth(Auth)),
      Auth \= none,
      memberchk(Auth, Auths)
    ).

device_candidate(ProviderId, Devices, Principal, DeviceRef) :-
    kb_capabilities:capability_property(ProviderId, capability(Cap)),
    member(device(Id, Owner, Caps), Devices),
    Owner == Principal,
    memberchk(Cap, Caps),
    Id = DeviceRef.

alias_gate(ProviderId, Slots, Aliases) :-
    ( alias_binding_slot(ProviderId, Slots, Alias) ->
        memberchk(alias(ProviderId, Alias), Aliases)
    ; true
    ).

alias_binding_slot(ProviderId, Slots, Alias) :-
    kb_capabilities:capability_binding(ProviderId, _ArgName, SlotName),
    slot_value(Slots, SlotName, ref(kind(app_alias), id(Alias))).

%% ============================================================
%% Outcome partitioning and decision
%% ============================================================

partition_outcomes([], [], [], [], []).
partition_outcomes([Outcome|Rest], Qualified, Denied, FailedAliases,
        FailedAvailability) :-
    partition_outcomes(Rest, QualifiedRest, DeniedRest, FailedAliasesRest,
        FailedAvailabilityRest),
    partition_one(Outcome, QualifiedRest, DeniedRest, FailedAliasesRest,
        FailedAvailabilityRest, Qualified, Denied, FailedAliases,
        FailedAvailability).

partition_one(qualified(Candidate), Q, D, FA, FV,
        [Candidate|Q], D, FA, FV).
partition_one(denied(ProviderId), Q, D, FA, FV,
        Q, [ProviderId|D], FA, FV).
partition_one(failed(availability), Q, D, FA, FV,
        Q, D, FA, [availability|FV]).
partition_one(failed(alias), Q, D, FA, FV,
        Q, D, [alias|FA], FV).

decide(Intent, Slots, Policies, Qualified, _Denied, _FailedAliases,
        _FailedAvailability, Plan) :-
    Qualified = [_|_],
    !,
    apply_policies(Policies, Qualified, Narrowed),
    Narrowed = [_|_],
    select_ready_or_ambiguous(Intent, Slots, Narrowed, Plan).
decide(Intent, _Slots, _Policies, [], Denied, _FailedAliases,
        _FailedAvailability, Plan) :-
    Denied = [_|_],
    !,
    denied_plan(Intent, Denied, Plan).
decide(Intent, _Slots, _Policies, [], [], FailedAliases,
        FailedAvailability, Plan) :-
    unavailable_plan(Intent, FailedAliases, FailedAvailability, Plan).

select_ready_or_ambiguous(Intent, Slots, Candidates, Plan) :-
    order_candidates(Candidates, [Top|Rest]),
    Top = candidate(Priority, _ProviderId, _Location, _DeviceRef),
    include_priority(Priority, Rest, Group),
    ( Group == [] ->
        ready_plan(Intent, Slots, Top, Plan)
    ; group_alternatives([Top|Group], Alternatives),
      ambiguous_plan(Intent, Alternatives, Plan)
    ).

ready_plan(Intent, Slots, candidate(Priority, ProviderId, Location, DeviceRef),
        Plan) :-
    kb_capabilities:capability_property(ProviderId, side_effect(SideEffect)),
    kb_capabilities:capability_property(ProviderId, requires_auth(Auth)),
    provider_evidence(Priority, ProviderId, Location, DeviceRef, Evidence),
    plan_arguments(ProviderId, Slots, Arguments),
    Intent = intent(ns(NS), name(Name)),
    Plan = execution_plan(
        intent(ns(NS), name(Name)),
        provider(id(ProviderId), location(Location), device(DeviceRef),
                 side_effect(SideEffect), requires_auth(Auth)),
        ready,
        arguments(Arguments),
        evidence(Evidence),
        alternatives([])).

group_alternatives(Candidates, Alternatives) :-
    findall(Alternative,
        ( member(candidate(_Priority, ProviderId, Location, DeviceRef),
                 Candidates),
          alternative_atom(ProviderId, Location, DeviceRef, Alternative)
        ),
        RawAlternatives),
    sort(RawAlternatives, Sorted),
    plan_alternative_cap(Cap),
    take(Sorted, Cap, Alternatives).

alternative_atom(ProviderId, device, DeviceRef, Alternative) :-
    format(atom(Alternative), '~w@~w', [ProviderId, DeviceRef]).
alternative_atom(ProviderId, server, none, ProviderId).

ambiguous_plan(Intent, Alternatives, Plan) :-
    Intent = intent(ns(NS), name(Name)),
    Plan = execution_plan(
        intent(ns(NS), name(Name)),
        provider(id(none), location(none), device(none),
                 side_effect(none), requires_auth(none)),
        ambiguous,
        arguments([]),
        evidence([]),
        alternatives(Alternatives)).

denied_plan(Intent, Denied, Plan) :-
    denied_provider_evidence(Denied, Evidence),
    Intent = intent(ns(NS), name(Name)),
    Plan = execution_plan(
        intent(ns(NS), name(Name)),
        provider(id(none), location(none), device(none),
                 side_effect(none), requires_auth(none)),
        denied(insufficient_authorization),
        arguments([]),
        evidence(Evidence),
        alternatives([])).

denied_provider_evidence(Denied, Evidence) :-
    Denied = [TopDenied|_],
    kb_capabilities:capability_property(TopDenied, requires_auth(Auth)),
    Auth \= none,
    format(atom(RequiresAtom), 'requires(~w)', [Auth]),
    format(atom(DeniedAtom), 'denied(~w)', [TopDenied]),
    Evidence = [RequiresAtom, DeniedAtom].

unavailable_plan(Intent, FailedAliases, FailedAvailability, Plan) :-
    unavailable_reason(FailedAliases, FailedAvailability, Reason),
    Intent = intent(ns(NS), name(Name)),
    Plan = execution_plan(
        intent(ns(NS), name(Name)),
        provider(id(none), location(none), device(none),
                 side_effect(none), requires_auth(none)),
        unavailable(Reason),
        arguments([]),
        evidence([]),
        alternatives([])).

unavailable_reason(FailedAliases, _FailedAvailability, alias_unsupported) :-
    FailedAliases = [_|_],
    !.
unavailable_reason([], FailedAvailability, provider_unavailable) :-
    FailedAvailability = [_|_],
    !.
unavailable_reason([], [], no_provider).

%% ============================================================
%% Policy narrowing
%% ============================================================

apply_policies([], Candidates, Candidates).
apply_policies([Policy|Rest], Candidates, Final) :-
    ( policy_filter(Policy, Candidates, Narrowed),
      Narrowed = [_|_] ->
        apply_policies(Rest, Narrowed, Final)
    ; apply_policies(Rest, Candidates, Final)
    ).

policy_filter(prefer(location(Location)), Candidates, Filtered) :-
    location_candidates(Location, Candidates, Filtered).
policy_filter(prefer(device(DeviceId)), Candidates, Filtered) :-
    device_candidates(DeviceId, Candidates, Filtered).

location_candidates(_, [], []).
location_candidates(Location, [Candidate|Rest], [Candidate|Filtered]) :-
    Candidate = candidate(_, _, Location, _),
    !,
    location_candidates(Location, Rest, Filtered).
location_candidates(Location, [_|Rest], Filtered) :-
    location_candidates(Location, Rest, Filtered).

device_candidates(_, [], []).
device_candidates(DeviceId, [Candidate|Rest], [Candidate|Filtered]) :-
    Candidate = candidate(_, _, _, DeviceId),
    !,
    device_candidates(DeviceId, Rest, Filtered).
device_candidates(DeviceId, [_|Rest], Filtered) :-
    device_candidates(DeviceId, Rest, Filtered).

%% ============================================================
%% Ordering helpers
%% ============================================================

order_candidates(Candidates, Ordered) :-
    findall(Rank-Candidate,
        ( member(Candidate, Candidates),
          Candidate = candidate(Priority, _ProviderId, _Location, _DeviceRef),
          Rank is 1000000 - Priority
        ),
        Keyed),
    keysort(Keyed, SortedKeys),
    pairs_values(SortedKeys, Ordered).

pairs_values([], []).
pairs_values([_-Candidate|Rest], [Candidate|OrderedRest]) :-
    pairs_values(Rest, OrderedRest).

include_priority(_, [], []).
include_priority(Priority, [Candidate|Rest], [Candidate|Group]) :-
    Candidate = candidate(Priority, _, _, _),
    !,
    include_priority(Priority, Rest, Group).
include_priority(Priority, [_|Rest], Group) :-
    include_priority(Priority, Rest, Group).

take(List, Cap, Taken) :-
    length(Prefix, Cap),
    append(Prefix, _, List),
    !,
    Taken = Prefix.
take(List, _, List).

%% ============================================================
%% Arguments and evidence
%% ============================================================

plan_arguments(ProviderId, Slots, Arguments) :-
    findall(argument(name(ArgName), value(Value)),
        ( kb_capabilities:capability_binding(ProviderId, ArgName, SlotName),
          slot_value(Slots, SlotName, Value)
        ),
        Arguments).

provider_evidence(Priority, ProviderId, device, DeviceRef, Evidence) :-
    !,
    format(atom(PrioAtom), 'prio(~w)', [Priority]),
    kb_capabilities:capability_property(ProviderId, capability(Cap)),
    format(atom(CapAtom), 'cap(~w)', [Cap]),
    format(atom(DevAtom), 'dev(~w)', [DeviceRef]),
    Evidence = [PrioAtom, CapAtom, DevAtom].
provider_evidence(Priority, ProviderId, server, none, Evidence) :-
    format(atom(PrioAtom), 'prio(~w)', [Priority]),
    kb_capabilities:capability_property(ProviderId, capability(Cap)),
    format(atom(CapAtom), 'cap(~w)', [Cap]),
    Evidence = [PrioAtom, CapAtom].

%% ============================================================
%% Slot helpers
%% ============================================================

slot_value(Slots, SlotName, Value) :-
    member(slot(name(SlotName), value(Value), _Origin), Slots).

all_bound_slots([]).
all_bound_slots([slot(name(_), value(Value), _)|Rest]) :-
    bound_value(Value),
    all_bound_slots(Rest).

bound_value(text(Atom)) :-
    atom(Atom),
    atom_length(Atom, Length),
    plan_text_bound(Bound),
    Length =< Bound.
bound_value(duration(Seconds)) :- integer(Seconds).
bound_value(number(Number)) :- number(Number).
bound_value(boolean(Bool)) :- ( Bool == true ; Bool == false ).
bound_value(ref(kind(Kind), id(Id))) :-
    atom(Kind),
    atom(Id).
bound_value(datetime(Y, Mo, D, H, Mi, S)) :-
    integer(Y), integer(Mo), integer(D), integer(H), integer(Mi),
    integer(S).

%% ============================================================
%% Flat projections for marshalling (pyswip boundary)
%% ============================================================

plan_head_row(Plans, Idx, NS, Name, StatusKind, Reason, ProviderId, Location,
        DeviceRef, SideEffect, RequiresAuth, Evidence, Alternatives) :-
    nth0(Idx, Plans, execution_plan(
        intent(ns(NS), name(Name)),
        provider(id(ProviderId), location(Location), device(DeviceRef),
                 side_effect(SideEffect), requires_auth(RequiresAuth)),
        Status,
        _Arguments,
        evidence(Evidence),
        alternatives(Alternatives))),
    plan_status_fields(Status, StatusKind, Reason).

plan_status_fields(ready, ready, none).
plan_status_fields(unavailable(Reason), unavailable, Reason).
plan_status_fields(ambiguous, ambiguous, none).
plan_status_fields(denied(Reason), denied, Reason).

plan_arg_row(Plans, Idx, ArgIdx, ArgName, ValueKind, A1, A2) :-
    nth0(Idx, Plans, execution_plan(_, _, _, arguments(Args), _, _)),
    nth0(ArgIdx, Args, argument(name(ArgName), value(Value))),
    plan_value_fields(Value, ValueKind, A1, A2).

plan_value_fields(text(Atom), text, Atom, none).
plan_value_fields(duration(Seconds), duration, Seconds, none).
plan_value_fields(number(Number), number, Number, none).
plan_value_fields(boolean(Bool), boolean, Bool, none).
plan_value_fields(ref(kind(Kind), id(Id)), ref, Kind, Id).
plan_value_fields(datetime(Y, Mo, D, H, Mi, S), datetime,
    [Y, Mo, D, H, Mi, S], none).
