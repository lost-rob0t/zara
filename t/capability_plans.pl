% Capability plan selection tests (issue #157).
%
% Pins the pure selection pipeline: kb/capabilities.pl facts + plan_for_frame/3
% outcomes (ready/unavailable/ambiguous/denied), candidate ordering, policy
% preference, stale re-derivation, and the flat projections used by pyswip.
% Contract: docs/intentframe-contract.md examples 12/13; rage/157 design D1-D8.

:- begin_tests(capability_plans).

:- use_module('../kb/capabilities').
:- use_module('../modules/capability_plans').

% --- fixtures ---------------------------------------------------------------

frame_open(Target, frame(intent(ns(app), name(open)),
    [slot(name(target), value(ref(kind(app_alias), id(Target))), origin(utterance))],
    complete)).

frame_search(Query, frame(intent(ns(web), name(search)),
    [slot(name(query), value(text(Query)), origin(utterance))],
    complete)).

frame_timer(Slots, frame(intent(ns(device), name('timer.set')), Slots, complete)).

env_alice(Environment) :-
    Environment = environment(principal(alice), auths([]),
        devices([]), providers([]), aliases([]), policies([])).

env_desktop(Environment) :-
    Environment = environment(principal(alice), auths([]),
        devices([]),
        providers([open_desktop, search_server, timer_server]),
        aliases([alias(open_desktop, firefox), alias(open_desktop, editor)]),
        policies([])).

env_android(Environment) :-
    Environment = environment(principal(alice), auths([]),
        devices([device(droid, alice, ['app.open', 'timer.set'])]),
        providers([]), aliases([]), policies([])).

env_both(Environment) :-
    Environment = environment(principal(alice), auths([]),
        devices([device(droid, alice, ['app.open', 'timer.set'])]),
        providers([open_desktop, search_server, timer_server]),
        aliases([alias(open_desktop, firefox)]),
        policies([])).

frame_unknown(frame(intent(ns(device), name('warp.drive')), [], complete)).

frame_pause(frame(intent(ns(media), name(pause)), [], complete)).

% --- example 6/13: open selects the initiating-device provider ---------------

test(open_app_ready_when_device_advertises) :-
    frame_open(firefox, Frame),
    env_android(Env),
    capability_plans:plan_for_frame(Frame, Env, Plan),
    assertion(Plan = execution_plan(
        intent(ns(app), name(open)),
        provider(id(open_app), location(device), device(droid),
                 side_effect(external), requires_auth(none)),
        ready,
        arguments([argument(name(app),
                            value(ref(kind(app_alias), id(firefox))))]),
        _Evidence,
        alternatives([]))).

test(open_desktop_ready_when_alias_served) :-
    frame_open(firefox, Frame),
    env_desktop(Env),
    capability_plans:plan_for_frame(Frame, Env, Plan),
    assertion(Plan = execution_plan(
        intent(ns(app), name(open)),
        provider(id(open_desktop), location(server), device(none),
                 side_effect(external), requires_auth(none)),
        ready,
        arguments([argument(name(app),
                            value(ref(kind(app_alias), id(firefox))))]),
        _Evidence,
        alternatives([]))).

test(open_desktop_wins_when_device_advertises_highest_priority) :-
    frame_open(firefox, Frame),
    env_both(Env),
    capability_plans:plan_for_frame(Frame, Env, Plan),
    assertion(Plan = execution_plan(
        intent(ns(app), name(open)),
        provider(id(open_app), location(device), device(droid),
                 side_effect(external), requires_auth(none)),
        ready, _, _, alternatives([]))).

% --- example 13: same frame, different platform, typed unavailability --------

test(open_termux_desktop_alias_unsupported) :-
    frame_open(termux, Frame),
    env_desktop(Env),
    capability_plans:plan_for_frame(Frame, Env, Plan),
    assertion(Plan = execution_plan(
        intent(ns(app), name(open)),
        provider(id(none), location(none), device(none),
                 side_effect(none), requires_auth(none)),
        unavailable(alias_unsupported), arguments([]), evidence([]),
        alternatives([]))).

test(open_no_reachable_provider_is_provider_unavailable) :-
    frame_open(firefox, Frame),
    env_alice(Env),
    capability_plans:plan_for_frame(Frame, Env, Plan),
    assertion(Plan = execution_plan(
        intent(ns(app), name(open)),
        provider(id(none), location(none), device(none),
                 side_effect(none), requires_auth(none)),
        unavailable(provider_unavailable), arguments([]), evidence([]),
        alternatives([]))).

test(open_termux_android_ready_run_command_shape) :-
    frame_open(termux, Frame),
    env_android(Env),
    capability_plans:plan_for_frame(Frame, Env, Plan),
    assertion(Plan = execution_plan(
        intent(ns(app), name(open)),
        provider(id(open_app), location(device), device(droid),
                 side_effect(external), requires_auth(none)),
        ready, _, _, alternatives([]))).

% --- example 9: search chooses the server search provider --------------------

test(search_selects_server_provider) :-
    frame_search('prolog dictionaries', Frame),
    env_desktop(Env),
    capability_plans:plan_for_frame(Frame, Env, Plan),
    assertion(Plan = execution_plan(
        intent(ns(web), name(search)),
        provider(id(search_server), location(server), device(none),
                 side_effect(external), requires_auth(none)),
        ready,
        arguments([argument(name(query),
                            value(text('prolog dictionaries')))]),
        _, alternatives([]))).

% --- timer: explicit policy and availability ---------------------------------

frame_timer_duration(frame(intent(ns(device), name('timer.set')),
    [slot(name(duration), value(duration(120)), origin(utterance))],
    complete)).

test(timer_default_policy_highest_priority_server) :-
    frame_timer([slot(name(duration), value(duration(120)), origin(utterance))], Frame),
    env_both(Env),
    capability_plans:plan_for_frame(Frame, Env, Plan),
    assertion(Plan = execution_plan(
        intent(ns(device), name('timer.set')),
        provider(id(timer_server), location(server), device(none),
                 side_effect(local), requires_auth(none)),
        ready, _, _, alternatives([]))).

test(timer_policy_prefers_device) :-
    frame_timer_duration(Frame),
    Env = environment(principal(alice), auths([]),
        devices([device(droid, alice, ['app.open', 'timer.set'])]),
        providers([open_desktop, search_server, timer_server]),
        aliases([alias(open_desktop, firefox)]),
        policies([prefer(location(device))])),
    capability_plans:plan_for_frame(Frame, Env, Plan),
    assertion(Plan = execution_plan(
        intent(ns(device), name('timer.set')),
        provider(id(timer_device), location(device), device(droid),
                 side_effect(local), requires_auth(none)),
        ready, _, _, alternatives([]))).

test(timer_preferred_location_unavailable_falls_back) :-
    frame_timer_duration(Frame),
    Env = environment(principal(alice), auths([]),
        devices([]),
        providers([open_desktop, search_server, timer_server]),
        aliases([alias(open_desktop, firefox)]),
        policies([prefer(location(device))])),
    capability_plans:plan_for_frame(Frame, Env, Plan),
    assertion(Plan = execution_plan(
        intent(ns(device), name('timer.set')),
        provider(id(timer_server), location(server), device(none),
                 side_effect(local), requires_auth(none)),
        ready, _, _, alternatives([]))).

test(timer_targeting_policy_selects_named_device) :-
    frame_timer_duration(Frame),
    Env = environment(principal(alice), auths([]),
        devices([device(d1, alice, ['timer.set']),
                 device(d2, alice, ['timer.set'])]),
        providers([]), aliases([]),
        policies([prefer(device(d2))])),
    capability_plans:plan_for_frame(Frame, Env, Plan),
    assertion(Plan = execution_plan(
        intent(ns(device), name('timer.set')),
        provider(id(timer_device), location(device), device(d2),
                 side_effect(local), requires_auth(none)),
        ready, _, _, alternatives([]))).

test(timer_optional_label_bound_when_present) :-
    frame_timer([slot(name(duration), value(duration(120)), origin(utterance)),
                 slot(name(label), value(text(tea)), origin(utterance))], Frame),
    env_desktop(Env),
    capability_plans:plan_for_frame(Frame, Env, Plan),
    assertion(Plan = execution_plan(
        intent(ns(device), name('timer.set')),
        provider(id(timer_server), _, _, _, _),
        ready,
        arguments([argument(name(duration), value(duration(120))),
                   argument(name(label), value(text(tea)))]),
        _, alternatives([]))).

% --- server admin provider requires authorization independent of authn -------

env_admin(Environment) :-
    Environment = environment(principal(alice),
        auths(['daemon.admin']),
        devices([device(evil, alice, ['daemon.admin'])]),
        providers([admin_restart]), aliases([]), policies([])).

test(admin_provider_denied_without_authorization) :-
    frame_admin(Frame),
    env_noauth(Env),
    capability_plans:plan_for_frame(Frame, Env, Plan),
    assertion(Plan = execution_plan(
        intent(ns(skill), name('admin.restart')),
        provider(id(none), location(none), device(none),
                 side_effect(none), requires_auth(none)),
        denied(insufficient_authorization), arguments([]),
        evidence(Evidence), alternatives([]))),
    assertion(memberchk(requires('daemon.admin'), Evidence)).

test(admin_provider_ready_with_authorization) :-
    frame_admin(Frame),
    env_admin(Env),
    capability_plans:plan_for_frame(Frame, Env, Plan),
    assertion(Plan = execution_plan(
        intent(ns(skill), name('admin.restart')),
        provider(id(admin_restart), location(server), device(none),
                 side_effect(external), requires_auth('daemon.admin')),
        ready, arguments([]), _, alternatives([]))).

frame_admin(frame(intent(ns(skill), name('admin.restart')), [], complete)).

env_noauth(environment(principal(alice), auths([]),
    devices([device(evil, alice, ['daemon.admin'])]),
    providers([admin_restart]), aliases([]), policies([]))).

test(malicious_device_advertisement_cannot_acquire_admin_capability) :-
    frame_admin(Frame),
    env_advert_only(Env),
    capability_plans:plan_for_frame(Frame, Env, Plan),
    assertion(Plan = execution_plan(
        intent(ns(skill), name('admin.restart')),
        provider(id(none), location(none), device(none),
                 side_effect(none), requires_auth(none)),
        unavailable(provider_unavailable), arguments([]), evidence([]),
        alternatives([]))).

env_advert_only(environment(principal(alice), auths([]),
    devices([device(evil, alice, ['daemon.admin'])]),
    providers([]), aliases([]), policies([]))).

% --- deterministic ties and ambiguity ----------------------------------------

test(two_devices_same_provider_is_ambiguous_with_device_alternatives) :-
    frame_open(firefox, Frame),
    Env = environment(principal(alice), auths([]),
        devices([device(d1, alice, ['app.open']),
                 device(d2, alice, ['app.open'])]),
        providers([]), aliases([]), policies([])),
    capability_plans:plan_for_frame(Frame, Env, Plan),
    assertion(Plan = execution_plan(
        intent(ns(app), name(open)),
        provider(id(none), location(none), device(none),
                 side_effect(none), requires_auth(none)),
        ambiguous, arguments([]), evidence([]),
        alternatives(['open_app@d1', 'open_app@d2']))).

test(provider_priority_tie_is_ambiguous_with_sorted_alternatives) :-
    frame_pause(Frame),
    Env = environment(principal(alice), auths([]),
        devices([device(d1, alice, ['media.pause'])]),
        providers([pause_server]), aliases([]), policies([])),
    capability_plans:plan_for_frame(Frame, Env, Plan),
    assertion(Plan = execution_plan(
        intent(ns(media), name(pause)),
        provider(id(none), location(none), device(none),
                 side_effect(none), requires_auth(none)),
        ambiguous, arguments([]), evidence([]),
        alternatives(['pause_device@d1', 'pause_server']))).

test(ambiguous_alternatives_capped_at_eight) :-
    frame_open(firefox, Frame),
    findall(device(Id, alice, ['app.open']),
            ( between(1, 10, N),
              atom_number(Raw, N),
              atom_concat(d, Raw, Id) ),
            Devices),
    Env = environment(principal(alice), auths([]),
        devices(Devices), providers([]), aliases([]), policies([])),
    capability_plans:plan_for_frame(Frame, Env, Plan),
    assertion(Plan = execution_plan(
        intent(ns(app), name(open)), _, ambiguous, _,
        _, alternatives(Alternatives))),
    assertion(length(Alternatives, 8)).

% --- candidate ordering and unknown facts ------------------------------------

test(candidates_ordered_by_descending_priority) :-
    frame_open(firefox, Frame),
    env_both(Env),
    capability_plans:plan_candidates(Frame, Env, Candidates),
    assertion(Candidates = [candidate(100, open_app, device, droid),
                            candidate(50, open_desktop, server, none)]).

test(candidates_empty_for_unknown_intent) :-
    frame_unknown(Frame),
    env_desktop(Env),
    capability_plans:plan_candidates(Frame, Env, Candidates),
    assertion(Candidates == []).

test(unknown_provider_in_environment_is_ignored) :-
    frame_search('x', Frame),
    Env = environment(principal(alice), auths([]),
        devices([]), providers([ghost_provider]), aliases([]), policies([])),
    capability_plans:plan_candidates(Frame, Env, Candidates),
    assertion(Candidates == []).

test(unknown_intent_yields_typed_unavailable) :-
    frame_unknown(Frame),
    env_desktop(Env),
    capability_plans:plan_for_frame(Frame, Env, Plan),
    assertion(Plan = execution_plan(
        intent(ns(device), name('warp.drive')),
        provider(id(none), location(none), device(none),
                 side_effect(none), requires_auth(none)),
        unavailable(no_provider), arguments([]), evidence([]),
        alternatives([]))).

% --- cross-principal isolation ------------------------------------------------

test(other_principals_device_is_invisible) :-
    frame_open(firefox, Frame),
    Env = environment(principal(bob), auths([]),
        devices([device(d1, alice, ['app.open'])]),
        providers([]), aliases([]), policies([])),
    capability_plans:plan_for_frame(Frame, Env, Plan),
    assertion(Plan = execution_plan(
        intent(ns(app), name(open)),
        provider(id(none), location(none), device(none),
                 side_effect(none), requires_auth(none)),
        unavailable(provider_unavailable), arguments([]), evidence([]),
        alternatives([]))).

% --- stale re-derivation -------------------------------------------------------

test(stale_ready_plan_becomes_provider_missing) :-
    frame_open(firefox, Frame),
    env_android(Env),
    capability_plans:plan_for_frame(Frame, Env, OldPlan),
    EmptyEnv = environment(principal(alice), auths([]),
        devices([]), providers([]), aliases([]), policies([])),
    capability_plans:plan_stale(OldPlan, Frame, EmptyEnv, FreshPlan),
    assertion(FreshPlan = execution_plan(
        intent(ns(app), name(open)),
        provider(id(none), location(none), device(none),
                 side_effect(none), requires_auth(none)),
        unavailable(provider_missing), arguments([]), evidence([]),
        alternatives([]))).

test(fresh_environment_keeps_ready_plan) :-
    frame_open(firefox, Frame),
    env_android(Env),
    capability_plans:plan_for_frame(Frame, Env, OldPlan),
    capability_plans:plan_stale(OldPlan, Frame, Env, FreshPlan),
    assertion(FreshPlan == OldPlan).

test(stale_recheck_with_policy_change_gives_new_ready_plan) :-
    frame_timer_duration(Frame),
    env_both(Env0),
    capability_plans:plan_for_frame(Frame, Env0, OldPlan),
    assertion(OldPlan = execution_plan(_, provider(id(timer_server), _, _, _, _), ready, _, _, _)),
    Env1 = environment(principal(alice), auths([]),
        devices([device(droid, alice, ['app.open', 'timer.set'])]),
        providers([open_desktop, search_server, timer_server]),
        aliases([alias(open_desktop, firefox)]),
        policies([prefer(location(device))])),
    capability_plans:plan_stale(OldPlan, Frame, Env1, FreshPlan),
    assertion(FreshPlan = execution_plan(
        intent(ns(device), name('timer.set')),
        provider(id(timer_device), location(device), device(droid),
                 side_effect(local), requires_auth(none)),
        ready, _, _, alternatives([]))).

% --- closed inputs --------------------------------------------------------------

test(incomplete_frame_has_no_plan) :-
    OpenFrame = frame(intent(ns(device), name('timer.set')), [], missing([duration])),
    env_android(Env),
    \+ capability_plans:plan_for_frame(OpenFrame, Env, _).

test(oversized_text_argument_fails_closed) :-
    length(CharCodes, 600),
    maplist(=(0'a), CharCodes),
    atom_codes(BigText, CharCodes),
    OversizeFrame = frame(intent(ns(web), name(search)),
        [slot(name(query), value(text(BigText)), origin(utterance))],
        complete),
    env_desktop(Env),
    \+ capability_plans:plan_for_frame(OversizeFrame, Env, _).

% --- evidence is structured and deterministic -----------------------------------

test(ready_plan_evidence_pinned) :-
    frame_open(firefox, Frame),
    env_android(Env),
    capability_plans:plan_for_frame(Frame, Env, Plan),
    assertion(Plan = execution_plan(_, _, ready, _,
        evidence(['prio(100)', 'cap(app.open)', 'dev(droid)']), _)).

test(explanation_shape_for_ready_plan) :-
    frame_open(firefox, Frame),
    env_android(Env),
    capability_plans:plan_explanation(Frame, Env,
        explanation(ready, open_app, 100, Evidence, [])),
    assertion(Evidence == ['prio(100)', 'cap(app.open)', 'dev(droid)']).

% --- flat projections (pyswip boundary) ----------------------------------------

test(plan_head_row_projects_flat_fields) :-
    frame_open(firefox, Frame),
    env_android(Env),
    Plans = [Plan],
    capability_plans:plan_for_frame(Frame, Env, Plan),
    capability_plans:plan_head_row(Plans, 0, NS, Name, StatusKind, Reason,
        ProviderId, Location, DeviceRef, SideEffect, RequiresAuth,
        Evidence, Alternatives),
    assertion(NS == app),
    assertion(Name == open),
    assertion(StatusKind == ready),
    assertion(Reason == none),
    assertion(ProviderId == open_app),
    assertion(Location == device),
    assertion(DeviceRef == droid),
    assertion(SideEffect == external),
    assertion(RequiresAuth == none),
    assertion(Evidence == ['prio(100)', 'cap(app.open)', 'dev(droid)']),
    assertion(Alternatives == []).

test(plan_arg_row_projects_typed_values) :-
    frame_open(firefox, Frame),
    env_android(Env),
    Plans = [Plan],
    capability_plans:plan_for_frame(Frame, Env, Plan),
    capability_plans:plan_arg_row(Plans, 0, 0, ArgName, ValueKind, A1, A2),
    assertion(ArgName == app),
    assertion(ValueKind == ref),
    assertion(A1 == app_alias),
    assertion(A2 == firefox).


% --- property-style loops over the whole declared KB -------------------------

test(every_declared_intent_yields_a_typed_plan_without_availability) :-
    EmptyEnv = environment(principal(nobody), auths([]),
        devices([]), providers([]), aliases([]), policies([])),
    forall(
        ( kb_capabilities:capability_provider(NS, Name, _, _),
          frame_for_intent(NS, Name, Frame)
        ),
        ( capability_plans:plan_for_frame(Frame, EmptyEnv, Plan),
          Plan = execution_plan(_, _, Status, _, _, _),
          member(Status, [ready, unavailable(_), ambiguous, denied(_)])
        )
    ).

frame_for_intent(NS, Name, frame(intent(ns(NS), name(Name)), Slots, complete)) :-
    findall(slot(name(SlotName), value(Value), origin(utterance)),
        ( kb_capabilities:capability_binding(_Provider, _, SlotName),
          slot_value_for_type(Value)
        ),
        Slots0),
    sort(Slots0, Slots).

slot_value_for_type(text(example)).
slot_value_for_type(duration(30)).
slot_value_for_type(ref(kind(app_alias), id(firefox))).
slot_value_for_type(ref(kind(contact), id(alice))).
slot_value_for_type(ref(kind(media_alias), id(news))).

test(candidates_always_ordered_by_descending_priority) :-
    OpenEnv = environment(principal(alice), auths(['daemon.admin']),
        devices([device(d1, alice, ['app.open', 'timer.set',
                                   'screen.capture', 'media.pause'])]),
        providers([open_desktop, search_server, timer_server,
                   screen_server, pause_server, admin_restart]),
        aliases([alias(open_desktop, firefox)]), policies([])),
    forall(
        frame_for_any_intent(Frame),
        ( capability_plans:plan_candidates(Frame, OpenEnv, Candidates),
          priorities_descending(Candidates)
        )
    ).

frame_for_any_intent(Frame) :-
    kb_capabilities:capability_provider(NS, Name, _, _),
    frame_for_intent(NS, Name, Frame).

priorities_descending([]).
priorities_descending([candidate(P, _, _, _)|Rest]) :-
    priorities_descending_from(P, Rest).

priorities_descending_from(_, []).
priorities_descending_from(P, [candidate(P2, _, _, _)|Rest]) :-
    P >= P2,
    priorities_descending_from(P2, Rest).

:- end_tests(capability_plans).
