:- module(kb_notification_policy,
    [
        notification_source_allowed/1,
        notification_source_denied/1,
        notification_content_policy/2,
        notification_route_policy/2,
        notification_filter/3,
        notification_hook/3,
        notification_match_action/3,
        notification_ai_policy/2,
        notification_spam_policy/2,
        notification_feedback/2,
        notification_source_decision/2,
        notification_content_mode/2,
        notification_route_target/2,
        notification_filter_decision/5,
        notification_spam_decision/6,
        notification_hook_for/6
    ]).

/** <module> Deterministic notification routing policy.

This module is the symbolic policy layer for issue #1376. It owns no platform
listener, transport, database, scheduler, provider runtime, or effect executor.
User facts are loaded through modules/config_loader.pl into this module and may
be reloaded without rebuilding Zara.
*/

:- dynamic notification_source_allowed/1.
:- dynamic notification_source_denied/1.
:- dynamic notification_content_policy/2.
:- dynamic notification_route_policy/2.
:- dynamic notification_filter/3.
:- dynamic notification_hook/3.
:- dynamic notification_match_action/3.
:- dynamic notification_ai_policy/2.
:- dynamic notification_spam_policy/2.
:- dynamic notification_feedback/2.

% Conservative defaults. User config is asserted before these clauses and the
% exported resolvers cut on the first applicable fact.
notification_content_policy(default, metadata_only).
notification_route_policy(default, most_recently_active).
notification_ai_policy(default, disabled).
notification_spam_policy(default, smart).

notification_source_decision(App, deny) :-
    notification_source_denied(App), !.
notification_source_decision(App, allow) :-
    notification_source_allowed(App), !.
notification_source_decision(_, allow).

notification_content_mode(App, Mode) :-
    notification_content_policy(App, Mode), !.
notification_content_mode(_, Mode) :-
    notification_content_policy(default, Mode), !.

notification_route_target(App, Policy) :-
    notification_route_policy(App, Policy), !.
notification_route_target(_, Policy) :-
    notification_route_policy(default, Policy), !.

notification_filter_decision(App, Category, Importance, Decision, RuleId) :-
    notification_filter(RuleId, Match, Decision),
    notification_match(Match, App, Category, Importance),
    !.

notification_hook_for(App, Category, Importance, HookId, Kind, Arg) :-
    notification_hook(HookId, Match, Action),
    notification_match(Match, App, Category, Importance),
    notification_action_wire(Action, Kind, Arg).
notification_hook_for(App, Category, Importance, HookId, Kind, Arg) :-
    notification_match_action(HookId, Match, Actions),
    notification_match(Match, App, Category, Importance),
    member(Action, Actions),
    notification_action_wire(Action, Kind, Arg).

notification_match(any, _, _, _).
notification_match(app(App), App, _, _).
notification_match(category(Category), _, Category, _).
notification_match(importance(Importance), _, _, Importance).
notification_match(app_category(App, Category), App, Category, _).
notification_match(app_importance(App, Importance), App, _, Importance).

notification_action_wire(dismiss, dismiss, none).
notification_action_wire(open, open, none).
notification_action_wire(invoke_action(ActionId), invoke_action, ActionId).
notification_action_wire(inline_reply, inline_reply, none).
notification_action_wire(start_workflow(WorkflowId), start_workflow, WorkflowId).
notification_action_wire(open_link(LinkId), open_link, LinkId).
notification_action_wire(create_todo, create_todo, none).
notification_action_wire(capture_note, capture_note, none).
notification_action_wire(route_peer(PeerId), route_peer, PeerId).

% Deterministic anti-spam expert. Explicit durable feedback is stronger than
% runtime heuristics; observations alone never become permanent preferences.
% Runtime feedback is passed in by the durable router store. When there is no
% runtime preference, the same user-authored Prolog overlay can provide one.
notification_spam_decision(App, Count, Duplicate, none, Decision, Reason) :-
    notification_feedback(App, Feedback), !,
    notification_spam_decision(App, Count, Duplicate, Feedback, Decision, Reason).
notification_spam_decision(App, Count, Duplicate, none, Decision, Reason) :-
    notification_feedback(default, Feedback), !,
    notification_spam_decision(App, Count, Duplicate, Feedback, Decision, Reason).
notification_spam_decision(_, _, _, always_allow, allow, explicit_always_allow) :- !.
notification_spam_decision(_, _, _, mute, suppress, explicit_mute) :- !.
notification_spam_decision(_, _, _, digest, digest, explicit_digest) :- !.
notification_spam_decision(_, _, true, _, group, duplicate) :- !.
notification_spam_decision(_, Count, false, _, digest, burst_digest) :-
    Count >= 12, !.
notification_spam_decision(_, Count, false, _, group, burst_group) :-
    Count >= 6, !.
notification_spam_decision(App, _, false, _, Decision, configured_policy) :-
    notification_spam_policy(App, Policy),
    spam_policy_decision(Policy, Decision), !.
notification_spam_decision(_, _, false, _, Decision, default_policy) :-
    notification_spam_policy(default, Policy),
    spam_policy_decision(Policy, Decision), !.

spam_policy_decision(smart, allow).
spam_policy_decision(allow, allow).
spam_policy_decision(suppress, suppress).
spam_policy_decision(group, group).
spam_policy_decision(digest, digest).
spam_policy_decision(ask, ask).
