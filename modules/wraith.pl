:- module(wraith, [
    wraith_role/1,
    wraith_state/1,
    wraith_terminal_state/1,
    wraith_message_type/1,
    valid_budget/1,
    valid_agent_spec/1,
    valid_message/1,
    task_transition/2,
    may_spawn/5
]).

% Wraith defines Zara's logical subagent contract.
% Execution remains behind Zara's runtime boundary; these predicates do not
% grant process, tool, filesystem, network, or model authority.

wraith_role(supervisor).
wraith_role(worker).
wraith_role(reviewer).
wraith_role(researcher).
wraith_role(grader).
wraith_role(optimizer).
wraith_role(promotion_gate).

wraith_state(created).
wraith_state(ready).
wraith_state(running).
wraith_state(paused).
wraith_state(completed).
wraith_state(failed).
wraith_state(cancelled).

wraith_terminal_state(completed).
wraith_terminal_state(failed).
wraith_terminal_state(cancelled).

wraith_message_type(task).
wraith_message_type(result).
wraith_message_type(observation).
wraith_message_type(request).
wraith_message_type(vote).
wraith_message_type(critique).
wraith_message_type(status).
wraith_message_type(failure).
wraith_message_type(cancel).
wraith_message_type(budget_warning).

valid_budget(budget(Kind, Limit)) :-
    valid_identifier(Kind),
    integer(Limit),
    Limit >= 0.

valid_agent_spec(
    agent_spec(Id, Profile, Parent, RuntimeId, Generation, Capabilities, Budgets)
) :-
    valid_identifier(Id),
    valid_identifier(Profile),
    valid_parent(Id, Parent),
    valid_identifier(RuntimeId),
    integer(Generation),
    Generation > 0,
    valid_identifier_list(Capabilities, 64),
    valid_budget_list(Budgets, 32).

valid_message(
    wraith_message(Id, Sender, Recipient, Type, PayloadRef, TraceRef)
) :-
    valid_identifier(Id),
    valid_identifier(Sender),
    valid_identifier(Recipient),
    wraith_message_type(Type),
    valid_ref(payload, PayloadRef),
    valid_trace_ref(TraceRef).

task_transition(created, ready).
task_transition(created, failed).
task_transition(created, cancelled).
task_transition(ready, running).
task_transition(ready, failed).
task_transition(ready, cancelled).
task_transition(running, paused).
task_transition(running, completed).
task_transition(running, failed).
task_transition(running, cancelled).
task_transition(paused, running).
task_transition(paused, failed).
task_transition(paused, cancelled).

may_spawn(ParentState, ChildCount, MaxChildren, RemainingBudget, RequiredBudget) :-
    memberchk(ParentState, [ready, running]),
    integer(ChildCount),
    integer(MaxChildren),
    ChildCount >= 0,
    MaxChildren >= 0,
    ChildCount < MaxChildren,
    integer(RemainingBudget),
    integer(RequiredBudget),
    RemainingBudget >= 0,
    RequiredBudget >= 0,
    RemainingBudget >= RequiredBudget.

valid_parent(_, none).
valid_parent(Id, Parent) :-
    valid_identifier(Parent),
    Parent \== Id.

valid_trace_ref(none).
valid_trace_ref(Ref) :-
    valid_ref(trace, Ref).

valid_identifier_list(Values, Limit) :-
    is_list(Values),
    length(Values, Count),
    Count =< Limit,
    maplist(valid_identifier, Values),
    sort(Values, Sorted),
    length(Sorted, Count).

valid_budget_list(Budgets, Limit) :-
    is_list(Budgets),
    length(Budgets, Count),
    Count =< Limit,
    maplist(valid_budget, Budgets),
    maplist(budget_kind, Budgets, Kinds),
    sort(Kinds, SortedKinds),
    length(SortedKinds, Count).

budget_kind(budget(Kind, _), Kind).

valid_ref(Prefix, Ref) :-
    atom(Prefix),
    atom(Ref),
    atomic_list_concat([Prefix, Token], ':', Ref),
    valid_identifier(Token).

valid_identifier(Id) :-
    atom(Id),
    atom_codes(Id, [First | Rest]),
    lower_code(First),
    length(Rest, TailLength),
    TailLength =< 63,
    maplist(identifier_code, Rest).

identifier_code(Code) :-
    lower_code(Code), !.
identifier_code(Code) :-
    digit_code(Code), !.
identifier_code(0'.).
identifier_code(0'_).
identifier_code(0'-).

lower_code(Code) :-
    Code >= 0'a,
    Code =< 0'z.

digit_code(Code) :-
    Code >= 0'0,
    Code =< 0'9.
