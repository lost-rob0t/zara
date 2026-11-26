:- begin_tests(intents).

:- use_module('../modules/intent_resolver').

intent_fixture(passive, "stop", stop, []).
intent_fixture(conversation, "stop", end_conversation, []).
intent_fixture(dictation, "stop", dictation_stop, []).
intent_fixture(passive, "end", end_conversation, []).
intent_fixture(conversation, "end", end_conversation, []).
intent_fixture(dictation, "end", dictation_stop, []).
intent_fixture(dictation, "disable", dictation_stop, []).
intent_fixture(passive, "hello", python(say_hello), []).
intent_fixture(conversation, "hello", python(say_hello), []).
intent_fixture(dictation, "hello", python(say_hello), []).
intent_fixture(passive, "search weather", search, [weather]).
intent_fixture(conversation, "search weather", search, [weather]).
intent_fixture(dictation, "search weather", search, [weather]).
intent_fixture(passive, "search todos milk", python(search_todos), [todos, milk]).
intent_fixture(conversation, "search todos milk", python(search_todos), [todos, milk]).
intent_fixture(dictation, "search todos milk", python(search_todos), [todos, milk]).
intent_fixture(passive, "find tasks", python(search_todos), [tasks]).
intent_fixture(conversation, "find tasks", python(search_todos), [tasks]).
intent_fixture(dictation, "find tasks", python(search_todos), [tasks]).
intent_fixture(passive, "org editor", python(open_org_todos), [editor]).
intent_fixture(conversation, "org editor", python(open_org_todos), [editor]).
intent_fixture(dictation, "org editor", python(open_org_todos), [editor]).
intent_fixture(passive, "agenda 5", python(todo_brief), ['5']).
intent_fixture(conversation, "prioritize todos", python(todo_brief), [todos]).
intent_fixture(passive, "open", pending(open), [app]).
intent_fixture(passive, "text", pending(text), [contact, message]).
intent_fixture(passive, "text alice", pending(text), [message]).
intent_fixture(passive, "schedule", pending(python(schedule_todo)), [task]).
intent_fixture(passive, "set", pending(python(schedule_todo)), [task]).
intent_fixture(passive, "schedule dentist", python(schedule_todo), [dentist]).
intent_fixture(passive, "show todos", python(list_todos), [todos]).
intent_fixture(conversation, "show todos", python(list_todos), [todos]).
intent_fixture(passive, "list tasks", python(list_todos), [tasks]).
intent_fixture(passive, "show", python(list_todos), []).
intent_fixture(passive, "set reminder call mom", python(schedule_todo), [reminder, call, mom]).
intent_fixture(passive, "edit todo milk", python(edit_todo), [todo, milk]).
intent_fixture(passive, "export todos markdown", python(export_todos), [todos, markdown]).

% Issue #363: generic verbs must not claim turns that are not about todos.
% These utterances must resolve to NO deterministic intent so the turn
% falls through to the conversational agent.
nontodo_fixture(conversation, "show off your prolog skills").
nontodo_fixture(conversation, "prolog skills show it").
nontodo_fixture(conversation, "list experts").
nontodo_fixture(passive, "set up bluetooth").
nontodo_fixture(conversation, "edit the prolog file").
nontodo_fixture(passive, "export the report to pdf").

test(stateful_intent_corpus,
     [forall(intent_fixture(State, Input, Intent, Args))]) :-
    intent_resolver:resolve(Input, State, Intent, Args).

test(ambiguous_todo_verbs_fall_through_to_agent,
     [forall(nontodo_fixture(State, Input))]) :-
    \+ intent_resolver:resolve(Input, State, _, _).

emit_intent_corpus :-
    forall(intent_fixture(State, Input, _, _),
           ( intent_resolver:resolve(Input, State, Intent, Args),
             format('~q|~q|~q|~q~n', [State, Input, Intent, Args])
           )).

:- end_tests(intents).
