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
intent_fixture(passive, "open", pending(open), [app]).
intent_fixture(passive, "text", pending(text), [contact, message]).
intent_fixture(passive, "text alice", pending(text), [message]).
intent_fixture(passive, "schedule", pending(python(schedule_todo)), [task]).
intent_fixture(passive, "set", pending(python(schedule_todo)), [task]).
intent_fixture(passive, "schedule dentist", python(schedule_todo), [dentist]).

test(stateful_intent_corpus,
     [forall(intent_fixture(State, Input, Intent, Args))]) :-
    intent_resolver:resolve(Input, State, Intent, Args).

emit_intent_corpus :-
    forall(intent_fixture(State, Input, _, _),
           ( intent_resolver:resolve(Input, State, Intent, Args),
             format('~q|~q|~q|~q~n', [State, Input, Intent, Args])
           )).

:- end_tests(intents).
