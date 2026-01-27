:- module(kb_intents, [verb_intent/3]).

% Core system intents and synonyms
% verb_intent(Surface, Intent, Arity)

% Greetings
verb_intent("hello", greet, 0).
verb_intent("hi", greet, 0).
verb_intent("hey", greet, 0).
verb_intent("greetings", greet, 0).
verb_intent("howdy", greet, 0).
verb_intent("ahoy", greet, 0).

% Media Control
verb_intent("play", play, 1).
verb_intent("pause", pause, 0).
verb_intent("stop", stop, 0).
verb_intent("resume", resume, 0).
verb_intent("next", next, 0).
verb_intent("skip", skip, 0).

% Communication
verb_intent("call", call, 1).
verb_intent("dial", call, 1).
verb_intent("phone", call, 1).
verb_intent("text", text, 2).
verb_intent("message", text, 2).
verb_intent("sms", text, 2).

% Device Control
verb_intent("open", open, 1).
verb_intent("launch", open, 1).
verb_intent("start", open, 1).
verb_intent("lock", lock, 0).
verb_intent("unlock", unlock, 0).

% Navigation
verb_intent("search", search, 1).
verb_intent("find", search, 1).
verb_intent("lookup", search, 1).
verb_intent("navigate", navigate, 1).
verb_intent("goto", navigate, 1).

% LLM / Queries
verb_intent("ask", ask, rest).
verb_intent("tell", ask, rest).
verb_intent("explain", ask, rest).

% Fun
verb_intent("say", speak, rest).

% Philosophy & deep questions
verb_intent("what", ask, rest).
verb_intent("why", ask, rest).
verb_intent("how", ask, rest).
verb_intent("when", ask, rest).
verb_intent("where", ask, rest).
verb_intent("who", ask, rest).
