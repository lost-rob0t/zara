:- module(kb_intents, [verb_intent/3]).

% Core system intents and synonyms
% verb_intent(Surface, Intent, Arity)

% Greetings
verb_intent(hello, greet, 0).
verb_intent(hi, greet, 0).
verb_intent(hey, greet, 0).
verb_intent(hey_there, greet, 0).
verb_intent(hi_there, greet, 0).
verb_intent(greetings, greet, 0).
verb_intent(good_morning, greet, 0).
verb_intent(good_afternoon, greet, 0).
verb_intent(good_evening, greet, 0).
verb_intent(howdy, greet, 0).
verb_intent(yo, greet, 0).
verb_intent(sup, greet, 0).
verb_intent(what_s_up, greet, 0).
verb_intent(wassup, greet, 0).
verb_intent(hiya, greet, 0).
verb_intent(hello_there, greet, 0).
verb_intent(gday, greet, 0).
verb_intent(good_day, greet, 0).
verb_intent(hola, greet, 0).
verb_intent(hey_zara, greet, 0).
verb_intent(hi_zara, greet, 0).
verb_intent(morning, greet, 0).
verb_intent(evening, greet, 0).
verb_intent(hey_you, greet, 0).
verb_intent(hello_friend, greet, 0).
verb_intent(greetings_human, greet, 0).
verb_intent(salutations, greet, 0).
verb_intent(hail, greet, 0).
verb_intent(welcome, greet, 0).

% Media Control
verb_intent(play, play, 1).
verb_intent(pause, pause, 0).
verb_intent(stop, stop, 0).
verb_intent(resume, resume, 0).
verb_intent(next, next, 0).
verb_intent(skip, skip, 0).

% Communication
% verb_intent(call, call, 1).
% verb_intent(dial, call, 1).
% verb_intent(phone, call, 1).
verb_intent(text, text, 2).
verb_intent(message, text, 2).
verb_intent(sms, text, 2).

% Device Control
verb_intent(open, open, 1).
verb_intent(launch, open, 1).
verb_intent(start, open, 1).
verb_intent(run, open, 1).
verb_intent(lock, lock, 0).
verb_intent(unlock, unlock, 0).
verb_intent(command, open, 0).

% Dictation
% Keep these as 0-arity so “start voice mode”, “enable dictation”, etc. can resolve
% even if extra tokens follow.

% Start
verb_intent(dictate, dictation_start, 0).
verb_intent(dictation, dictation_start, 0).
verb_intent(voice, dictation_start, 0).
verb_intent(voicemode, dictation_start, 0).
verb_intent(voice_mode, dictation_start, 0).
verb_intent(startvoicemode, dictation_start, 0).
verb_intent(start_voice_mode, dictation_start, 0).
verb_intent(mic, dictation_start, 0).
verb_intent(micmode, dictation_start, 0).

% Enable/start synonyms
verb_intent(enable, dictation_start, 0).
verb_intent(begin, dictation_start, 0).
verb_intent(activate, dictation_start, 0).

% Stop/disable synonyms
verb_intent(stopdictation, dictation_stop, 0).
verb_intent(stopvoice, dictation_stop, 0).
verb_intent(disable, dictation_stop, 0).
verb_intent(deactivate, dictation_stop, 0).
verb_intent(end, dictation_stop, 0).
verb_intent(quitdictation, dictation_stop, 0).

% Navigation
verb_intent(search, search, rest).
verb_intent(find, search, rest).
verb_intent(lookup, search, rest).
verb_intent(navigate, navigate, 1).
verb_intent(goto, navigate, 1).

% LLM / Queries
verb_intent(ask, ask, rest).
verb_intent(tell, ask, rest).
verb_intent(explain, ask, rest).

% Fun
verb_intent(say, speak, rest).

% Python skills
verb_intent(hello, python(say_hello), rest).
verb_intent(greet, python(say_hello), rest).

% Conversation stop phrases
verb_intent(bye, end_conversation, 0).
verb_intent(goodbye, end_conversation, 0).
verb_intent(farewell, end_conversation, 0).
verb_intent(see_you, end_conversation, 0).
verb_intent(see_you_later, end_conversation, 0).
verb_intent(end, end_conversation, 0).
verb_intent(quit, end_conversation, 0).
verb_intent(stop, end_conversation, 0).
verb_intent(end_conversation, end_conversation, 0).
verb_intent(stop_conversation, end_conversation, 0).
verb_intent(end_session, end_conversation, 0).
verb_intent(stop_session, end_conversation, 0).

% Philosophy & deep questions
verb_intent(what, ask, rest).
verb_intent(why, ask, rest).
verb_intent(how, ask, rest).
verb_intent(when, ask, rest).
verb_intent(where, ask, rest).
verb_intent(who, ask, rest).

% ----------------------------------------------------------------------
% TODO / Reminder / Scheduling
% ----------------------------------------------------------------------

% “add a todo”, “todo this”
verb_intent(todo, todo, rest).
verb_intent(add, todo, rest).             % "add task", "add a todo"
verb_intent(note, todo, rest).            % "note to self ..."

% “remind me…”
verb_intent(remind, reminder, rest).
verb_intent(remember, reminder, rest).    % "remember to ..."
verb_intent(reminder, reminder, rest).

% “schedule …”
verb_intent(schedule, schedule, rest).
verb_intent(sched, schedule, rest).       % shorthand
verb_intent(plan, schedule, rest).        % "plan to ..."
verb_intent(set, schedule, rest).         % "set a reminder", "set task"
verb_intent(task, schedule, rest).         % "set a reminder", "set task"

% Timers
verb_intent(timer, timer, 2).      % timer Duration Label
verb_intent(alarm, alarm, 2).      % alarm Duration Label
