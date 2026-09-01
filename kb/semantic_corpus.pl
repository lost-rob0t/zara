% Semantic command corpus (issue #156) - the canonical realistic utterance set.
%
% corpus_case(Id, Utterance, State, Context, ExpectedFrames, Tags)
%
% - Utterance/State feed resolve_frames/4 directly.
% - Context is [] or partial_frame(Frame, MissingSlots) with an open frame
%   from a clarification session (contract docs/intentframe-contract.md).
% - ExpectedFrames is a list of frame/3 terms in the contract's portable
%   subset (compounds + lists only; no dicts, no floats in slot identity).
% - Tags: single quoted atoms describing scope; `known_defect_*` tags mark
%   expectations pinned to a recorded defect that a later slice must flip.
%
% Comparator rules (contract section 3): frames are equivalent iff intent,
% status, and slot set (name -> (value, origin)) are equal. Evidence is not
% part of frame/3 and never compared.

% --- timer -----------------------------------------------------------------

corpus_case(timer_complete,
    "set a timer for 2 minutes called tea", passive, [],
    [frame(intent(ns(device), name('timer.set')),
        [slot(name(duration), value(duration(120)), origin(utterance)),
         slot(name(label), value(text('tea')), origin(utterance))],
        complete)],
    [builtin, timer, positive]).

corpus_case(timer_bare_phrase, "set a timer", passive, [],
    [frame(intent(ns(device), name('timer.set')), [], missing([duration]))],
    [builtin, timer, pending]).

corpus_case(timer_bare_verb, "timer", passive, [],
    [frame(intent(ns(device), name('timer.set')), [], missing([duration]))],
    [builtin, timer, pending]).

corpus_case(timer_zero, "timer for 0 seconds", passive, [],
    [frame(intent(ns(device), name('timer.set')),
        [slot(name(duration), value(duration(0)), origin(utterance))],
        complete)],
    [builtin, timer, boundary]).

corpus_case(timer_negative, "timer -5 minutes", passive, [],
    [frame(intent(ns(device), name('timer.set')),
        [slot(name(duration), value(duration(300)), origin(utterance))],
        complete)],
    [builtin, timer, boundary, known_behavior_sign_stripped]).
% The normalizer strips punctuation including minus signs, so a spoken-style
% negative duration is unreachable at the token layer; the resolver's
% invalid(value(duration), negative) branch is host-level defense and the
% Python mirror validator owns negative rejection (zara/runtime/frames.py).

corpus_case(timer_huge, "timer for 999999 hours", passive, [],
    [frame(intent(ns(device), name('timer.set')),
        [slot(name(duration), value(duration(3599996400)), origin(utterance))],
        complete)],
    [builtin, timer, boundary]).
% resolver accepts any non-negative integer seconds; host policy caps
% lifetime values (documented split; host validator owns the cap).

corpus_case(timer_followup,
    "2 minutes", passive,
    partial_frame(frame(intent(ns(device), name('timer.set')), [], missing([duration])),
        [duration]),
    [frame(intent(ns(device), name('timer.set')),
        [slot(name(duration), value(duration(120)), origin(follow_up))],
        complete)],
    [builtin, timer, dialogue]).

corpus_case(timer_correction,
    "actually 5 minutes", passive,
    partial_frame(
        frame(intent(ns(device), name('timer.set')),
            [slot(name(duration), value(duration(120)), origin(utterance))],
            complete),
        []),
    [frame(intent(ns(device), name('timer.set')),
        [slot(name(duration), value(duration(300)), origin(correction))],
        complete)],
    [builtin, timer, dialogue, correction]).

corpus_case(timer_unparseable_followup,
    "bananas", passive,
    partial_frame(frame(intent(ns(device), name('timer.set')), [], missing([duration])),
        [duration]),
    [frame(intent(ns(device), name('timer.set')), [],
        invalid(value(duration), unparseable))],
    [builtin, timer, dialogue, boundary]).

corpus_case(timer_label_only, "timer 20 minutes pizza", passive, [],
    [frame(intent(ns(device), name('timer.set')),
        [slot(name(duration), value(duration(1200)), origin(utterance)),
         slot(name(label), value(text('pizza')), origin(utterance))],
        complete)],
    [builtin, timer, positive]).

% --- open ------------------------------------------------------------------

corpus_case(open_complete, "open firefox", passive, [],
    [frame(intent(ns(app), name(open)),
        [slot(name(target), value(ref(kind(app_alias), id(firefox))), origin(utterance))],
        complete)],
    [builtin, open, positive]).

corpus_case(open_punctuation, "open Firefox!!", passive, [],
    [frame(intent(ns(app), name(open)),
        [slot(name(target), value(ref(kind(app_alias), id(firefox))), origin(utterance))],
        complete)],
    [builtin, open, boundary, punctuation]).

corpus_case(open_pending, "open", passive, [],
    [frame(intent(ns(app), name(open)), [], missing([target]))],
    [builtin, open, pending]).

% --- text / message --------------------------------------------------------

corpus_case(text_complete, "text alice hello world", passive, [],
    [frame(intent(ns(message), name(send)),
        [slot(name(contact), value(ref(kind(contact), id(alice))), origin(utterance)),
         slot(name(message), value(text('hello world')), origin(utterance))],
        complete)],
    [builtin, text, positive]).

corpus_case(text_pending, "text", passive, [],
    [frame(intent(ns(message), name(send)), [], missing([contact, message]))],
    [builtin, text, pending]).

corpus_case(text_partial, "text sarah", passive, [],
    [frame(intent(ns(message), name(send)),
        [slot(name(contact), value(ref(kind(contact), id(sarah))), origin(utterance))],
        missing([message]))],
    [builtin, text, pending]).

corpus_case(text_quoted_command_words, "text alice open firefox now", passive, [],
    [frame(intent(ns(message), name(send)),
        [slot(name(contact), value(ref(kind(contact), id(alice))), origin(utterance)),
         slot(name(message), value(text('open firefox')), origin(utterance))],
        complete)],
    [builtin, text, boundary, injection]).
% Slot text containing command words stays inert text; 'now' is a normalizer
% filler and is stripped (deterministic current behavior).
% 'now' is a registered filler (normalizer.pl), so it is stripped from the
% message text; command-looking words remain inert text/1 slot content.

% --- search / skill --------------------------------------------------------

corpus_case(search_complete, "search prolog dictionaries", passive, [],
    [frame(intent(ns(web), name(search)),
        [slot(name(query), value(text('prolog dictionaries')), origin(utterance))],
        complete)],
    [builtin, search, positive]).

corpus_case(search_pending, "search", passive, [],
    [frame(intent(ns(web), name(search)), [], missing([query]))],
    [builtin, search, pending]).

corpus_case(search_todos, "search todos milk", passive, [],
    [frame(intent(ns(skill), name(search_todos)),
        [slot(name(query), value(text('milk')), origin(utterance))],
        complete)],
    [builtin, skill, positive]).

corpus_case(schedule_complete, "schedule buy milk", passive, [],
    [frame(intent(ns(skill), name(schedule_todo)),
        [slot(name(task), value(text('buy milk')), origin(utterance))],
        complete)],
    [builtin, skill, positive]).

corpus_case(schedule_pending, "schedule", passive, [],
    [frame(intent(ns(skill), name(schedule_todo)), [], missing([task]))],
    [builtin, skill, pending]).

% --- device ----------------------------------------------------------------

corpus_case(screenshot, "take a screenshot", passive, [],
    [frame(intent(ns(device), name('screen.capture')), [], complete)],
    [builtin, device, positive]).

corpus_case(alarm_complete, "alarm 7 am", passive, [],
    [frame(intent(ns(device), name(alarm)),
        [slot(name(time), value(text('7 am')), origin(utterance))],
        complete)],
    [builtin, device, positive]).

corpus_case(alarm_pending, "alarm", passive, [],
    [frame(intent(ns(device), name(alarm)), [], missing([time]))],
    [builtin, device, pending]).

corpus_case(play_pending, "play", passive, [],
    [frame(intent(ns(media), name(play)), [], missing([target]))],
    [builtin, media, pending]).

corpus_case(play_complete, "play some music", passive, [],
    [frame(intent(ns(media), name(play)),
        [slot(name(target), value(ref(kind(media_alias), id('some music'))), origin(utterance))],
        complete)],
    [builtin, media, positive]).

% --- precedence / conversation ---------------------------------------------

corpus_case(stop_precedence, "stop", passive, [],
    [frame(intent(ns(conversation), name(end)), [], complete)],
    [builtin, conversation, precedence]).

corpus_case(end_conversation, "end", conversation, [],
    [frame(intent(ns(conversation), name(end)), [], complete)],
    [builtin, conversation, precedence]).

corpus_case(dictation_stop, "stop", dictation, [],
    [frame(intent(ns(dictation), name(stop)), [], complete)],
    [builtin, dictation, precedence]).

corpus_case(cancel_open_frame, "never mind", passive, [],
    [frame(intent(ns(conversation), name(cancel)), [], complete)],
    [builtin, conversation, cancel]).

corpus_case(ask_bare, "what", passive, [],
    [frame(intent(ns(conversation), name(ask)), [], missing([query]))],
    [builtin, conversation, pending]).

corpus_case(ask_question, "what is the weather", passive, [],
    [frame(intent(ns(conversation), name(ask)),
        [slot(name(query), value(text('is weather')), origin(utterance))],
        complete)],
    [builtin, conversation, positive]).
% The query text is subject to the current filler-stripping normalizer;
% deterministic but lossy ('the' removed) until the Unicode-preserving
% normalizer lands (#122/#172). Pinned as current behavior.

% --- empty / unknown --------------------------------------------------------

corpus_case(empty_input, "", passive, [], [], [boundary, empty]).

corpus_case(whitespace_input, "   ", passive, [], [], [boundary, empty]).

corpus_case(no_verb_falls_through, "the meaning of life", passive, [],
    [], [boundary, conversational]).

corpus_case(ambiguity_choice, "5 minutes", passive,
    partial_frame(
        frame(intent(ns(device), name('timer.set')), [],
            ambiguous(['20 minutes', '5 minutes'])),
        [duration]),
    [frame(intent(ns(device), name('timer.set')),
        [slot(name(duration), value(duration(300)), origin(follow_up))],
        complete)],
    [builtin, timer, dialogue, ambiguous]).

corpus_case(ambiguity_unresolved, "bananas", passive,
    partial_frame(
        frame(intent(ns(device), name('timer.set')), [],
            ambiguous(['20 minutes', '5 minutes'])),
        [duration]),
    [frame(intent(ns(device), name('timer.set')), [],
        ambiguous(['20 minutes', '5 minutes']))],
    [builtin, timer, dialogue, ambiguous]).

corpus_case(set_a_timer_not_schedule, "set a timer", passive, [],
    [frame(intent(ns(device), name('timer.set')), [], missing([duration]))],
    [builtin, timer, precedence]).

% --- unicode ----------------------------------------------------------------

corpus_case(unicode_ascii_normalizer_defect, "text alice café", passive, [],
    [frame(intent(ns(message), name(send)),
        [slot(name(contact), value(ref(kind(contact), id(alice))), origin(utterance)),
         slot(name(message), value(text('caf')), origin(utterance))],
        complete)],
    [builtin, text, boundary, unicode,
     known_defect_ascii_normalizer]).
% Current normalizer deletes non-ASCII characters (normalizer.pl:252-257,
% recorded defect in rage/154-intentframe-research.org). The expected
% frame below pins the CURRENT behavior so drift is detected; the
% Unicode-preserving normalizer (#122/#172) must flip this case.
