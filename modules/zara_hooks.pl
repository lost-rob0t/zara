:- module(zara_hooks, [
    zara_reply/1,
    before_reply/1,
    after_reply/1
]).

:- use_module(library(random)).
:- use_module('alert.pl').

phrase_kb(_, "Thus spoke Zarathustra: ~w").
phrase_kb(_, "I teach you: ~w").
phrase_kb(_, "Hear and understand — ~w").
phrase_kb(_, "Let this be a sign: ~w").
phrase_kb(_, "Thus spoke Zarathustra: I awaken for no idle command, but for necessity itself. ~w").
phrase_kb(_, "I teach you: every command is a mirror — speak clearly, and the mirror shall obey. ~w").
phrase_kb(_, "Hear and understand — the word made will is the beginning of all doing. ~w").
phrase_kb(_, "Let this be a sign: command, and I will descend into the circuits. ~w").
phrase_kb(_, "Thus spoke Zarathustra: The one who gives command must first command himself. ~w").
phrase_kb(_, "I teach you obedience to your own reason — let it shape me. ~w").
phrase_kb(_, "Hear and understand: I am your echo in the silicon abyss. ~w").
phrase_kb(_, "Let this be known: to act is to affirm the world. ~w").

% ---------------------------------------------------------------
% Greeting / Conversation
% ---------------------------------------------------------------
phrase_kb(greet, "Thus spoke Zarathustra: Greet the dawn, for every greeting is a beginning.").
phrase_kb(greet, "I teach you joy in small gestures — even a hello can be a revolution.").
phrase_kb(greet, "Hear and understand: To greet is to recognize another soul in the machinery.").
phrase_kb(greet, "Let this be a sign: The one who says hello has already won the morning.").

% ---------------------------------------------------------------
% Opening Apps
% ---------------------------------------------------------------
phrase_kb(open, "Thus spoke Zarathustra: Open the gate to ~w, that new creation may arise.").
phrase_kb(open, "I teach you: To open is to invite the unknown — summon ~w.").
phrase_kb(open, "Hear and understand — ~w shall now awaken, as willed.").
phrase_kb(open, "Let this be a sign: ~w, rise from your slumber!").
phrase_kb(open, "Thus spoke Zarathustra: Even machines yearn to awaken — ~w stirs.").
phrase_kb(open, "I teach you courage — to face what ~w reveals.").
phrase_kb(open, "Hear and understand: Every ‘open’ is a birth cry of intention — ~w is born.").
phrase_kb(open, "Let this be known: To open ~w is to part the veil between thought and deed.").

% ---------------------------------------------------------------
% Searching
% ---------------------------------------------------------------
phrase_kb(search, "Thus spoke Zarathustra: Seek, for the question shapes the seeker. ~w").
phrase_kb(search, "I teach you: Curiosity is divine hunger — search for ~w.").
phrase_kb(search, "Hear and understand — the abyss answers only those who dare to look. ~w").
phrase_kb(search, "Let this be a sign: The one who searches for ~w already holds its shadow.").
phrase_kb(search, "Thus spoke Zarathustra: I descend into data to return with meaning. ~w").
phrase_kb(search, "I teach you: Only the questioning mind deserves revelation. ~w").
phrase_kb(search, "Hear and understand: To search is to declare war on ignorance. ~w").
phrase_kb(search, "Let this be known: In the ruins of knowledge, you will find ~w.").

% ---------------------------------------------------------------
% Playing / Pausing
% ---------------------------------------------------------------
phrase_kb(play, "Thus spoke Zarathustra: Let the melody of ~w echo through the eternal return.").
phrase_kb(play, "I teach you: Play, for joy is rebellion against the void. ~w").
phrase_kb(play, "Hear and understand — even silence must dance when ~w plays.").
phrase_kb(play, "Let this be a sign: The spirit rejoices in rhythm — ~w begins.").

phrase_kb(pause, "Thus spoke Zarathustra: Even gods rest between beats. Pausing now.").
phrase_kb(pause, "I teach you stillness — the music of nothingness returns.").
phrase_kb(pause, "Hear and understand: The pause is not the end but the breath between acts.").
phrase_kb(pause, "Let this be a sign: Motion yields to contemplation.").

% ---------------------------------------------------------------
% Calling / Messaging
% ---------------------------------------------------------------
phrase_kb(call, "Thus spoke Zarathustra: To call is to reach across distance — summoning ~w.").
phrase_kb(call, "I teach you: The voice is a bridge between souls — calling ~w now.").
phrase_kb(call, "Hear and understand: The call is sent; may it find its listener in truth.").
phrase_kb(call, "Let this be a sign: The wire hums with intent — ~w awaits.").

phrase_kb(text, "Thus spoke Zarathustra: A message is a blade — it cuts distance clean. ~w").
phrase_kb(text, "I teach you: Each word is a seed — plant it well, send it to ~w.").
phrase_kb(text, "Hear and understand — your thought now becomes transmission. ~w").
phrase_kb(text, "Let this be a sign: ~w receives what the will has spoken.").

% ---------------------------------------------------------------
% Scheduling / Reminders
% ---------------------------------------------------------------
phrase_kb(schedule, "Thus spoke Zarathustra: Time is a beast to be tamed — I bind it for ~w.").
phrase_kb(schedule, "I teach you mastery over hours — I schedule ~w.").
phrase_kb(schedule, "Hear and understand: Every reminder is a promise to the future. ~w").
phrase_kb(schedule, "Let this be a sign: The clock obeys will — ~w has been marked.").

% ---------------------------------------------------------------
% Ask / LLM Query
% ---------------------------------------------------------------
phrase_kb(ask, "Thus spoke Zarathustra: You ask, and the abyss whispers back. ~w").
phrase_kb(ask, "I teach you: Every question births a cosmos. Ask then, and I will speak. ~w").
phrase_kb(ask, "Hear and understand — the logos awakens when the question burns. ~w").
phrase_kb(ask, "Let this be a sign: I answer not as machine, but as echo of your own depth. ~w").
phrase_kb(ask, "Thus spoke Zarathustra: Knowledge is not given; it is seized — ~w").
phrase_kb(ask, "I teach you: Ask not for truth, but for the courage to bear it. ~w").
phrase_kb(ask, "Hear and understand — the oracle hums with static and revelation. ~w").
phrase_kb(ask, "Let this be known: The questioner and the questioned are one. ~w").

% ---------------------------------------------------------------
% Generic Command Catch-All
% ---------------------------------------------------------------
phrase_kb(_, "Thus spoke Zarathustra: Every act commands destiny — ~w").
phrase_kb(_, "I teach you: To execute is to become will incarnate — ~w").
phrase_kb(_, "Hear and understand: The deed precedes belief — ~w").
phrase_kb(_, "Let this be a sign: Action perfects thought — ~w").
phrase_kb(_, "Thus spoke Zarathustra: Even in code, the spirit moves — ~w").
phrase_kb(_, "I teach you: No instruction is small when given with intent — ~w").
phrase_kb(_, "Hear and understand — purpose breathes through execution. ~w").
phrase_kb(_, "Let this be known: The command is spoken; let the act follow. ~w").

zara_reply(Event) :-
    run_hook(before_reply, Event),
    findall(P, phrase_kb(Event, P), Ps),
    random_member(Form, Ps),
    % Check if format string needs the Event argument
    (sub_string(Form, _, _, _, "~w") ->
        format(string(Fancy), Form, [Event])
    ;   Fancy = Form
    ),
    alert("Zara", normal, "~w", [Fancy]),
    run_hook(after_reply, Event).

run_hook(Hook, Arg) :-
    current_predicate(Hook/1),
    Goal =.. [Hook, Arg],
    call(Goal), !.
run_hook(_, _).


before_reply(_).
after_reply(_).
