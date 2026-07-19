:- module(zara_hooks, [
    acknowledge/0,
    reply_result/1,
    reply_text/2,
    reply_phrases/3,
    before_reply/1,
    after_reply/1
]).

:- use_module(library(process)).
:- use_module(library(random)).
:- use_module(library(time)).
:- use_module('alert.pl').

concise_phrase(success, greet, "Hello.").
concise_phrase(success, open, "Opened ~w.").
concise_phrase(success, search, "Searching for ~w.").
concise_phrase(success, text, "Sent the message to ~w.").
concise_phrase(success, call, "Started a call with ~w.").
concise_phrase(success, play, "Playing ~w.").
concise_phrase(success, pause, "Paused playback.").
concise_phrase(success, timer, "Set the timer for ~w.").
concise_phrase(success, dictation_start, "Started dictation.").
concise_phrase(success, dictation_stop, "Stopped dictation.").

concise_phrase(failure, open, "I couldn't open ~w.").
concise_phrase(failure, search, "I couldn't search for ~w.").
concise_phrase(failure, text, "I couldn't send the message to ~w.").
concise_phrase(failure, call, "I couldn't call ~w.").
concise_phrase(failure, play, "I couldn't play ~w.").
concise_phrase(failure, timer, "I couldn't set the timer for ~w.").
concise_phrase(failure, dictation_start, "I couldn't start dictation.").
concise_phrase(failure, dictation_stop, "I couldn't stop dictation.").
concise_phrase(failure, command_resolution, "I couldn't understand ~w.").

rich_phrase(success, greet,
            "Greet the dawn, for every greeting is a beginning.").
rich_phrase(success, open,
            "The gate to ~w is open; let new creation arise.").
rich_phrase(success, search,
            "Seek, for the question shapes the seeker: ~w.").
rich_phrase(success, text,
            "The word has crossed the distance to ~w.").
rich_phrase(success, call,
            "The voice now reaches across the distance to ~w.").
rich_phrase(success, play,
            "Let ~w answer the silence.").
rich_phrase(success, pause,
            "The pause is the breath between acts.").

generic_phrase(success, "Completed ~w.").
generic_phrase(failure, "I couldn't complete ~w.").

acknowledge :-
    reply_text(acknowledgement, Text),
    emit_reply(acknowledgement, Text).

reply_result(Result) :-
    reply_text(Result, Text),
    emit_reply(Result, Text).

reply_text(command_result(Status, Intent, Args, _Detail), Text) :-
    reply_phrases(Status, Intent, Phrases),
    random_member(Template, Phrases),
    reply_subject(Intent, Args, Subject),
    render_phrase(Template, Subject, Text).
reply_text(acknowledgement, "Okay.").

reply_phrases(Status, Intent, Phrases) :-
    findall(Phrase, concise_phrase(Status, Intent, Phrase), Concise),
    specific_phrases(Status, Intent, Concise, Phrases).

specific_phrases(Status, _Intent, [], Phrases) :-
    !,
    findall(Phrase, generic_phrase(Status, Phrase), Phrases),
    Phrases \= [].
specific_phrases(success, Intent, Concise, Phrases) :-
    rich_replies_enabled,
    !,
    findall(Phrase, rich_phrase(success, Intent, Phrase), Rich),
    append(Concise, Rich, Phrases).
specific_phrases(_, _, Phrases, Phrases).

rich_replies_enabled :-
    getenv('ZARA_RICH_REPLIES', Value),
    downcase_atom(Value, Normalized),
    memberchk(Normalized, ['1', true, yes, on]).

reply_subject(open, [App|_], Subject) :- !,
    term_string(App, Subject, [quoted(false)]).
reply_subject(search, Args, Subject) :- !,
    args_text(Args, Subject).
reply_subject(text, [Contact|_], Subject) :- !,
    term_string(Contact, Subject, [quoted(false)]).
reply_subject(call, [Contact|_], Subject) :- !,
    term_string(Contact, Subject, [quoted(false)]).
reply_subject(play, Args, Subject) :- !,
    args_text(Args, Subject).
reply_subject(timer, [_Seconds, Name], Subject) :-
    Name \== '', !,
    term_string(Name, Subject, [quoted(false)]).
reply_subject(timer, [Seconds|_], Subject) :- !,
    format(string(Subject), '~w seconds', [Seconds]).
reply_subject(Intent, [], Subject) :- !,
    term_string(Intent, Subject, [quoted(false)]).
reply_subject(_, Args, Subject) :-
    args_text(Args, Subject).

args_text(Args, Text) :-
    maplist(argument_text, Args, Parts),
    atomics_to_string(Parts, " ", Text).

argument_text(Argument, Text) :-
    term_string(Argument, Text, [quoted(false)]).

render_phrase(Template, Subject, Text) :-
    ( sub_string(Template, _, _, _, "~w")
    -> format(string(Text), Template, [Subject])
    ;  Text = Template
    ).

emit_reply(Event, Text) :-
    run_hook(before_reply, Event),
    deliver_safely(alert:alert("Zara", normal, "~w", [Text])),
    deliver_safely(speak_reply(Text)),
    run_hook(after_reply, Event).

deliver_safely(Goal) :-
    catch((call(Goal) -> true ; true), Error, print_message(warning, Error)).

speak_reply(Text) :-
    getenv('ZARA_REPLY_TTS_COMMAND', Command),
    Command \== '',
    !,
    process_create(path(Command), ['--', Text], [process(Process)]),
    bounded_speech_wait(Process, Status),
    Status == exit(0).
speak_reply(_).

bounded_speech_wait(Process, Status) :-
    catch(
        call_with_time_limit(0.5, process_wait(Process, Status)),
        time_limit_exceeded,
        ( terminate_speech_process(Process),
          Status = timeout
        )
    ).

terminate_speech_process(Process) :-
    catch(process_kill(Process, kill), _, true),
    catch(process_wait(Process, _), _, true).

run_hook(Hook, Event) :-
    Goal =.. [Hook, Event],
    catch(call(Goal), _, true),
    !.
run_hook(_, _).

before_reply(_).
after_reply(_).
