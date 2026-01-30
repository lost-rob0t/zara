:- module(intent_resolver, [
    resolve/3,
    canonicalize_tokens/2,
    convert_number_atoms/2
]).
% todo_capture deprecated; Python skills handle todo capture.
:- use_module('../modules/normalizer', [strip_fillers/2]).
:- use_module(library(lists)).
:- use_module(normalizer).
:- use_module('../kb/intents').
:- use_module('object_intents').
:- use_module('pending_question').

arg_spec(open, [target]).
arg_spec(play, [media]).
arg_spec(search, [query]).
arg_spec(navigate, [destination]).
arg_spec(text, [contact, message]).
arg_spec(timer, [duration]).
arg_spec(alarm, [duration]).
arg_spec(capture_todo, [item]).
arg_spec(schedule_todo, [item, schedule_time]).

%% ============================================================
%% PUBLIC API
%% ============================================================

    resolve(Raw, Intent, Args) :-
    normalizer:normalize_string(Raw, Toks0),
    strip_fillers(Toks0, Core0),
    canonicalize_tokens(Toks0, Toks),
    try_exact(Toks, Intent0, Args0),
    object_intents:refine_intent(Intent0, Toks, Intent1),
    maybe_request_args(Intent1, Args0, Intent, Args),
    !.

canonicalize_tokens(Toks0, Toks) :-
    strip_fillers(Toks0, Core0),
    ( select_verb_head(Core0, Verb, Rest)
    -> ( member(Verb, [why, what, how, when, where, who]) ->
        % For question words, keep them in args too
        Toks = [Verb|Rest]
    ; Toks = [Verb|Rest]
    )
    ; Toks = Core0
    ).


%% ============================================================
%% VERB MATCHING
%% ============================================================

try_exact([Word|Rest], Intent, Args) :-
    ( kb_intents:verb_intent(Word, Intent, Arity) ->
        ( Arity = rest, member(Word, [why, what, how, when, where, who]) ->
            % For question words, include them in the args
            Args = [Word|Rest]
        ; extract_args(Arity, Rest, Intent, Args)
        )
    ; fail ).



%% ============================================================
%% VERB SELECTION
%% ============================================================

select_verb_head([W|Rs], W, Rs) :-
    kb_intents:verb_intent(W, _, _), !.
select_verb_head([_|Rs], Verb, Tail) :-
    select_verb_head(Rs, Verb, Tail).


%%============================================================
%%integer Extraction
%%============================================================
convert_number_atoms([], []).

convert_number_atoms([Tok|Rest], [Num|ParsedRest]) :-
    atom(Tok),
    atom_number(Tok, Num),
    convert_number_atoms(Rest, ParsedRest).

convert_number_atoms([Tok|Rest], [Tok|ParsedRest]) :-
    convert_number_atoms(Rest, ParsedRest).


%=============================================================
% Units
%=============================================================
/* normalize_unit(+Raw, -Unit)
   Converts different spellings of units to a canonical form.
*/

normalize_unit(minutes, minutes).
normalize_unit(minute, minutes).
normalize_unit(min, minutes).

normalize_unit(seconds, seconds).
normalize_unit(second, seconds).
normalize_unit(sec, seconds).
normalize_unit(s, seconds).

normalize_unit(hours, hours).
normalize_unit(hour, hours).
normalize_unit(hr, hours).
normalize_unit(h, hours).

%% ============================================================
%% ARGUMENT EXTRACTION
%% ============================================================

maybe_request_args(open, Args, ask, [open_target]) :-
    missing_open_target(Args),
    pending_question:set_pending_question(open, [target], "What should I open?", Args, _).
maybe_request_args(play, Args, ask, [play_target]) :-
    missing_generic_arg(Args),
    pending_question:set_pending_question(play, [media], "What should I play?", Args, _).
maybe_request_args(search, Args, ask, [search_query]) :-
    missing_generic_arg(Args),
    pending_question:set_pending_question(search, [query], "What should I search for?", Args, _).
maybe_request_args(navigate, Args, ask, [navigate_target]) :-
    missing_generic_arg(Args),
    pending_question:set_pending_question(navigate, [destination], "Where should I navigate?", Args, _).
maybe_request_args(text, Args, ask, [text_message]) :-
    missing_text_args(Args, MissingSlots),
    MissingSlots \= [],
    text_prompt(MissingSlots, Prompt),
    pending_question:set_pending_question(text, MissingSlots, Prompt, Args, _).
maybe_request_args(timer, Args, ask, [timer_duration]) :-
    missing_timer_args(Args),
    pending_question:set_pending_question(timer, [duration], "How long should the timer be?", Args, _).
maybe_request_args(alarm, Args, ask, [alarm_duration]) :-
    missing_timer_args(Args),
    pending_question:set_pending_question(alarm, [duration], "When should I set the alarm for?", Args, _).
maybe_request_args(python(capture_todo), Args, ask, [todo_item]) :-
    missing_generic_arg(Args),
    pending_question:set_pending_question(capture_todo, [item], "What should I add to your todos?", Args, _).
maybe_request_args(python(schedule_todo), Args, ask, [schedule_item]) :-
    schedule_missing_slots(Args, MissingSlots),
    MissingSlots \= [],
    schedule_prompt(MissingSlots, Prompt),
    pending_question:set_pending_question(schedule_todo, MissingSlots, Prompt, Args, _).
maybe_request_args(Intent, Args, Intent, Args).


schedule_missing_slots(Args, MissingSlots) :-
    schedule_args_split(Args, TodoTokens, TimeTokens),
    missing_slots_for(TodoTokens, TimeTokens, MissingSlots).

missing_slots_for([], [], [item, schedule_time]).
missing_slots_for([], [_|_], [item]).
missing_slots_for([_|_], [], [schedule_time]).
missing_slots_for([_|_], [_|_], []).

missing_open_target(Args) :-
    missing_generic_arg(Args).

missing_generic_arg(Args) :-
    Args = []
    ; Args = [''].

missing_text_args([], [contact, message]).
missing_text_args([''], [contact, message]).
missing_text_args([_|[]], [message]).
missing_text_args([_, ''|_], [message]).
missing_text_args([_|_], []).

missing_timer_args(Args) :-
    missing_generic_arg(Args).

text_prompt([contact, message], "Who should I text, and what should I say?").
text_prompt([message], "What should I say in the text?").

schedule_prompt([item, schedule_time], "Which todo should I schedule, and for when?").
schedule_prompt([item], "Which todo should I schedule?").
schedule_prompt([schedule_time], "When should I schedule it?").

schedule_args_split([], [], []).
schedule_args_split(Args, TodoTokens, TimeTokens) :-
    append(TodoTokens, [to|TimeTokens], Args),
    TodoTokens \= [],
    TimeTokens \= [],
    !.
schedule_args_split(Args, TodoTokens, TimeTokens) :-
    append(TodoTokens, [for|TimeTokens], Args),
    TodoTokens \= [],
    TimeTokens \= [],
    !.
schedule_args_split(Args, Args, []).


extract_args(0, _, _, []).
extract_args(1, Rest, Intent, [Arg]) :-
    arg1(Intent, Rest, Arg), !.
extract_args(2, Rest, Intent, [A,B]) :-
    arg2(Intent, Rest, A, B), !.
extract_args(rest, Rest, Intent, Args) :-
    Args = Rest.
extract_args(_, Rest, _Intent, Rest).

arg1(play, Rest, Media) :-
    drop_preps([of,for,to,me,some], Rest, R1),
    head_atom(R1, Media).
arg1(call, Rest, Contact) :-
    drop_preps([to], Rest, R),
    head_atom(R, Contact).
arg1(open, Rest, App) :-
    head_atom(Rest, App).
arg1(resume, _, _) :-
    fail.
arg1(_, Rest, X) :-
    head_atom(Rest, X).
% Timer
arg1(timer, Rest, Duration) :-
    extract_number(Rest, Duration), !.
arg1(alarm, Rest, Duration) :-
    extract_number(Rest, Duration), !.

arg2(text, [Contact|MsgParts], Contact, Message) :-
    atomic_list_concat(MsgParts, ' ', Message), !.
arg2(_, Rest, A, B) :-
    Rest = [A|Tail],
    head_atom(Tail, B).

%% ============================================================
%% UTILITY PREDICATES
%% ============================================================

head_atom([], '').
head_atom([H|_], H).

drop_preps([], Rest, Rest).
drop_preps([Prep|Preps], [H|T], Rest) :-
    (H = Prep -> drop_preps(Preps, T, Rest) ; Rest = [H|T]).
drop_preps(Preps, [], []) :-
    Preps = [_|_].


