:- module(todo_capture,
    [
        capture_todo/1      % +RawTaskString
    ]).

/*
   TODO Capture Module (GTD Org Entry Writer)
   ------------------------------------------
   Takes a raw task string, infers GTD tag + category,
   asks for a date if missing, then writes a TODO entry
   into the Org file specified by the user KB facts:

       todo_destination(Path).
       todo_context_mode(Mode).

   Depends on:
     kb_todo_context:infer_context/4
     user KB facts: todo_destination/1, todo_context_mode/1
*/

:- use_module(library(readutil)).
:- use_module(library(lists)).
:- use_module(library(date)).
:- use_module('../kb/todo_context').                % infer_context/4
:- use_module('../kb/config', [todo_destination/1,
                                todo_context_mode/1]).

% ----------------------------------------------------------------------
% Public Entry Point
% ----------------------------------------------------------------------

capture_todo(Raw) :-
    format("capture_todo: ~w~n", [Raw]),
    normalize_task(Raw, Tokens, CleanText),
    infer_tag_and_category(Tokens, Tag, Category),
    maybe_ask_for_date(Tokens, DateStamp),
    write_org_entry(CleanText, Tag, Category, DateStamp),
    format('~n📌 A new intention has been noted in your book of tasks.~n~n').

% ----------------------------------------------------------------------
% Normalize Input
% ----------------------------------------------------------------------

normalize_task(Raw, Tokens, CleanText) :-
    format("normalize_task: ~w~n", [Raw]),
    string_lower(Raw, Lwr),
    split_string(Lwr, " ", " ", Parts),
    maplist(string_to_atom, Parts, Tokens0),
    exclude(==(''), Tokens0, Tokens),
    atomic_list_concat(Tokens, ' ', CleanText).

% ----------------------------------------------------------------------
% Context & Tag Inference
% ----------------------------------------------------------------------

% keep this, but make its format undeniably safe
infer_tag_and_category(Tokens, Tag, Category) :-
    todo_context_mode(Mode),
    ( Mode = infer           -> enable_llm_fallback(false)
    ; Mode = infer_with_llm  -> enable_llm_fallback(true)
    ; Mode = llm_only        -> enable_llm_fallback(true)
    ),
    infer_context(Tokens, Tag, Category, Confidence0),
    Confidence is float(Confidence0),
    format("~nContext ~w (~w, confidence ~2f)~n",
           [Tag, Category, Confidence]).


% ----------------------------------------------------------------------
% Ask for Date if Necessary
% ----------------------------------------------------------------------

maybe_ask_for_date(Tokens, DateStamp) :-
    detect_date(Tokens, DateStamp), !.
maybe_ask_for_date(_, DateStamp) :-
    ask_for_date(yes),
    zarathustra_ask_date(DateStamp).
maybe_ask_for_date(_, "").

% Placeholder: extend with NLP date detection later
detect_date(_, _) :- fail.

ask_for_date(yes).

% fix the prompt reader
zarathustra_ask_date(DateStamp) :-
    format('~nYou speak of a task, yet time remains unshaped.~n'),
    format('When shall this be done? (e.g., "tomorrow at 3pm" or "skip")~n> '),
    read_line_to_string(user_input, TimeRaw),  % <- FIXED
    ( TimeRaw = "skip" ->
        DateStamp = ""
    ; parse_date(TimeRaw, DateStamp) ->
        true
    ; format('I could not grasp that moment. Try again.~n'),
      zarathustra_ask_date(DateStamp)
    ).

% Minimal date parser placeholder
parse_date(String, Stamp) :-
    format("parse_date: ~w~n", [String]),
    get_time(Now),
    format_time(string(Stamp), "<%Y-%m-%d %a>", Now).

                                % ----------------------------------------------------------------------
% Org File Writing
% ----------------------------------------------------------------------

write_org_entry(Task, Tag, Category, DateStamp) :-
    todo_destination(Path),
    expand_path(Path, AbsPath),
    get_time(Now),
    stamp_date_time(Now, DT, 'UTC'),
    format_time(string(Created), '[%Y-%m-%d %a %H:%M]', DT),
    build_entry(Task, Tag, Category, Created, DateStamp, Entry),
    append_to_file(AbsPath, Entry).

build_entry(Task, Tag, Category, Created, DateStamp, Entry) :-
    format(string(Header), "* TODO ~w :~w:", [Task, Tag]),
    ( DateStamp == "" ->
        format(string(Props),
":PROPERTIES:
:CREATED:  ~w
:CATEGORY: ~w
:END:
", [Created, Category])
    ;
        format(string(Props),
":PROPERTIES:
:CREATED:  ~w
:CATEGORY: ~w
:END:
SCHEDULED: ~w
", [Created, Category, DateStamp])
    ),
    string_concat(Header, "\n", X),
    string_concat(X, Props, EntryWithNL),
    string_concat(EntryWithNL, "\n", Entry).

append_to_file(File, Text) :-
    setup_call_cleanup(
        open(File, append, Stream),
        write(Stream, Text),
        close(Stream)
    ).

% Expand ~/ → absolute path
expand_path(In, Out) :-
    atom(In), atom_string(In, S), !,
    expand_path(S, Out).
expand_path(S, Out) :-
    sub_string(S, 0, 2, _, "~/"), !,
    sub_string(S, 2, _, 0, Rest),
    expand_file_name(Rest, [Expanded]),
    atom_string(Out, Expanded).
expand_path(S, S).
