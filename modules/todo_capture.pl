:- module(todo_capture,
    [
        capture_todo/1,          % capture_todo("do thing")
        capture_todo/2           % capture_todo("do thing", ask_date(yes|no))
    ]).

:- use_module(library(error)).
:- use_module(library(readutil)).
:- use_module(library(filesex)).
:- use_module(library(pcre)).
:- use_module(library(date)).
:- use_module(library(apply)).     % foldl/4
:- use_module('../modules/normalizer', [normalize_string/2, strip_fillers/2]).
:- use_module('../kb/todo_context', [infer_context/4, enable_llm_fallback/1]).
:- use_module('../kb/config', [
    todo_destination/1,
    todo_destination_md/1,
    todo_context_mode/1,
    todo_format/1,
    todo_template/2
]).

%% ----------------------------------------------------------------------
%% Public API
%% ----------------------------------------------------------------------

capture_todo(Raw) :-
    capture_todo(Raw, ask_date(no)).

capture_todo(Raw, ask_date(Ask)) :-
    must_be(string, Raw),
    must_be(oneof([yes,no]), Ask),

    normalize_task(Raw, Tokens, CleanTask),
    infer_tag_and_category(Tokens, Tag, Category, Confidence),
    maybe_get_due(Ask, Raw, DueIso, DueOrg, DueLine, DueSuffix),

    now_created_stamp(Created),

    resolve_format(Format),
    resolve_destination(Format, DestPath0),
    resolve_template(Format, Template0),

    format(atom(TagAtom), "~w", [Tag]),
    string_upper(Category, CategoryUC),

    Pairs =
        [ "{task}"-CleanTask
        , "{tag}"-TagAtom
        , "{category}"-CategoryUC
        , "{created}"-Created
        , "{scheduled}"-DueIso
        , "{scheduled_org}"-DueOrg
        , "{scheduled_line}"-DueLine
        , "{due_suffix}"-DueSuffix
        , "{cursor}"-"%%"
        ],

    template_apply(Template0, Pairs, Entry),
    append_text(DestPath0, Entry),

    format("~nTODO captured (~w / ~w, confidence ~2f) -> ~w~n~n",
           [Tag, CategoryUC, Confidence, DestPath0]).

%% ----------------------------------------------------------------------
%% Normalization
%% ----------------------------------------------------------------------

normalize_task(Raw, Tokens, CleanTask) :-
    normalize_string(Raw, T0),
    strip_fillers(T0, Tokens),
    ( Tokens == []
    -> CleanTask = "unspecified task"
    ; atomic_list_concat(Tokens, ' ', AtomTask),
      atom_string(AtomTask, CleanTask)
    ).

%% ----------------------------------------------------------------------
%% Context inference (with sane confidence)
%% ----------------------------------------------------------------------

infer_tag_and_category(Tokens, Tag, Category, Confidence) :-
    todo_context_mode(Mode),
    ( Mode == infer          -> enable_llm_fallback(false)
    ; Mode == infer_with_llm -> enable_llm_fallback(true)
    ; Mode == llm_only       -> enable_llm_fallback(true)
    ;                         enable_llm_fallback(false)
    ),
    infer_context(Tokens, Tag, Category0, Confidence0),
    ( string(Category0) -> Category = Category0
    ; atom_string(Category0, Category)
    ),
    Confidence is float(Confidence0).

%% ----------------------------------------------------------------------
%% Due parsing / prompting
%% ----------------------------------------------------------------------

maybe_get_due(_Ask, Raw, DueIso, DueOrg, DueLine, DueSuffix) :-
    due_from_text(Raw, DueIso0, DueOrg0),
    !,
    due_line_and_suffix(DueIso0, DueOrg0, DueIso, DueOrg, DueLine, DueSuffix).
maybe_get_due(yes, _Raw, DueIso, DueOrg, DueLine, DueSuffix) :-
    prompt_for_due(DueIso0, DueOrg0),
    due_line_and_suffix(DueIso0, DueOrg0, DueIso, DueOrg, DueLine, DueSuffix).
maybe_get_due(no, _Raw, "", "", "", "").

prompt_for_due(DueIso, DueOrg) :-
    format("When is this due? Examples: \"tomorrow at 3pm\", \"2026-01-30 15:00\", \"in 20 minutes\", or \"skip\"~n> "),
    read_line_to_string(user_input, In0),
    normalize_string(In0, In1),
    string_lower(In1, In),
    ( In == ""
    -> DueIso = "", DueOrg = ""
    ; In == "skip"
    -> DueIso = "", DueOrg = ""
    ; parse_due_string(In1, DueIso, DueOrg)
    -> true
    ; format("Couldn't parse due date. Type again, or \"skip\".~n"),
      prompt_for_due(DueIso, DueOrg)
    ).

due_from_text(Raw, DueIso, DueOrg) :-
    must_be(string, Raw),
    normalize_string(Raw, R0),
    string_lower(R0, R),
    parse_due_string(R, DueIso, DueOrg).

due_line_and_suffix("", "", "", "", "", "") :- !.
due_line_and_suffix(DueIso0, DueOrg0, DueIso, DueOrg, DueLine, DueSuffix) :-
    must_be(string, DueIso0),
    must_be(string, DueOrg0),
    DueIso = DueIso0,
    DueOrg = DueOrg0,
    format(string(DueLine), "SCHEDULED: %s~n", [DueOrg]),
    format(string(DueSuffix), " (due: %s)", [DueIso]).

%% ----------------------------------------------------------------------
%% Due parsing core
%% ----------------------------------------------------------------------

parse_due_string(In, DueIso, DueOrg) :-
    must_be(string, In),
    ( parse_due_iso(In, Stamp)
    ; parse_due_relative(In, Stamp)
    ; parse_due_today_tomorrow(In, Stamp)
    ),
    stamp_to_due_strings(Stamp, DueIso, DueOrg).

parse_due_iso(In, Stamp) :-
    % YYYY-MM-DD [HH:MM]
    re_matchsub(
        "([0-9]{4})-([0-9]{2})-([0-9]{2})(?:[ T]([0-9]{1,2}):([0-9]{2}))?",
        In,
        Sub,
        [caseless(true)]
    ),
    atom_number(Sub.1, Y),
    atom_number(Sub.2, M),
    atom_number(Sub.3, D),
    ( get_dict(4, Sub, H0) -> atom_number(H0, H) ; H = 9 ),
    ( get_dict(5, Sub, Min0) -> atom_number(Min0, Min) ; Min = 0 ),
    must_be(between(0, 23), H),
    must_be(between(0, 59), Min),
    now_offset(Off),
    date_time_stamp(date(Y,M,D,H,Min,0,Off,-,-), Stamp).

parse_due_relative(In, Stamp) :-
    % in N minutes|hours|days|weeks
    re_matchsub("^in\\s+([0-9]+)\\s+(minutes?|mins?|hours?|hrs?|days?|weeks?)\\b", In, Sub, [caseless(true)]),
    atom_number(Sub.1, N),
    atom_string(Sub.2, Unit0),
    string_lower(Unit0, Unit),
    unit_seconds(Unit, N, Sec),
    get_time(Now),
    Stamp is Now + Sec.

parse_due_today_tomorrow(In, Stamp) :-
    ( sub_string(In, _, _, _, "tomorrow")
    -> DayDelta = 1
    ; sub_string(In, _, _, _, "today")
    -> DayDelta = 0
    ),
    ( parse_time_in_string(In, H, Min)
    -> true
    ; H = 9, Min = 0
    ),
    local_day_start_stamp(TodayStart),
    Stamp is TodayStart + DayDelta*86400 + H*3600 + Min*60.

parse_time_in_string(In, H, Min) :-
    % supports: "at 3pm", "3 pm", "15:00", "3:30pm"
    ( re_matchsub("(?:\\bat\\s+)?([0-9]{1,2})(?::([0-9]{2}))?\\s*(am|pm)\\b", In, Sub, [caseless(true)])
    -> atom_number(Sub.1, H0),
       ( get_dict(2, Sub, Min0) -> atom_number(Min0, Min1) ; Min1 = 0 ),
       atom_string(Sub.3, AmPm0),
       string_lower(AmPm0, AmPm),
       hour_ampm_to_24(H0, AmPm, H1),
       H = H1,
       Min = Min1
    ; re_matchsub("\\b([0-9]{1,2}):([0-9]{2})\\b", In, Sub2, [])
    -> atom_number(Sub2.1, H),
       atom_number(Sub2.2, Min)
    ),
    must_be(between(0, 23), H),
    must_be(between(0, 59), Min).

hour_ampm_to_24(H0, "am", H) :-
    must_be(between(1, 12), H0),
    ( H0 =:= 12 -> H = 0 ; H = H0 ).
hour_ampm_to_24(H0, "pm", H) :-
    must_be(between(1, 12), H0),
    ( H0 =:= 12 -> H = 12 ; H is H0 + 12 ).

unit_seconds(Unit, N, Sec) :-
    must_be(integer, N),
    N >= 0,
    ( sub_string(Unit, _, _, _, "min") ->
        Sec is N * 60
    ; ( sub_string(Unit, _, _, _, "hour")
      ; sub_string(Unit, _, _, _, "hr")
      ) ->
        Sec is N * 3600
    ; sub_string(Unit, _, _, _, "day") ->
        Sec is N * 86400
    ; sub_string(Unit, _, _, _, "week") ->
        Sec is N * 7 * 86400
    ; domain_error(time_unit, Unit)
    ).

stamp_to_due_strings(Stamp, DueIso, DueOrg) :-
    must_be(number, Stamp),
    format_time(string(DueIso), "%Y-%m-%d %H:%M", Stamp),
    format_time(string(OrgInner), "%Y-%m-%d %a %H:%M", Stamp),
    format(string(DueOrg), "<%s>", [OrgInner]).

now_offset(Off) :-
    get_time(Now),
    stamp_date_time(Now, DT, local),
    date_time_value(utc_offset, DT, Off).

local_day_start_stamp(Stamp) :-
    get_time(Now),
    stamp_date_time(Now, date(Y,M,D,_,_,_,Off,_,_), local),
    date_time_stamp(date(Y,M,D,0,0,0,Off,-,-), Stamp).

%% ----------------------------------------------------------------------
%% Created stamp
%% ----------------------------------------------------------------------

now_created_stamp(Created) :-
    get_time(Now),
    format_time(string(Created), "%Y-%m-%d %H:%M", Now).

%% ----------------------------------------------------------------------
%% Format / destination / template resolution
%% ----------------------------------------------------------------------

resolve_format(Format) :-
    ( todo_format(F0) -> true ; F0 = org ),
    ( F0 == org ; F0 == markdown ),
    Format = F0.

resolve_destination(org, Path) :-
    todo_destination(P0),
    expand_user_path(P0, Path).
resolve_destination(markdown, Path) :-
    ( todo_destination_md(P0) -> true
    ; todo_destination(P0)
    ),
    expand_user_path(P0, Path).

resolve_template(Format, Template) :-
    ( todo_template(Format, T0)
    -> to_string(T0, Template)
    ; domain_error(todo_template_defined_for, Format)
    ).

%% ----------------------------------------------------------------------
%% Template / IO helpers
%% ----------------------------------------------------------------------

template_apply(Template0, Pairs, Out) :-
    to_string(Template0, T1),
    must_be(list, Pairs),
    foldl(apply_one_pair, Pairs, T1, Out).

apply_one_pair(Key-Val, In, Out) :-
    must_be(ground, Key),
    to_string(Key, K0),
    to_string(Val, V0),
    re_replace(K0/g, V0, In, Out).

append_text(Path0, Text0) :-
    to_string(Path0, Path1),
    to_string(Text0, Text1),
    expand_user_path(Path1, Path),
    ensure_parent_dir(Path),
    setup_call_cleanup(
        open(Path, append, S, [encoding(utf8)]),
        format(S, "~s", [Text1]),
        close(S)
    ).

ensure_parent_dir(Path) :-
    file_directory_name(Path, Dir),
    ( exists_directory(Dir) -> true
    ; make_directory_path(Dir)
    ).

expand_user_path(In0, Out) :-
    to_string(In0, In),
    ( sub_string(In, 0, 2, _, "~/")
    -> getenv("HOME", Home),
       sub_string(In, 2, _, 0, Rest),
       format(string(Out), "~w/~w", [Home, Rest])
    ; Out = In
    ).

to_string(X, S) :-
    ( string(X) -> S = X
    ; atom(X)   -> atom_string(X, S)
    ; is_list(X)-> string_codes(S, X)
    ; number(X) -> format(string(S), "~w", [X])
    ;              format(string(S), "~w", [X])
    ).
