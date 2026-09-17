:- module(response_policy, [
    analyze_json/2, catalog_json/1, settings_json/1,
    user_rule/6, user_source/4
]).

:- use_module(library(apply)).
:- use_module(library(error)).
:- use_module(library(http/json)).
:- use_module(library(lists)).
:- use_module(library(solution_sequences)).
:- use_module(library(time)).
:- use_module('../kb/response_policy', []).
:- use_module('../kb/policy_config', []).
:- multifile user_rule/6, user_source/4.
:- dynamic user_rule/6, user_source/4.

settings_json(Json) :-
    call_with_time_limit(0.5, settings(Settings)),
    atom_json_dict(Json, Settings, [as(string)]).

settings(Settings) :-
    findall(Key-Value,
        ( policy_config:default_setting(Key, _), policy_config:setting(Key, Value) ),
        Pairs),
    dict_create(Settings, policy, Pairs),
    must_be(boolean, Settings.enabled),
    ( memberchk(Settings.mode, [observe, advise]) -> true
    ; throw(error(domain_error(policy_mode, Settings.mode), _)) ),
    bounded_integer(Settings.max_input_chars, 1, 262144),
    bounded_integer(Settings.max_findings, 1, 64),
    bounded_integer(Settings.max_revisions, 0, 1),
    must_be(number, Settings.revision_timeout_seconds),
    ( Settings.revision_timeout_seconds > 0,
      Settings.revision_timeout_seconds =< 120 -> true
    ; throw(error(domain_error(policy_timeout, Settings.revision_timeout_seconds), _)) ),
    must_be(list(atom), Settings.disabled_rules).

bounded_integer(Value, Min, Max) :-
    must_be(integer, Value),
    ( between(Min, Max, Value) -> true
    ; throw(error(domain_error(policy_integer_range, Value), _)) ).

analyze_json(Text, Json) :-
    catch(
        ( call_with_inference_limit(
              call_with_time_limit(0.5, analyze(Text, Candidate)), 2000000, Limit)
        -> ( Limit == inference_limit_exceeded
           -> Report = _{status:budget_exceeded, findings:[]}
           ; Report = Candidate )
        ; Report = _{status:inspection_failed, findings:[]}
        ),
        Error, exception_report(Error, Report)),
    atom_json_dict(Json, Report, [as(string)]), !.

exception_report(time_limit_exceeded, _{status:budget_exceeded, findings:[]}) :- !.
exception_report(inference_limit_exceeded, _{status:budget_exceeded, findings:[]}) :- !.
exception_report(_, _{status:inspection_failed, findings:[]}).

analyze(Text, Report) :-
    must_be(string, Text),
    settings(Settings),
    string_length(Text, Length),
    ( Settings.enabled == false
    -> Report = _{status:disabled, findings:[]}
    ; Length > Settings.max_input_chars
    -> Report = _{status:input_limit, findings:[], truncated:true}
    ; sentences(Text, Sentences),
      effective_rules(Rules),
      findall(Key-Finding,
          ( member(Rule, Rules),
            Rule = rule(Id, Category, Severity, Patterns, Advice, Source),
            \+ memberchk(Id, Settings.disabled_rules),
            once(matching_pattern(Sentences, Patterns, Pattern)),
            rank(Severity, Rank), Key = key(Rank, Id),
            repair_mode(Category, Severity, Repair),
            provenance(Id, Source, Origin, Provenance),
            Finding = _{id:Id, category:Category, severity:Severity,
                pattern:Pattern, advice:Advice, source:Provenance,
                origin:Origin, repair:Repair, confidence:heuristic}
          ), Pairs),
      keysort(Pairs, Sorted),
      pairs_values_local(Sorted, Findings0),
      take(Settings.max_findings, Findings0, Findings),
      length(Findings0, Total), length(Findings, Returned),
      ( Total > Returned -> Truncated = true ; Truncated = false ),
      Report = _{status:ok, language:en, findings:Findings, truncated:Truncated}
    ).

rank(error, 0).
rank(warning, 1).
rank(info, 2).

repair_mode(Category, Severity, Repair) :-
    ( memberchk(Category, [capability, refusal, style, sycophancy])
    -> Repair = false
    ; Severity == info -> Repair = false
    ; Repair = true ).

effective_rule(rule(Id, Category, Severity, Patterns, Advice, Source)) :-
    user_rule(Id, Category, Severity, Patterns, Advice, Source).
effective_rule(rule(Id, Category, Severity, Patterns, Advice, Source)) :-
    kb_response_policy:rule(Id, Category, Severity, Patterns, Advice, Source),
    \+ user_rule(Id, _, _, _, _, _).

effective_rules(Rules) :-
    once(findnsols(257, Rule, effective_rule(Rule), Rules)),
    length(Rules, Count),
    ( Count =< 256 -> true ; throw(error(resource_error(policy_rules), _)) ),
    maplist(valid_rule, Rules),
    findall(Id, member(rule(Id, _, _, _, _, _), Rules), Ids),
    sort(Ids, Unique), length(Unique, UniqueCount),
    ( UniqueCount =:= Count -> true
    ; throw(error(domain_error(unique_policy_rule_ids, Ids), _)) ).

valid_rule(rule(Id, Category, Severity, Patterns, Advice, Source)) :-
    must_be(atom, Id), atom_length(Id, IdLength), bounded_integer(IdLength, 1, 128),
    must_be(atom, Category), must_be(atom, Source),
    ( rank(Severity, _) -> true ; throw(error(domain_error(policy_severity, Severity), _)) ),
    must_be(list(string), Patterns), length(Patterns, Count), bounded_integer(Count, 1, 16),
    maplist(valid_pattern, Patterns),
    must_be(string, Advice), string_length(Advice, Length), bounded_integer(Length, 1, 1024).

valid_pattern(Pattern) :-
    string_length(Pattern, Length), bounded_integer(Length, 1, 160),
    ( tokens(Pattern, [_|_]) -> true
    ; throw(error(domain_error(nonempty_policy_pattern, Pattern), _)) ).

provenance(Id, Source, Origin, Provenance) :-
    ( user_rule(Id, _, _, _, _, _) -> Origin = user ; Origin = bundled ),
    ( user_source(Source, Title, Url, Date) -> true
    ; kb_response_policy:source(Source, Title, Url, Date) -> true
    ; Title = "Operator extension", Url = "local:operator", Date = "unspecified" ),
    Provenance = _{id:Source, title:Title, url:Url, date:Date,
        basis:research_inspired_operator_authored}.

catalog_json(Json) :-
    call_with_time_limit(0.5, catalog(Catalog)),
    atom_json_dict(Json, Catalog, [as(string)]).

catalog(Catalog) :-
    settings(Settings), effective_rules(Rules),
    findall(Entry,
        ( member(rule(Id, Category, Severity, Patterns, Advice, Source), Rules),
          provenance(Id, Source, Origin, Provenance),
          ( memberchk(Id, Settings.disabled_rules) -> Enabled = false ; Enabled = true ),
          repair_mode(Category, Severity, Repair),
          Entry = _{id:Id, category:Category, severity:Severity, patterns:Patterns,
              advice:Advice, source:Provenance, origin:Origin, enabled:Enabled,
              repair:Repair}
        ), Entries),
    Catalog = _{schema_version:1, language:en, rules:Entries, settings:Settings}.

matching_pattern(Sentences, Patterns, Pattern) :-
    member(Pattern, Patterns), tokens(Pattern, Needle),
    member(Words, Sentences),
    \+ conditional(Words),
    append(Prefix, Tail, Words), append(Needle, _, Tail),
    \+ negated_prefix(Prefix).

conditional([First|_]) :-
    memberchk(First, ["if", "unless", "when", "suppose", "hypothetically", "example"]), !.
conditional(["for", "example"|_]).
conditional(["the", "phrase"|_]).
conditional(["the", "string"|_]).

negated_prefix(Prefix) :-
    reverse(Prefix, Reversed), take(5, Reversed, Near),
    member(Word, Near),
    memberchk(Word, ["not", "never", "cannot", "can't", "didn't", "don't", "without"]), !.

sentences(Text, Sentences) :-
    split_string(Text, "\n", "\r", Lines),
    visible_lines(Lines, none, Visible),
    atomics_to_string(Visible, "\n", Joined),
    split_string(Joined, ".!?;\n", " \t\r", Parts),
    maplist(tokens, Parts, Sentences).

visible_lines([], _, []).
visible_lines([Line|Lines], Fence, Visible) :-
    normalize_space(string(Trimmed), Line),
    ( fence_marker(Trimmed, Marker)
    -> ( Fence == none -> Next = Marker
       ; Fence == Marker -> Next = none
       ; Next = Fence ),
       visible_lines(Lines, Next, Visible)
    ; Fence \== none
    -> visible_lines(Lines, Fence, Visible)
    ; sub_string(Trimmed, 0, 1, _, ">")
    -> visible_lines(Lines, Fence, Visible)
    ; string_codes(Line, Codes), maplist(normalize_quote, Codes, Normalized),
      strip_inline(Normalized, plain, 0, Kept), string_codes(Plain, Kept),
      Visible = [Plain|Rest], visible_lines(Lines, Fence, Rest)
    ).

fence_marker(Line, backticks) :- sub_string(Line, 0, 3, _, "```"), !.
fence_marker(Line, tildes) :- sub_string(Line, 0, 3, _, "~~~").

normalize_quote(Code, 39) :- memberchk(Code, [8216, 8217]), !.
normalize_quote(Code, 34) :- memberchk(Code, [8220, 8221]), !.
normalize_quote(Code, Code).

strip_inline([], _, _, []).
strip_inline([Code|Codes], quoted(Quote), _, [32|Rest]) :-
    Code =:= Quote, !, strip_inline(Codes, plain, 0, Rest).
strip_inline([_|Codes], quoted(Quote), _, Rest) :- !,
    strip_inline(Codes, quoted(Quote), 0, Rest).
strip_inline([Code|Codes], plain, Previous, [32|Rest]) :-
    ( memberchk(Code, [34, 96])
    ; Code =:= 39, \+ code_type(Previous, alnum) ), !,
    strip_inline(Codes, quoted(Code), 0, Rest).
strip_inline([Code|Codes], plain, _, [Code|Rest]) :-
    strip_inline(Codes, plain, Code, Rest).

tokens(Text, Words) :-
    string_lower(Text, Lower), string_codes(Lower, Codes),
    maplist(token_code, Codes, Normalized), string_codes(Clean, Normalized),
    split_string(Clean, " ", " ", Words).

token_code(Code, Code) :- ( code_type(Code, alnum) ; Code =:= 39 ), !.
token_code(_, 32).

pairs_values_local([], []).
pairs_values_local([_-Value|Pairs], [Value|Values]) :- pairs_values_local(Pairs, Values).

take(0, _, []) :- !.
take(_, [], []) :- !.
take(Count, [Item|Items], [Item|Rest]) :-
    Next is Count - 1, take(Next, Items, Rest).
