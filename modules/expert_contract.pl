:- module(expert_contract, [
    expert_protocol/1,
    expert_descriptor/5,
    expert_descriptor_protocol/2,
    expert_operation/4,
    expert_availability/2,
    expert_keyword/2,
    expert_alias/3,
    expert_verdict/1,
    expert_error_code/1,
    expert_activation_transition/2,
    expert_protocol_compatible/1,
    selectable_expert/1,
    expert_catalog_row/4,
    expert_match/3,
    expert_match_ambiguous/2,
    expert_corpus_lines/1,
    emit_expert_corpus/1,
    assert_expert_alias/3
]).
% Portable ZARA-EXPERT/1 codec (issue #1233, phase 2).
%
% Mirrors the Python contract authority zara/experts.py for both swipl and
% Trealla hosts: the curated descriptor registry projection seeded from
% contracts/zara-expert-v1/descriptors.tsv, per-expert operations and
% applicability keywords, the closed verdict/error vocabularies, the
% activation transition table, protocol-major gating and pure keyword
% applicability with typed ambiguity. Activation, invocation and execution
% authority stay with their owners (#897/#663); this codec only projects
% contract state.
%
% expert_protocol(Protocol)
%   the protocol constant atom 'ZARA-EXPERT/1'
% expert_descriptor(Id, Version, Package, Kind, ManifestDigest)
%   dynamic curated facts; Id is the canonical #986 symbol atom
% expert_descriptor_protocol(Id, Protocol)
%   declared protocol major per expert; incompatible-major descriptors stay
%   visible but never selectable, mirroring Python's protocol_compatible
%   gating (the caller maps failures to typed errors)
% expert_operation(Id, Operation, InputFields, OutputFields)
%   flat operation projection; TSV operations carry no field schemas
% expert_availability(Id, Availability)
%   Availability in installed|available|ready|unavailable|absent as
%   authenticated current evidence, exactly as the TSV fixture encodes it
% expert_keyword(Id, Keyword)
%   applicability keywords driving pure matching
% expert_alias(Scope, Alias, Id)
%   dynamic, no default facts; assert_expert_alias/3 is the only mutator
% expert_verdict(Verdict)
%   the closed seven verdicts in zara/experts.py declaration order
% expert_error_code(Code)
%   the closed fourteen error codes in zara/experts.py declaration order
% expert_activation_transition(From, To)
%   the exact activation lifecycle table
% expert_protocol_compatible(Protocol)
%   accepts only 'ZARA-EXPERT/1'; other majors fail
% selectable_expert(Id)
%   protocol-compatible descriptor whose availability is in
%   installed|available|ready (Python _SELECTABLE_AVAILABILITY)
% expert_catalog_row(Id, Version, Kind, Availability)
%   flat row projection for marshalling (the plan_head_row pattern)
% expert_match(Task, Id, Reason)
%   pure keyword applicability over a task atom: tokens are lowercased
%   alphanumeric runs; a keyword hits when a token equals it or has it as a
%   prefix (the Python match rule); the unique best-scoring selectable
%   expert wins with Reason its sorted matched keywords; ties fail closed
% expert_match_ambiguous(Task, Ids)
%   companion tie signal with sorted candidate ids when the best score is
%   shared (the ambiguous-plan pattern from capability_plans; ties never
%   select silently)
% expert_corpus_lines(Lines)
%   deterministic corpus: one ground fact line per descriptor, operation
%   and keyword fact, sorted
% emit_expert_corpus(Stream)
%   writes the corpus lines; the only I/O in this module
%
% Pure: no assert/retract outside assert_expert_alias/3, no I/O outside
% emit_expert_corpus/1, no engine-specific builtins (hand-rolled helpers
% instead of partition/4, memberchk/2 or string primitives).

:- dynamic expert_descriptor/5.
:- dynamic expert_descriptor_protocol/2.
:- dynamic expert_operation/4.
:- dynamic expert_availability/2.
:- dynamic expert_keyword/2.
:- dynamic expert_alias/3.

expert_protocol('ZARA-EXPERT/1').

expert_descriptor('zara:expert/android-troubleshooting', '0.3.0', zara,
    hybrid, 'sha256:android.troubleshoot.v1').
expert_descriptor('zara:expert/future', '2.0.0', zara, symbolic,
    'sha256:future.expert.v2').
expert_descriptor('zara:expert/prolog-rlm', '0.9.1', zara, service,
    'sha256:prolog.rlm.v1').
expert_descriptor('zara:expert/todo', '1.0.0', zara, symbolic,
    'sha256:todo.expert.v1').

expert_descriptor_protocol('zara:expert/android-troubleshooting',
    'ZARA-EXPERT/1').
expert_descriptor_protocol('zara:expert/future', 'ZARA-EXPERT/2').
expert_descriptor_protocol('zara:expert/prolog-rlm', 'ZARA-EXPERT/1').
expert_descriptor_protocol('zara:expert/todo', 'ZARA-EXPERT/1').

expert_operation('zara:expert/android-troubleshooting', 'adb.diagnose',
    [], []).
expert_operation('zara:expert/android-troubleshooting', 'logs.explain',
    [], []).
expert_operation('zara:expert/future', 'demo.ping', [], []).
expert_operation('zara:expert/prolog-rlm', 'rlm.explain', [], []).
expert_operation('zara:expert/prolog-rlm', 'rlm.query', [], []).
expert_operation('zara:expert/todo', 'route.diagnose', [], []).
expert_operation('zara:expert/todo', 'route.explain', [], []).

expert_availability('zara:expert/android-troubleshooting', available).
expert_availability('zara:expert/future', ready).
expert_availability('zara:expert/prolog-rlm', absent).
expert_availability('zara:expert/todo', ready).

expert_keyword('zara:expert/android-troubleshooting', adb).
expert_keyword('zara:expert/android-troubleshooting', android).
expert_keyword('zara:expert/android-troubleshooting', build).
expert_keyword('zara:expert/android-troubleshooting', wear).
expert_keyword('zara:expert/future', future).
expert_keyword('zara:expert/prolog-rlm', prolog).
expert_keyword('zara:expert/prolog-rlm', reasoning).
expert_keyword('zara:expert/prolog-rlm', rlm).
expert_keyword('zara:expert/todo', reminders).
expert_keyword('zara:expert/todo', tasks).
expert_keyword('zara:expert/todo', todo).

expert_verdict(succeeded).
expert_verdict(failed).
expert_verdict(unknown).
expert_verdict(blocked).
expert_verdict(unsupported).
expert_verdict(cancelled).
expert_verdict(error).

expert_error_code(invalid_input).
expert_error_code(ambiguity).
expert_error_code(unsupported_operation).
expert_error_code(unsupported_backend).
expert_error_code(incompatible_protocol).
expert_error_code(denied).
expert_error_code(approval_required).
expert_error_code(stale_generation).
expert_error_code(unavailable).
expert_error_code(deadline_exceeded).
expert_error_code(budget_exceeded).
expert_error_code(cancelled).
expert_error_code(interrupted).
expert_error_code(unknown_external_outcome).

expert_activation_transition(inactive, activating).
expert_activation_transition(activating, active).
expert_activation_transition(activating, failed).
expert_activation_transition(active, draining).
expert_activation_transition(active, unavailable).
expert_activation_transition(active, failed).
expert_activation_transition(draining, inactive).
expert_activation_transition(unavailable, active).
expert_activation_transition(failed, inactive).

expert_protocol_compatible('ZARA-EXPERT/1').

selectable_expert(Id) :-
    expert_descriptor(Id, _, _, _, _),
    expert_descriptor_protocol(Id, Protocol),
    expert_protocol_compatible(Protocol),
    expert_availability(Id, Availability),
    selectable_availability(Availability).

selectable_availability(installed).
selectable_availability(available).
selectable_availability(ready).

expert_catalog_row(Id, Version, Kind, Availability) :-
    expert_descriptor(Id, Version, _, Kind, _),
    expert_availability(Id, Availability).

expert_match(Task, Id, Keywords) :-
    expert_task_tokens(Task, Tokens),
    expert_match_scored(Tokens, Scored),
    Scored = [_|_],
    expert_top_scored(Scored, Top),
    Top = [_-Id-Keywords].

expert_match_ambiguous(Task, Ids) :-
    expert_task_tokens(Task, Tokens),
    expert_match_scored(Tokens, Scored),
    Scored = [_|_],
    expert_top_scored(Scored, Top),
    Top = [_, _ | _],
    expert_scored_ids(Top, Ids0),
    sort(Ids0, Ids).

expert_match_scored(Tokens, Scored) :-
    findall(Score-Id-Keywords,
        ( selectable_expert(Id),
          expert_matched_keywords(Id, Tokens, Matched),
          Matched \= [],
          sort(Matched, Keywords),
          length(Keywords, Score) ),
        Scored).

expert_matched_keywords(Id, Tokens, Matched) :-
    findall(Keyword,
        ( expert_keyword(Id, Keyword),
          expert_keyword_hit(Keyword, Tokens) ),
        Matched).

expert_keyword_hit(Keyword, Tokens) :-
    member(Token, Tokens),
    ( Token == Keyword ->
        true
    ; sub_atom(Token, 0, _, _, Keyword)
    ).

expert_top_scored(Scored, Top) :-
    expert_max_score(Scored, Max),
    expert_with_score(Scored, Max, Top).

expert_max_score([Score-_-_], Score) :-
    !.
expert_max_score([Score-_-_|Rest], Max) :-
    expert_max_score(Rest, RestMax),
    ( Score > RestMax ->
        Max = Score
    ; Max = RestMax
    ).

expert_with_score([], _, []).
expert_with_score([Entry|Rest], Score, [Entry|Kept]) :-
    Entry = Score-_-_,
    !,
    expert_with_score(Rest, Score, Kept).
expert_with_score([_|Rest], Score, Kept) :-
    expert_with_score(Rest, Score, Kept).

expert_scored_ids([], []).
expert_scored_ids([_-Id-_|Rest], [Id|Ids]) :-
    expert_scored_ids(Rest, Ids).

expert_task_tokens(Task, Tokens) :-
    atom(Task),
    atom_codes(Task, Codes),
    expert_lowercase_codes(Codes, Lowered),
    expert_codes_tokens(Lowered, Tokens).

expert_lowercase_codes([], []).
expert_lowercase_codes([Code|Rest], [Lowered|LoweredRest]) :-
    ( Code >= 0'A, Code =< 0'Z ->
        Lowered is Code + 32
    ; Lowered = Code
    ),
    expert_lowercase_codes(Rest, LoweredRest).

expert_codes_tokens([], []).
expert_codes_tokens([Code|Rest], [Token|Tokens]) :-
    expert_alnum_code(Code),
    !,
    expert_take_alnum([Code|Rest], TokenCodes, RestCodes),
    atom_codes(Token, TokenCodes),
    expert_codes_tokens(RestCodes, Tokens).
expert_codes_tokens([_|Rest], Tokens) :-
    expert_codes_tokens(Rest, Tokens).

expert_take_alnum([], [], []).
expert_take_alnum([Code|Rest], [Code|TokenCodes], RestCodes) :-
    expert_alnum_code(Code),
    !,
    expert_take_alnum(Rest, TokenCodes, RestCodes).
expert_take_alnum([Code|Rest], [], [Code|Rest]) :-
    \+ expert_alnum_code(Code).

expert_alnum_code(Code) :-
    ( Code >= 0'a, Code =< 0'z ->
        true
    ; Code >= 0'0, Code =< 0'9
    ).

expert_corpus_lines(Lines) :-
    findall(Line, expert_corpus_line(Line), Unsorted),
    sort(Unsorted, Lines).

expert_corpus_line(Line) :-
    expert_corpus_fact(Fact),
    term_to_atom(Fact, FactAtom),
    atom_concat(FactAtom, '.', Line).

expert_corpus_fact(expert_descriptor(Id, Version, Package, Kind, Digest)) :-
    expert_descriptor(Id, Version, Package, Kind, Digest).
expert_corpus_fact(expert_operation(Id, Operation, InputFields, OutputFields)) :-
    expert_operation(Id, Operation, InputFields, OutputFields).
expert_corpus_fact(expert_keyword(Id, Keyword)) :-
    expert_keyword(Id, Keyword).

emit_expert_corpus(Stream) :-
    expert_corpus_lines(Lines),
    emit_corpus_lines(Lines, Stream).

emit_corpus_lines([], _).
emit_corpus_lines([Line|Rest], Stream) :-
    write(Stream, Line),
    nl(Stream),
    emit_corpus_lines(Rest, Stream).

assert_expert_alias(Scope, Alias, Id) :-
    expert_alias_parts_valid(Scope, Alias, Id),
    expert_alias_target_registered(Id),
    expert_alias_slot_free(Scope, Alias, Id).

expert_alias_parts_valid(Scope, Alias, Id) :-
    ( atom(Scope), atom(Alias), atom(Id) ->
        true
    ; throw(error(domain_error(zara_expert_alias, Scope-Alias-Id), _))
    ).

expert_alias_target_registered(Id) :-
    ( expert_descriptor(Id, _, _, _, _) ->
        true
    ; throw(error(domain_error(zara_expert_alias, Id), _))
    ).

expert_alias_slot_free(Scope, Alias, Id) :-
    ( expert_alias(Scope, Alias, Existing) ->
        ( Existing == Id ->
            true
        ; throw(error(domain_error(zara_expert_alias, Scope-Alias-Existing), _))
        )
    ; assertz(expert_alias(Scope, Alias, Id))
    ).
