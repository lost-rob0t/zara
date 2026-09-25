:- module(symbolic_dialogue, [
    response_act/2,
    render_response/2,
    resolve_reference/3,
    resolve_discourse/3,
    symbolic_reply/3,
    symbolic_follow_up/4
]).

:- use_module('../modules/normalizer', [normalize_string/2]).

% Pure symbolic dialogue projection for #1252.
%
% This module owns no conversation persistence, expert registry, planner,
% permission state, provider runtime, or effect execution. It consumes typed
% outcomes from those canonical owners and produces deterministic response acts.
% Every public reply carries explicit zero-provider/zero-model evidence.
%
% A complete effect-shaped frame is never rendered as success. It remains a
% dispatch_required/1 act until the canonical capability/tool owner returns a
% verified effect_result with fresh postcondition evidence.

renderer_id('symbolic-dcg/v1').
max_render_codes(2048).
max_choice_count(8).
max_choice_codes(256).
max_summary_codes(1024).

symbolic_reply(Input, Text, Evidence) :-
    response_act(Input, Act),
    render_response(Act, Text),
    zero_model_evidence(Evidence).

symbolic_follow_up(Text, PreviousAct, ReplyText, Evidence) :-
    resolve_discourse(Text, PreviousAct, Act),
    render_response(Act, ReplyText),
    zero_model_evidence(Evidence).

zero_model_evidence(evidence(renderer(Renderer), provider_calls(0), model_calls(0))) :-
    renderer_id(Renderer).

% --- Typed outcome -> response act -----------------------------------------

response_act(frame(frame(intent(ns(conversation), name(greet)), _, complete)), greeting) :- !.
response_act(frame(frame(intent(ns(conversation), name(help)), _, complete)), help) :- !.
response_act(frame(frame(intent(ns(conversation), name(thanks)), _, complete)), acknowledgement(thanks)) :- !.
response_act(frame(frame(intent(ns(conversation), name(acknowledge)), _, complete)), acknowledgement(acknowledged)) :- !.
response_act(frame(frame(intent(ns(conversation), name(cancel)), _, complete)), cancelled) :- !.
response_act(frame(frame(_, _, missing([Slot|_]))), clarify(slot(Slot))) :-
    bounded_identifier(Slot),
    !.
response_act(frame(frame(_, _, ambiguous(Choices))), choose(Choices)) :-
    valid_choices(Choices),
    !.
response_act(frame(frame(_, _, invalid(value(Slot), Reason))), invalid(Slot, Reason)) :-
    bounded_identifier(Slot),
    bounded_identifier(Reason),
    !.
response_act(frame(Frame), dispatch_required(Frame)) :-
    Frame = frame(intent(ns(_), name(_)), _, complete),
    !.
response_act(effect_result(verified(Outcome, postcondition(EvidenceRef))),
        verified(Outcome, EvidenceRef)) :-
    bounded_text(Outcome, max_summary_codes),
    bounded_text(EvidenceRef, max_summary_codes),
    !.
response_act(plan_status(denied(Reason)), denied(Reason)) :-
    bounded_identifier(Reason),
    !.
response_act(plan_status(unavailable(Reason)), unavailable(Reason)) :-
    bounded_identifier(Reason),
    !.
response_act(plan_status(error(Reason)), error(Reason)) :-
    bounded_identifier(Reason),
    !.
response_act(expert_result(summary(Summary), evidence(EvidenceRef)),
        answer(expert, Summary, evidence(EvidenceRef))) :-
    bounded_text(Summary, max_summary_codes),
    bounded_text(EvidenceRef, max_choice_codes),
    !.
response_act(unsupported(_), unsupported) :- !.
response_act(_, unsupported).

% --- Follow-up reference resolution ---------------------------------------

resolve_reference(Text, Choices, Result) :-
    valid_choices(Choices),
    text_atom(Text, Atom),
    normalize_string(Atom, Tokens),
    resolve_reference_tokens(Tokens, Choices, Result),
    !.
resolve_reference(_, _, clarify(reference_not_found)).

resolve_reference_tokens(Tokens, Choices, Result) :-
    ordinal_reference(Tokens, Index),
    !,
    ( nth1(Index, Choices, Choice) ->
        Result = resolved(Choice)
    ; Result = clarify(reference_not_found)
    ).
resolve_reference_tokens(Tokens, Choices, Result) :-
    pronoun_reference(Tokens),
    !,
    ( Choices = [Only] ->
        Result = resolved(Only)
    ; Result = clarify(ambiguous_reference)
    ).
resolve_reference_tokens(Tokens, Choices, Result) :-
    findall(Choice,
        ( member(Choice, Choices),
          choice_tokens(Choice, Tokens)
        ),
        Matches),
    ( Matches = [Only] ->
        Result = resolved(Only)
    ; Matches = [] ->
        Result = clarify(reference_not_found)
    ; Result = clarify(ambiguous_reference)
    ).

% Resolve natural discourse follow-ups against a typed prior response act.
% The caller supplies the prior act from Zara's canonical conversation owner;
% this module keeps no history of its own.
resolve_discourse(Text, PreviousAct, Act) :-
    text_atom(Text, Atom),
    normalize_string(Atom, Tokens),
    discourse_act(Tokens, PreviousAct, Act),
    !.
resolve_discourse(_, _, clarify(reference_not_found)).

discourse_act(Tokens, PreviousAct, Act) :-
    why_reference(Tokens),
    !,
    ( expert_evidence_act(PreviousAct, EvidenceRef) ->
        evidence_explanation(EvidenceRef, Explanation),
        Act = answer(expert, Explanation, evidence(EvidenceRef))
    ; Act = clarify(reference_not_found)
    ).
discourse_act(Tokens, PreviousAct, Act) :-
    repeat_reference(Tokens),
    !,
    ( referenceable_act(PreviousAct) ->
        Act = PreviousAct
    ; Act = clarify(reference_not_found)
    ).
discourse_act(_, _, unsupported).

why_reference([why]).
why_reference([why, though]).
why_reference([why, is, that]).
why_reference([why, is, it]).
why_reference([why, did, you, do, that]).
why_reference([why, did, you, do, it]).

repeat_reference([it]).
repeat_reference([that]).
repeat_reference([this]).
repeat_reference([repeat, that]).
repeat_reference([say, that, again]).

expert_evidence_act(answer(expert, Summary, evidence(EvidenceRef)), EvidenceRef) :-
    bounded_text(Summary, max_summary_codes),
    bounded_text(EvidenceRef, max_choice_codes).

referenceable_act(Act) :-
    expert_evidence_act(Act, _).

evidence_explanation(EvidenceRef, Explanation) :-
    text_codes(EvidenceRef, EvidenceCodes),
    string_codes("I answered from evidence ", PrefixCodes),
    append(PrefixCodes, EvidenceCodes, PartialCodes),
    append(PartialCodes, [0'.], Codes),
    max_summary_codes(Max),
    length(Codes, Length),
    Length =< Max,
    string_codes(Explanation, Codes).

ordinal_reference([first], 1).
ordinal_reference([the, first, one], 1).
ordinal_reference([second], 2).
ordinal_reference([the, second, one], 2).
ordinal_reference([third], 3).
ordinal_reference([the, third, one], 3).
ordinal_reference([fourth], 4).
ordinal_reference([the, fourth, one], 4).
ordinal_reference([fifth], 5).
ordinal_reference([the, fifth, one], 5).
ordinal_reference([sixth], 6).
ordinal_reference([the, sixth, one], 6).
ordinal_reference([seventh], 7).
ordinal_reference([the, seventh, one], 7).
ordinal_reference([eighth], 8).
ordinal_reference([the, eighth, one], 8).
ordinal_reference([Token], Index) :-
    atom_number(Token, Index),
    integer(Index),
    Index >= 1,
    Index =< 8.

pronoun_reference([it]).
pronoun_reference([that]).
pronoun_reference([this]).
pronoun_reference([that, one]).
pronoun_reference([this, one]).

choice_tokens(Choice, Tokens) :-
    text_atom(Choice, Atom),
    normalize_string(Atom, Tokens).

% --- Deterministic response rendering -------------------------------------

render_response(Act, Text) :-
    phrase(response_codes(Act), Units),
    response_units_codes(Units, Codes),
    max_render_codes(Max),
    length(Codes, Length),
    Length =< Max,
    string_codes(Text, Codes).

% SWI emits DCG string literals as integer code units while pinned Trealla
% emits one-character atoms. Normalize only those two inert representations
% before constructing the response string; any other unit fails closed.
response_units_codes([], []).
response_units_codes([Unit|Units], [Code|Codes]) :-
    response_unit_code(Unit, Code),
    response_units_codes(Units, Codes).

response_unit_code(Unit, Unit) :-
    integer(Unit),
    Unit >= 0,
    Unit =< 1114111,
    !.
response_unit_code(Unit, Code) :-
    atom(Unit),
    atom_codes(Unit, [Code]),
    integer(Code),
    Code >= 0,
    Code =< 1114111.

response_codes(greeting) -->
    "Hey — what can I help with?".
response_codes(help) -->
    "I can help with conversation, device and media actions, search, navigation, and registered experts. What do you want to do?".
response_codes(acknowledgement(thanks)) -->
    "You’re welcome.".
response_codes(acknowledgement(acknowledged)) -->
    "Got it.".
response_codes(cancelled) -->
    "Cancelled.".
response_codes(clarify(slot(duration))) -->
    "How long should I set the timer for?".
response_codes(clarify(slot(Slot))) -->
    "What should I use for ", value_codes(Slot), "?".
response_codes(clarify(reference_not_found)) -->
    "What are you referring to?".
response_codes(clarify(ambiguous_reference)) -->
    "Which one do you mean?".
response_codes(choose(Choices)) -->
    "I found a few matches: ", choice_list_codes(Choices, 1), ". Which one?".
response_codes(invalid(Slot, Reason)) -->
    "I couldn’t use ", value_codes(Slot), ": ", value_codes(Reason), ".".
response_codes(dispatch_required(_Frame)) -->
    "That action needs capability-checked execution before I can report success.".
response_codes(verified(Outcome, _EvidenceRef)) -->
    "Done: ", value_codes(Outcome), ".".
response_codes(denied(Reason)) -->
    "I can’t do that: ", value_codes(Reason), ".".
response_codes(unavailable(Reason)) -->
    "That isn’t available right now: ", value_codes(Reason), ".".
response_codes(error(Reason)) -->
    "That failed: ", value_codes(Reason), ".".
response_codes(answer(expert, Summary, evidence(_EvidenceRef))) -->
    value_codes(Summary).
response_codes(unsupported) -->
    "I don’t know how to handle that symbolically yet.".

choice_list_codes([Choice], Index) -->
    integer_codes(Index), ") ", value_codes(Choice).
choice_list_codes([Choice|Rest], Index) -->
    integer_codes(Index), ") ", value_codes(Choice), "; ",
    { Next is Index + 1 },
    choice_list_codes(Rest, Next).

integer_codes(Integer) -->
    { number_codes(Integer, Codes) },
    emit_codes(Codes).

value_codes(Value) -->
    { text_codes(Value, Codes) },
    emit_codes(Codes).

emit_codes([]) --> [].
emit_codes([Code|Codes]) --> [Code], emit_codes(Codes).

% --- Bounds / inert data validation ---------------------------------------

valid_choices(Choices) :-
    is_list(Choices),
    Choices = [_|_],
    max_choice_count(Max),
    length(Choices, Count),
    Count =< Max,
    maplist(valid_choice, Choices).

valid_choice(Choice) :-
    bounded_text(Choice, max_choice_codes).

bounded_identifier(Value) :-
    ( atom(Value) ; string(Value) ),
    bounded_text(Value, max_choice_codes).

bounded_text(Value, LimitPredicate) :-
    text_codes(Value, Codes),
    call(LimitPredicate, Max),
    Codes = [_|_],
    length(Codes, Length),
    Length =< Max.

text_atom(Value, Atom) :-
    atom(Value),
    !,
    Atom = Value.
text_atom(Value, Atom) :-
    string(Value),
    !,
    string_codes(Value, Codes),
    atom_codes(Atom, Codes).

text_codes(Value, Codes) :-
    string(Value),
    !,
    string_codes(Value, Codes).
text_codes(Value, Codes) :-
    atom(Value),
    !,
    atom_codes(Value, Codes).
