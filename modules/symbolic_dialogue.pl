:- module(symbolic_dialogue, [
    response_act/2,
    render_response/2,
    resolve_reference/3,
    symbolic_reply/3
]).

:- use_module('../modules/normalizer', [normalize_string/2]).

% Pure symbolic dialogue projection for #1252.
%
% This module owns no conversation persistence, expert registry, planner,
% permission state, provider runtime, or effect execution. It consumes typed
% outcomes from those canonical owners and produces deterministic response acts.
% Every public reply carries explicit zero-provider/zero-model evidence.

renderer_id('symbolic-dcg/v1').
max_render_codes(2048).
max_choice_count(8).
max_choice_codes(256).
max_summary_codes(1024).

symbolic_reply(Input, Text, Evidence) :-
    response_act(Input, Act),
    render_response(Act, Text),
    renderer_id(Renderer),
    Evidence = evidence(renderer(Renderer), provider_calls(0), model_calls(0)).

% --- Typed outcome -> response act -----------------------------------------

response_act(frame(frame(intent(ns(conversation), name(greet)), _, complete)), greeting) :- !.
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
response_act(frame(frame(intent(ns(_), name(_)), _, complete)), acknowledged) :- !.
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
    phrase(response_codes(Act), Codes),
    max_render_codes(Max),
    length(Codes, Length),
    Length =< Max,
    string_codes(Text, Codes).

response_codes(greeting) -->
    "Hey — what can I help with?".
response_codes(cancelled) -->
    "Cancelled.".
response_codes(acknowledged) -->
    "Okay.".
response_codes(clarify(slot(duration))) -->
    "How long should I set the timer for?".
response_codes(clarify(slot(Slot))) -->
    "What should I use for ", value_codes(Slot), "?".
response_codes(choose(Choices)) -->
    "I found a few matches: ", choice_list_codes(Choices, 1), ". Which one?".
response_codes(invalid(Slot, Reason)) -->
    "I couldn’t use ", value_codes(Slot), ": ", value_codes(Reason), ".".
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
