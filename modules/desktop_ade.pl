:- module(desktop_ade, [
    kb_module_summary/2,
    fact_sample/3,
    intent_catalog/3,
    wake_words/1
]).

% Read-only, bounded projections for the desktop Prolog inspector drawer.
% The drawer (zara/desktop/inspector.py) issues only these fixed goals via
% RunPrologQuery with the KB's ui_requested_solutions(50) cap. Nothing in
% this module consults, asserts, or retracts: it is a pure projection over
% the one authoritative engine (desktop_prolog_ide single_engine_authority).

%% kb_module_summary(-Module, -Count)
% Predicate-inventory rows for the KB facts browser: one row per known
% dynamic knowledge surface with its current clause count.
kb_module_summary(intents, Count) :-
    predicate_property(kb_intents:verb_intent(_, _, _), number_of_clauses(Count)).
kb_module_summary(config, Count) :-
    predicate_property(kb_config:wake_word(_), number_of_clauses(Count)).
kb_module_summary(object_words, Count) :-
    aggregate_all(count, object_words:object_word(_), Count).

%% fact_sample(+Module, -Term, +Limit)
% Bounded sample clauses of a module surface. Limit documents the drawer's
% requested bound; the UI additionally caps solutions at 50. Row terms
% mirror the stored clause shape so the drawer renders the KB verbatim.
fact_sample(intents, verb_intent(Phrase, Intent, Arity), _Limit) :-
    kb_intents:verb_intent(Phrase, Intent, Arity).
fact_sample(config, wake_word(Word), _Limit) :-
    kb_config:wake_word(Word).
fact_sample(object_words, object_word(Word), _Limit) :-
    object_words:object_word(Word).

%% intent_catalog(-Phrase, -Intent, -Arity)
% Static inventory of the intent KB for the drawer's Live/ADE view.
intent_catalog(Phrase, Intent, Arity) :-
    kb_intents:verb_intent(Phrase, Intent, Arity).

%% wake_words(-Word)
% Explicit wake words known to the configuration KB.
wake_words(Word) :-
    kb_config:wake_word(Word).
