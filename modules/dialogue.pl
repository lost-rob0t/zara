:- module(dialogue, [
    question/3,
    answer/5,
    register_question/3,
    register_choice_action/4,
    clear_question/1,
    clear_dialogue/0
]).

:- dynamic question/3.
:- dynamic choice_action/4.
:- multifile answer_rule/5.
:- dynamic answer_rule/5.

register_question(Id, Prompt, Choices) :-
    atom(Id),
    string(Prompt),
    is_list(Choices),
    retractall(question(Id, _, _)),
    assertz(question(Id, Prompt, Choices)).

register_choice_action(Id, Choice, Intent, Args) :-
    atom(Id),
    retractall(choice_action(Id, Choice, _, _)),
    assertz(choice_action(Id, Choice, Intent, Args)).

clear_question(Id) :-
    retractall(question(Id, _, _)),
    retractall(choice_action(Id, _, _, _)).

clear_dialogue :-
    retractall(question(_, _, _)),
    retractall(choice_action(_, _, _, _)).

answer(Id, RawAnswer, State, Intent, Args) :-
    answer_rule(Id, RawAnswer, State, Intent, Args),
    !.
answer(Id, RawAnswer, _State, Intent, Args) :-
    normalize_answer(RawAnswer, Answer),
    choice_action(Id, Choice, Intent, Args),
    normalize_answer(Choice, Answer),
    !.

normalize_answer(Value, Normalized) :-
    string(Value),
    !,
    string_lower(Value, Lower),
    normalize_space(string(Normalized), Lower).
normalize_answer(Value, Normalized) :-
    atom(Value),
    atom_string(Value, String),
    normalize_answer(String, Normalized).
