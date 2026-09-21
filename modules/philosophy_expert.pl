:- module(philosophy_expert,
    [
        concept_summary/3,
        philosopher_summary/5,
        position_summary/5,
        compare_positions/5,
        argument_summary/5,
        objection_summary/3,
        fallacy_summary/3,
        recommend_framework/3
    ]).

:- use_module('../kb/philosophy').

concept_summary(Input, Canonical, Summary) :-
    canonical_input(Input, Canonical),
    kb_philosophy:concept(Canonical, Summary).

philosopher_summary(Input, Canonical, Display, School, Era) :-
    canonical_input(Input, Canonical),
    kb_philosophy:philosopher(Canonical, Display, School, Era).

position_summary(PhilosopherInput, TopicInput, Philosopher, Topic, Position) :-
    canonical_input(PhilosopherInput, Philosopher),
    canonical_input(TopicInput, Topic),
    kb_philosophy:position(Philosopher, Topic, Position).

compare_positions(TopicInput, LeftInput, RightInput, LeftPosition, RightPosition) :-
    position_summary(LeftInput, TopicInput, _, _, LeftPosition),
    position_summary(RightInput, TopicInput, _, _, RightPosition).

argument_summary(Input, Name, Tradition, Premises, Conclusion) :-
    canonical_input(Input, Name),
    kb_philosophy:argument(Name, Tradition, _, Premises, Conclusion).

objection_summary(Input, Label, Summary) :-
    canonical_input(Input, Target),
    kb_philosophy:objection(Target, Label, Summary).

fallacy_summary(Input, Name, Summary) :-
    canonical_input(Input, Name),
    kb_philosophy:fallacy(Name, Summary).

recommend_framework(Input, Framework, Why) :-
    canonical_input(Input, Goal),
    framework_for_goal(Goal, Framework, Why).

framework_for_goal(character, virtue_ethics,
    "Use virtue ethics when the central question is what traits, habits, and practical judgment a good person should cultivate.").
framework_for_goal(control, stoicism,
    "Use Stoicism to separate judgments and choices from externals that cannot be fully controlled.").
framework_for_goal(duty, deontology,
    "Use deontology when the question turns on obligations, constraints, universal rules, or respect for persons.").
framework_for_goal(consequences, utilitarianism,
    "Use utilitarianism when comparing expected effects on welfare and suffering is the central moral problem.").
framework_for_goal(meaning, existentialism,
    "Use existentialism when freedom, responsibility, commitment, and the lived construction of meaning are central.").
framework_for_goal(absurdity, absurdism,
    "Use absurdism when the tension between the demand for meaning and an indifferent world is the central problem.").
framework_for_goal(experience, empiricism,
    "Use empiricism when the dispute is primarily about what observation and experience justify.").
framework_for_goal(reason, rationalism,
    "Use rationalism when the dispute is primarily about what reason can establish independently of particular observations.").
framework_for_goal(practice, pragmatism,
    "Use pragmatism when inquiry should be evaluated through consequences, problem solving, and revisability in practice.").

canonical_input(Input, Canonical) :-
    text_string(Input, Raw),
    normalize_space(string(Trimmed), Raw),
    Trimmed \= "",
    string_lower(Trimmed, Lower),
    ( kb_philosophy:alias(Lower, Canonical)
    -> true
    ; split_string(Lower, " _-", " _-", Parts),
      Parts \= [],
      atomics_to_string(Parts, "_", Slug),
      atom_string(Canonical, Slug)
    ).

text_string(Value, String) :-
    ( string(Value)
    -> String = Value
    ; atom(Value)
    -> atom_string(Value, String)
    ).
