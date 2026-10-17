:- module(file_skill_resolver, [resolved_tool/2, tool_for_action/2]).

:- use_module('../kb/file_skills').

resolved_tool(Skill, Tool) :-
    kb_file_skills:file_skill(Skill, Tool, _Access).

tool_for_action(Action, Tool) :-
    nonvar(Action),
    kb_file_skills:file_skill(Action, Tool, _Access).
