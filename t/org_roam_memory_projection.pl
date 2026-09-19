:- begin_tests(org_roam_memory_projection).

:- use_module('../modules/org_roam_memory').

setup_projection :-
    org_roam_memory:clear_org_memory.

test(replace_projects_inert_memory_fact, [setup(setup_projection)]) :-
    org_roam_memory:replace_org_memory(
        "org-roam:node-1",
        "node-1",
        "zara",
        "* Build renderer\n#+begin_src prolog\nhalt.\n#+end_src",
        ["org-roam", "symbolic-memory", "project:zara"]
    ),
    org_roam_memory:org_memory(
        "org-roam:node-1",
        "node-1",
        "zara",
        Text,
        Tags
    ),
    sub_string(Text, _, _, _, "halt."),
    member("symbolic-memory", Tags),
    org_roam_memory:org_project_context("zara", "node-1", Text).

test(replace_is_idempotent_and_updates_source, [setup(setup_projection)]) :-
    org_roam_memory:replace_org_memory("source", "node", "p", "old", ["one"]),
    org_roam_memory:replace_org_memory("source", "node", "p", "new", ["two"]),
    findall(Text, org_roam_memory:org_memory("source", _, _, Text, _), Rows),
    assertion(Rows == ["new"]).

test(remove_and_clear, [setup(setup_projection)]) :-
    org_roam_memory:replace_org_memory("a", "node-a", "p", "A", []),
    org_roam_memory:replace_org_memory("b", "node-b", "p", "B", []),
    org_roam_memory:remove_org_memory("a"),
    \+ org_roam_memory:org_memory("a", _, _, _, _),
    org_roam_memory:org_memory("b", _, _, _, _),
    org_roam_memory:clear_org_memory,
    \+ org_roam_memory:org_memory(_, _, _, _, _).

:- end_tests(org_roam_memory_projection).
