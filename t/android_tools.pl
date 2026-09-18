:- begin_tests(android_tools).

:- use_module('../modules/android_tools').

test(unique_tool_names) :-
    findall(Name, android_tool_descriptor(Name, _, _, _, _, _), Names),
    sort(Names, Unique),
    same_length(Names, Unique).

test(unique_wire_capabilities) :-
    findall(Wire, android_tool_descriptor(_, Wire, _, _, _, _), Wires),
    sort(Wires, Unique),
    same_length(Wires, Unique).

test(app_search_contract) :-
    android_tool_descriptor(
        app_search,
        app_search,
        navigation,
        standard,
        none,
        _
    ),
    android_tool_argument(app_search, app, string, required, 128),
    android_tool_argument(app_search, query, string, required, 512),
    android_tool_request(
        app_search,
        [app-youtube, query-psytrance],
        device_action(app_search, [app-youtube, query-psytrance])
    ).

test(live_capability_filter) :-
    android_tool_available(app_search, [open_app, app_search]),
    \+ android_tool_available(open_uri, [open_app, app_search]).

test(navigation_does_not_force_mutation_approval) :-
    \+ android_tool_requires_approval(app_search).

:- end_tests(android_tools).
