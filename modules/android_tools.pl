:- module(android_tools, [
    android_tool_descriptor/6,
    android_tool_argument/5,
    android_tool_request/3,
    android_tool_available/2,
    android_tool_requires_approval/1
]).

% Canonical Android tool catalog.
%
% Prolog owns semantic identity, argument contracts and policy metadata.
% Android/Kotlin owns only the actual OS adapter and permission boundary.
% Python projects these descriptors into LLM tools when a live initiating
% device advertises the matching wire capability.

android_tool_descriptor(
    open_app,
    open_app,
    navigation,
    standard,
    none,
    "Open a reviewed app alias on the initiating Android device."
).
android_tool_descriptor(
    open_uri,
    open_uri,
    navigation,
    standard,
    none,
    "Open a bounded URI through Android's reviewed intent adapter."
).
android_tool_descriptor(
    app_search,
    app_search,
    navigation,
    standard,
    none,
    "Search a reviewed Android app alias with a bounded query."
).

android_tool_argument(open_app, app, string, required, 128).
android_tool_argument(open_uri, uri, string, required, 2048).
android_tool_argument(app_search, app, string, required, 128).
android_tool_argument(app_search, query, string, required, 512).

android_tool_request(Name, Args, device_action(Wire, Args)) :-
    android_tool_descriptor(Name, Wire, _Effect, _Authority, _Permission, _Description).

android_tool_available(Name, Capabilities) :-
    android_tool_descriptor(Name, Wire, _Effect, _Authority, _Permission, _Description),
    memberchk(Wire, Capabilities).

android_tool_requires_approval(Name) :-
    android_tool_descriptor(Name, _Wire, Effect, _Authority, _Permission, _Description),
    memberchk(Effect, [mutating, destructive]).
