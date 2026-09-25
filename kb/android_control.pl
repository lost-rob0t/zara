:- module(kb_android_control,
    [
        android_adb_plan/2,
        android_app_package/2,
        android_vision_policy/1,
        valid_android_adb_plan/2,
        valid_android_app_package/2,
        valid_android_vision_policy/1
    ]).

:- dynamic android_adb_plan/2.
:- dynamic android_app_package/2.
:- dynamic android_vision_policy/1.

% The model may observe screenshots when the desktop ADB plugin is enabled, but
% every state-changing vision action still travels through Zara's canonical
% tool-approval path.
android_vision_policy(confirm_each_action).

valid_android_adb_plan(Name, Actions) :-
    bounded_atom(Name, 64),
    is_list(Actions),
    length(Actions, Count),
    Count =< 32,
    maplist(valid_android_adb_action, Actions).

valid_android_adb_action(tap(X, Y)) :-
    coordinate(X),
    coordinate(Y).
valid_android_adb_action(swipe(X1, Y1, X2, Y2, DurationMs)) :-
    coordinate(X1),
    coordinate(Y1),
    coordinate(X2),
    coordinate(Y2),
    integer(DurationMs),
    between(1, 5000, DurationMs).
valid_android_adb_action(text(Text)) :-
    bounded_text(Text, 512).
valid_android_adb_action(key(Key)) :-
    memberchk(Key, [back, home, enter, recents, tab, escape, delete,
                    up, down, left, right]).
valid_android_adb_action(wait(DurationMs)) :-
    integer(DurationMs),
    between(0, 5000, DurationMs).
valid_android_adb_action(open_app(Alias)) :-
    bounded_atom(Alias, 64).

valid_android_app_package(Alias, Package) :-
    bounded_atom(Alias, 64),
    text_string(Package, PackageText),
    string_length(PackageText, Length),
    Length >= 3,
    Length =< 255,
    split_string(PackageText, ".", "", Segments),
    Segments = [_, _ | _],
    maplist(valid_package_segment, Segments).

valid_android_vision_policy(Policy) :-
    memberchk(Policy, [disabled, observe_only, confirm_each_action]).

coordinate(Value) :-
    integer(Value),
    between(0, 16384, Value).

bounded_atom(Value, Limit) :-
    atom(Value),
    atom_length(Value, Length),
    Length >= 1,
    Length =< Limit.

bounded_text(Value, Limit) :-
    text_string(Value, Text),
    string_length(Text, Length),
    Length >= 1,
    Length =< Limit,
    string_codes(Text, Codes),
    maplist(safe_text_code, Codes).

safe_text_code(Code) :-
    Code >= 32,
    Code =\= 127.

text_string(Value, Text) :-
    string(Value),
    !,
    Text = Value.
text_string(Value, Text) :-
    atom(Value),
    atom_string(Value, Text).

valid_package_segment(Segment) :-
    string_codes(Segment, [Head | Tail]),
    code_type(Head, alpha),
    maplist(package_code, Tail).

package_code(Code) :-
    code_type(Code, alnum),
    !.
package_code(0'_).
