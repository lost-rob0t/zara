:- module(output_policy, [evaluate_json/3, advice/5, text_matches/2]).

:- use_module(library(apply)).
:- use_module(library(error)).
:- use_module(library(http/json)).
:- use_module(library(lists)).
:- use_module(library(solution_sequences)).
:- use_module(library(time)).

:- multifile advice/5.
:- dynamic advice/5.

 evaluate_json(Text, ContextJSON, JSON) :-
    catch(
        ( call_with_time_limit(1,
              ( call_with_inference_limit(evaluate(Text, ContextJSON, Entries), 100000, Limit),
                ( Limit == inference_limit_exceeded -> throw(inference_limit_exceeded) ; true ) )),
          Payload = _{status:"ok", advice:Entries} ),
        _,
        Payload = _{status:"error", error:"evaluation_failed"}),
    atom_json_dict(JSON, Payload, [as(string)]).

evaluate(Text, ContextJSON, Entries) :-
    must_be(string, Text), string_length(Text, Size),
    ( Size =< 131072 -> true ; throw(resource_error(policy_text)) ),
    atom_json_dict(ContextJSON, Context, []), must_be(dict, Context),
    once(findnsols(33, match(Id, Priority, Message),
        advice(Id, Priority, Context, Text, Message), Matches)),
    length(Matches, Count),
    ( Count =< 32 -> true ; throw(resource_error(policy_advice)) ),
    maplist(valid_match, Matches, Keyed),
    keysort(Keyed, Sorted),
    pairs_entries(Sorted, Entries),
    maplist(entry_id, Entries, Ids), sort(Ids, UniqueIds),
    ( same_length(Ids, UniqueIds) -> true ; throw(domain_error(unique_policy_ids, [])) ).

valid_match(match(Id, Priority, Message), Priority-Id-Entry) :-
    must_be(atom, Id), atom_length(Id, IdLength),
    ( between(1, 64, IdLength) -> true ; throw(domain_error(policy_id, [])) ),
    must_be(integer, Priority),
    ( between(-100000, 100000, Priority) -> true ; throw(domain_error(policy_priority, [])) ),
    must_be(string, Message), string_length(Message, MessageLength),
    ( between(1, 2048, MessageLength) -> true ; throw(domain_error(policy_message, [])) ),
    atom_string(Id, IdString),
    Entry = _{id:IdString, priority:Priority, message:Message}.

pairs_entries([], []).
pairs_entries([_-Entry|Rest], [Entry|Entries]) :- pairs_entries(Rest, Entries).
entry_id(Entry, Id) :- Id = Entry.id.

text_matches(Matcher, Text) :-
    must_be(string, Text), must_be(acyclic, Matcher),
    matches(Matcher, Text, 0).

matches(_, _, Depth) :- Depth > 16, !, throw(resource_error(policy_match_depth)).
matches(exact(Pattern), Text, _) :- !, must_be(string, Pattern), Text == Pattern.
matches(contains(Pattern), Text, _) :- !,
    must_be(string, Pattern), once(sub_string(Text, _, _, _, Pattern)).
matches(icontains(Pattern), Text, _) :- !,
    must_be(string, Pattern), string_lower(Pattern, LowerPattern), string_lower(Text, LowerText),
    once(sub_string(LowerText, _, _, _, LowerPattern)).
matches(not(Matcher), Text, Depth) :- !,
    Next is Depth + 1, \+ matches(Matcher, Text, Next).
matches(all(Matchers), Text, Depth) :- !,
    bounded_matchers(Matchers), Next is Depth + 1,
    maplist(matches_at(Text, Next), Matchers).
matches(any(Matchers), Text, Depth) :- !,
    bounded_matchers(Matchers), Next is Depth + 1,
    once((member(Matcher, Matchers), matches(Matcher, Text, Next))).
matches(_, _, _) :- throw(domain_error(policy_matcher, [])).

matches_at(Text, Depth, Matcher) :- matches(Matcher, Text, Depth).
bounded_matchers(Matchers) :-
    must_be(list, Matchers), length(Matchers, Count),
    ( between(1, 32, Count) -> true ; throw(resource_error(policy_matchers)) ).
