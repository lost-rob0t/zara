:- module(gf_frontend, [gf_canonical_turn/4, gf_reply_tree/2]).

:- use_module('../modules/symbolic_dialogue_turn', [dialogue_turn/4]).

% The canonical conversation owner supplies context. GF supplies text, never
% an executable Prolog goal. Do not call this with unparsed user text as a
% substitute for GF parsing, and do not use it to dispatch device actions.
gf_canonical_turn(Canonical, State, Context, Result) :-
    atom(Canonical),
    atom_length(Canonical, Length),
    Length > 0,
    Length =< 512,
    symbolic_dialogue_turn:dialogue_turn(Canonical, State, Context, Turn),
    Turn = turn(_Frames, Act, _NextContext),
    gf_reply_tree(Act, Tree),
    Result = gf_turn(Turn, Tree,
        evidence(frontend('gf-pilot/v1'), model_calls(0), provider_calls(0))).

gf_reply_tree(greeting, 'GreetingReply') :- !.
gf_reply_tree(answer(expert, _, evidence('builtin-identity/v1')), 'IdentityReply') :- !.
gf_reply_tree(help, 'HelpReply') :- !.
gf_reply_tree(acknowledgement(thanks), 'ThanksReply') :- !.
gf_reply_tree(acknowledgement(acknowledged), 'AcknowledgedReply') :- !.
gf_reply_tree(cancelled, 'CancelledReply') :- !.
gf_reply_tree(clarify(slot(duration)), 'DurationReply') :- !.
gf_reply_tree(clarify(slot(target)), 'TargetReply') :- !.
gf_reply_tree(dispatch_required(_), 'PendingReply') :- !.
gf_reply_tree(_, 'UnsupportedReply').
