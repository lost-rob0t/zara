% Zara email policy and spam rule API.
%
% This file is portable policy. Credentials and OAuth tokens never belong here.
% User config may add clauses for the *_rule predicates.

:- multifile email_feed_rule/4.
:- multifile email_user_spam_rule/6.
:- multifile email_before_send_rule/6.
:- multifile email_after_receive_rule/6.

:- dynamic email_feed_rule/4.
:- dynamic email_user_spam_rule/6.
:- dynamic email_before_send_rule/6.
:- dynamic email_after_receive_rule/6.

email_contains_ci(Text, Needle) :-
    downcase_atom(Text, TextLower),
    downcase_atom(Needle, NeedleLower),
    sub_atom(TextLower, _, _, _, NeedleLower).

email_spam_rule(Sender, SenderDomain, Subject, Body, Score, Reason) :-
    email_user_spam_rule(Sender, SenderDomain, Subject, Body, Score, Reason).

email_spam_rule(Sender, _, _, _, Score, Source) :-
    email_feed_rule(sender, Sender, Score, Source).

email_spam_rule(_, SenderDomain, _, _, Score, Source) :-
    email_feed_rule(domain, SenderDomain, Score, Source).

email_spam_rule(_, _, Subject, _, Score, Source) :-
    email_feed_rule(subject, Value, Score, Source),
    email_contains_ci(Subject, Value).

email_spam_rule(_, _, _, Body, Score, Source) :-
    email_feed_rule(body, Value, Score, Source),
    email_contains_ci(Body, Value).

email_before_send(Account, To, Subject, Body, deny, Reason) :-
    email_before_send_rule(Account, To, Subject, Body, deny, Reason),
    !.
email_before_send(Account, To, Subject, Body, allow, Reason) :-
    email_before_send_rule(Account, To, Subject, Body, allow, Reason),
    !.
email_before_send(_, _, _, _, allow, default_allow).

% Examples for user rules files:
%
% email_before_send_rule(work, To, _, _, deny, external_recipient) :-
%     \+ sub_atom(To, _, _, 0, '@example.com').
%
% email_after_receive_rule(_, _, _, Subject, _, trash) :-
%     email_contains_ci(Subject, '[known junk]').
%
% email_user_spam_rule(_, 'bad.example', _, _, 80, local_domain_rule).
