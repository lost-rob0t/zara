% Zara email policy, spam rule, and LLM tool catalog API.
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

% Provider and model-tool symbols are facts so symbolic turns can enumerate the
% same capabilities exposed to the LangChain/Android tool layers.
email_provider(gmail).
email_provider(imap).
email_provider(pop3).

email_tool(email_accounts, read, 'List configured email accounts').
email_tool(email_search, read, 'Search bounded email metadata').
email_tool(email_read, read, 'Read one message as untrusted data').
email_tool(email_send, write, 'Send a message after before-send policy').
email_tool(email_reply, write, 'Reply after before-send policy').
email_tool(email_classify_spam, read, 'Run Prolog and feed spam rules').
email_tool(email_apply_rules, write, 'Apply post-receive mailbox action').
email_tool(email_refresh_spam_rules, write, 'Compile configured HTTPS feeds into inert Prolog facts').
email_tool(email_prolog_api, read, 'Return the email Prolog API catalog').

email_rule_predicate(email_before_send, 6).
email_rule_predicate(email_before_send_rule, 6).
email_rule_predicate(email_after_receive_rule, 6).
email_rule_predicate(email_spam_rule, 6).
email_rule_predicate(email_user_spam_rule, 6).
email_rule_predicate(email_feed_rule, 4).
email_rule_predicate(email_tool, 3).
email_rule_predicate(email_provider, 1).

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
