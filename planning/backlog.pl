% planning/backlog.pl -- executable mirror of the GitHub work queue.
% NOT loaded by the Zara runtime; GitHub issues remain authoritative.
% Update facts by hand when issue/PR state changes (YAGNI: no sync tooling).
%
% Usage:
%   nix develop -c swipl -s planning/backlog.pl -g "next(I), writeln(I)" -t halt.
%   nix develop -c swipl -s planning/backlog.pl  % then: next(I). queue(Q). blockers(122, Bs).

% Closed issues referenced by dependency edges.
closed(17). closed(128). closed(129). closed(130). closed(131). closed(132).
closed(133). closed(191). closed(209).

% Open issues: issue(Id, Priority, Status, ShortTitle).
issue(134, p0, open, "Daemon release gate (Voice replacement proof)").
issue(139, p1, open, "Implement first-class Zara daemon via RAGE").
issue(124, p0, open, "Full embedded async Prolog LLM client").
issue(216, p1, open, "Transcript normalization contract").
issue(217, p1, open, "Local S1-mini normalizer backend").
issue(218, p1, open, "Normalizer config, status, diagnostics").
issue(219, p1, open, "Wire normalization into voice/dictation/routing").
issue(220, p1, open, "Normalization release gate").
issue(122, p0, open, "Bounded utterance rewriter before intent resolution").
issue(28,  p2, open, "ZARA-027 stream LLM output").
issue(29,  p2, open, "ZARA-028 stream phrase TTS").
issue(30,  p2, open, "ZARA-029 warm startup optimization").
issue(31,  p2, open, "ZARA-030 duplex soak release gate").
issue(51,  p1, open, "ZARA-031 context management + skills").
issue(149, p1, open, "Publish zara-server OCI image to GHCR").
issue(180, p1, open, "Container volume/non-root hardening").
issue(181, p1, open, "Container lifecycle gates").
issue(182, p2, open, "Container model/cache policy + examples").
issue(183, p1, open, "Container security gate").
issue(154, p1, open, "IntentFrame research + contract freeze").
issue(155, p1, open, "Typed slots + clarification dialogue").
issue(156, p1, open, "Prolog IntentFrame adaptation + corpus").
issue(157, p1, open, "Capability reasoning + ExecutionPlan").
issue(158, p1, open, "api_service providers + Prolog config split").
issue(159, p1, open, "ZARA/1 capability advertisement").
issue(160, p1, open, "RuntimeHost semantic routing").
issue(161, p1, open, "Semantic routing release gate").
issue(162, p1, open, "Programmable command persistence").
issue(163, p1, open, "Compile commands to IntentFrame plans").
issue(164, p1, open, "Command authoring dialogue").
issue(165, p1, open, "Parameterized commands + hot reload").
issue(166, p1, open, "Voice fixture manifest").
issue(167, p1, open, "zara-record-voice-fixtures tool").
issue(168, p1, open, "Real speech corpus regression").
issue(169, p1, open, "Live-voice command gate").
issue(170, p2, open, "Android feasibility bakeoff").
issue(171, p2, open, "Android project skeleton + CI").
issue(172, p2, open, "Portable shared Prolog core").
issue(173, p2, open, "Android ZARA/1 enrollment").
issue(174, p2, open, "Android capability registry").
issue(175, p2, open, "Android mic/playback/barge-in").
issue(176, p2, open, "Android degraded Prolog behavior").
issue(177, p2, open, "Cross-platform command parity").
issue(178, p2, open, "Android adversarial gate").
issue(179, p2, open, "Android real-device gate").
issue(195, p0, open, "Samsung assistant research gate").
issue(196, p1, open, "Android Assistant role + voice service").
issue(197, p1, open, "Compose app shell").
issue(198, p1, open, "Hot-reloadable Prolog authority").
issue(199, p1, open, "Typed app/browser/YouTube adapters").
issue(200, p1, open, "Timer/alarm/calendar adapters").
issue(201, p1, open, "Termux named-action adapter").
issue(202, p1, open, "SmartThings OAuth provider").
issue(203, p1, open, "Samsung lifecycle hardening").
issue(204, p1, open, "Cross-component command gate").
issue(205, p0, open, "Adversarial Android/Samsung matrix").
issue(206, p0, open, "Samsung hardware release gate").
issue(87,  p1, open, "Wayland/X11 global shortcuts").
issue(88,  p1, open, "Desktop context attachments + permissions").
issue(89,  p1, open, "Tool execution/approvals in chat").
issue(90,  p1, open, "Voice states + desktop notifications").
issue(91,  p2, open, "Command palette + capability registry").
issue(92,  p2, open, "Desktop settings + diagnostics").
issue(93,  p2, open, "Prolog reasoning inspector").
issue(94,  p2, open, "Pets + event stream/tray").
issue(95,  p3, open, "Desktop packaging + lifecycle").
issue(56,  p2, open, "Local Recall visual-context intent").

% Roadmap order: phase(N, OrderedIssueIds). Rank = N*100 + index.
phase(0, [17]).                                    % regression: always first
phase(1, [134, 139]).                              % daemon: consumed queue tail
phase(2, [28, 29, 30, 31, 51]).                    % duplex voice + release gate + context
phase(3, [124, 216, 217, 218, 219, 220, 122]).     % model + normalization chain
phase(4, [149, 180, 181, 182, 183]).               % container distribution
phase(5, [154, 155, 156, 157, 158, 159, 160, 161]).% semantic intents
phase(6, [162, 163, 164, 165, 166, 167, 168, 169]).% programmable commands
phase(7, [170, 171, 172, 173, 174, 175, 176, 177, 178, 179]). % android client
phase(8, [195, 196, 197, 198, 199, 200, 201, 202, 203, 204, 205, 206]). % samsung
phase(9, [87, 88, 89, 90, 91, 92, 93, 94, 95, 56]).% desktop + recall

% Dependency edges (only edges that gate eligibility).
depends_on(122, 124).
depends_on(122, 219).
depends_on(134, 191).
depends_on(134, 209).
depends_on(134, 31).
depends_on(139, 134).
depends_on(217, 216).
depends_on(218, 217).
depends_on(219, 218).
depends_on(220, 219).
depends_on(29, 28).
depends_on(30, 29).
depends_on(31, 30).
depends_on(180, 149).
depends_on(181, 180).
depends_on(182, 181).
depends_on(183, 182).
depends_on(183, 161).
depends_on(155, 154).
depends_on(156, 155).
depends_on(157, 156).
depends_on(158, 157).
depends_on(159, 158).
depends_on(160, 159).
depends_on(161, 160).
depends_on(163, 162).
depends_on(164, 163).
depends_on(165, 164).
depends_on(166, 165).
depends_on(167, 166).
depends_on(168, 167).
depends_on(169, 168).
depends_on(171, 170).
depends_on(172, 171).
depends_on(173, 172).
depends_on(174, 173).
depends_on(175, 174).
depends_on(176, 175).
depends_on(177, 176).
depends_on(178, 177).
depends_on(179, 178).
depends_on(196, 195).
depends_on(197, 196).
depends_on(198, 197).
depends_on(199, 198).
depends_on(200, 199).
depends_on(201, 200).
depends_on(202, 201).
depends_on(203, 202).
depends_on(204, 203).
depends_on(205, 204).
depends_on(205, 179).
depends_on(206, 205).

% Epics: epic_children(EpicId, ChildIds).
epic_children(127, [128,129,130,131,132,133,134]).
epic_children(150, [154,155,156,157,158,159,160,161]).
epic_children(151, [162,163,164,165,166,167,168,169]).
epic_children(152, [170,171,172,173,174,175,176,177,178,179]).
epic_children(153, [149,180,181,182,183]).
epic_children(194, [195,196,197,198,199,200,201,202,203,204,205,206]).
epic_children(215, [216,217,218,219,220]).

% Open PRs: pr(Id, Issue, Status, CiDate, Note). CiDate = none | Y-M-D.
pr(222, none, open,  none,        parked_design_direction_decision_needed).
pr(223, 127,  open,  none,        parked_rebase_zara1_contract_reconciliation).
pr(138, none, open,  none,        parked_rebase_post_daemon_factcheck).

master_last_merge(2026-08-29).

% ---- Rules ----
open_issue(I) :- issue(I, _, open, _).
done(I) :- issue(I, _, done, _).
done(I) :- closed(I).

satisfied(I) :- \+ (depends_on(I, D), \+ done(D)).
eligible(I)  :- open_issue(I), satisfied(I).
blocked(I)   :- open_issue(I), \+ satisfied(I).
waiting_on(I, D) :- depends_on(I, D), \+ done(D).
blockers(I, Ds) :- setof(D, waiting_on(I, D), Ds).

phase_rank(I, R) :- phase(P, L), nth0(Idx, L, I), R is P * 100 + Idx.
next(I) :- setof(R-I, (eligible(I), phase_rank(I, R)), [_-I|_]).
queue(Q) :- setof(R-I, (eligible(I), phase_rank(I, R)), Q).

epic_progress(E, DoneCount, Total) :-
    epic_children(E, Cs),
    length(Cs, Total),
    include([C]>>done(C), Cs, Ds),
    length(Ds, DoneCount).

open_pr(P) :- pr(P, _, open, _, _).
stale_ci(P) :-
    pr(P, _, _, D, _),
    D \== none,
    master_last_merge(M),
    D @< M.
missing_ci(P) :- open_pr(P), pr(P, _, _, none, _).