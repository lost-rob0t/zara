# Council Ballots — 5 independent voters, 2026-11-26

Each voter read the shared brief, all three design docs, and spot-checked
cited file:line claims in each design's worktree before scoring.
Criteria: S=simplicity M=modernity P=prolog-ADE-reality F=feasibility
I=invariant-safety T=testability E=evidence-quality (1-5 each).

## VOTER 1 (prolog engineering correctness)

```
scores:
  calm-console:   S=5 M=4 P=4 F=5 I=5 T=5 E=5  total=33/35
  ade-workbench:  S=3 M=3 P=5 F=4 I=4 T=5 E=4  total=28/35
  ambient-canvas: S=3 M=5 P=4 F=3 I=3 T=4 E=5  total=27/35
winner: calm-console
runner-up: ade-workbench
```
- calm-console: 11 citations checked, all exact (controller.py:55-62 seam;
  settings.py fence/stale-drop/honest-cancel; prolog_engine.py:327-336,
  418-442; commands.py:118-129; zmq_transport.py:2188-2202, 2516-2532;
  symbolic_runtime.py:120-126; KB 10-13, 20-23; theme.py:142-160;
  test_adaptive_copilot.py:274-298).
- ade-workbench: 9 checked, 8 exact; IMPRECISE: kb/intents.pl line cite
  (dynamic decl :8, facts :24; substance true).
- ambient-canvas: 10 checked, all exact; canvas_delta silently truncates at
  MaxSolutions with no saturation/pagination rule (generation-fencing gap);
  150ms poll + per-event mirror_event writes add continuous novel load on the
  one serialized engine; needs a new host->engine seam that doesn't exist.
- Strongest objection: winner's ADE is summonable, allowlist-capped,
  tasks deferred — least ADE-shaped of the three; ade-workbench delivers the
  visible live engine the brief literally describes.
- Conditions: (1) inspector per §4, allowlist-asserted desktop_ade:* only, no
  pyswip/consult/assertz/retract; (2) three-fence discipline + named tests
  incl. off-Qt-thread for drawer path; (3) record query path + 50-cap in
  .prolog/kb/desktop_prolog_ide.pl same change; (4) no mock task rows — label
  honestly or omit until phase-2 tasks.list; (5) migrate only the two named
  tests, keep 13 fixture state names.

## VOTER 2 (UX simplicity & modernity)

```
scores:
  calm-console:   S=5 M=4 P=4 F=5 I=5 T=5 E=5  total=33/35
  ade-workbench:  S=2 M=3 P=5 F=4 I=4 T=5 E=5  total=28/35
  ambient-canvas: S=4 M=5 P=5 F=2 I=3 T=4 E=4  total=27/35
winner: calm-console
runner-up: ade-workbench
```
- All citations verified across worktrees; one overstatement flagged in
  ambient-canvas (prolog_engine.py:328-334 = serialization, not impossibility).
- Strongest objection: hidden 400px drawer + allowlist-only browsing + phase-2
  tasks can drift into the "debug afterthought" the brief forbids.
- Conditions: (1) drawer as first-class ADE (usable live KB inventory + intent
  catalog), not a settings clone; (2) phase-2 tasks.list in scope or gating
  follow-up issue; (3) off-Qt-thread + cancel-fence tests in
  t/test_desktop_inspector.py before merge; (4) byte-identical gated
  objectNames/stylesheets, 3 new fixtures + 2 declared migrations in one green
  change; (5) zero new color tokens/deps, theme selector assertions same commit.

## VOTER 3 (Qt feasibility & testability)

```
scores:
  calm-console:   S=5 M=4 P=4 F=5 I=5 T=5 E=5  total=33/35
  ade-workbench:  S=3 M=3 P=5 F=4 I=4 T=5 E=4  total=28/35
  ambient-canvas: S=3 M=5 P=4 F=3 I=4 T=4 E=5  total=28/35
winner: calm-console
runner-up: ade-workbench (tie-break PROLOG ADE REALITY 5 vs 4)
```
- calm-console: every citation landed exactly; t/test_copilot_screenshot_ci.py exists.
- ade-workbench: gate cites t/test_prolog_load.py — DOES NOT EXIST; overstates
  reducer handles TurnStarted; submits max_solutions=100 vs cited KB cap 50.
- ambient-canvas: citations exact; only design modifying RuntimeHost; 150ms
  admin-gated poll; unbounded canvas_card facts; two CI-flaky acceptance criteria.
- Strongest objection: summoned drawer is the weakest answer to "not a debug
  afterthought"; one engine lock means a heavy goal stalls live turns with no
  in-scope server-side deadline.
- Conditions: (1) seam-only RunPrologQuery, cap 50 everywhere incl. samples;
  (2) renderer-identity + zero-bridge-command tests; sidebar test migration
  equal-or-stronger; (3) exact settings fence semantics incl. restart/disconnect
  generation bump; (4) new Prolog in modules/desktop_ade.pl via main.pl + KB
  facts same change; (5) additive fixture matrix + REQUIRED_SCREENSHOTS one
  commit, 13 states preserved; (6) existing 15 theme roles only.

## VOTER 4 (invariants & security)

```
scores:
  calm-console:   S=5 M=4 P=4 F=5 I=5 T=5 E=5  total=33/35
  ade-workbench:  S=3 M=3 P=5 F=4 I=4 T=4 E=4  total=27/35
  ambient-canvas: S=3 M=5 P=4 F=3 I=3 T=4 E=5  total=27/35
winner: calm-console
runner-up: ade-workbench (PROLOG ADE REALITY tie-break)
```
- 20+ citations checked for calm-console (all accurate), 16 for ade-workbench
  (one wrong: kb/intents.pl:14), 19 for ambient-canvas (all accurate).
- Ranking note: ade-workbench over ambient-canvas at 27 by ADE REALITY;
  ambient-canvas carries the two largest invariant stressors jointly.
- Strongest objection: winner wins on invariant safety and feasibility, not on
  ADE ambition; tasks surface only partially satisfied at cutover.
- Conditions: (1) all three sections ship at release with live desktop_ade.pl
  projections; (2) tasks gap closed or accepted-scope documented; (3)
  max_solutions=50 per KB fact, no silent raise; (4) tests assert allowlist-only,
  zero mutation; (5) two migrations same change, equal-or-better coverage,
  manifest schema bump documented; (6) zero new color tokens, parity additive only.

## VOTER 5 (product value & migration risk)

```
scores:
  calm-console:   S=5 M=5 P=4 F=5 I=5 T=5 E=5  total=34/35
  ade-workbench:  S=3 M=3 P=5 F=4 I=5 T=5 E=4  total=29/35
  ambient-canvas: S=4 M=5 P=4 F=3 I=4 T=4 E=5  total=29/35
winner: calm-console
runner-up: ade-workbench
```
- Well over 2 citations spot-checked per design; calm-console and
  ambient-canvas exact; ade-workbench one line-number miss (kb/intents.pl:14,
  substance right).
- Strongest objection: product value for the daily non-Prolog user is mostly
  cosmetic; ADE parked behind Ctrl+I repeats the "seven tabs deep" Settings
  discoverability failure; heavy-goal engine-lock stall with deadline deferred.
- Conditions: (1) persistent labeled header affordance + first-run hint;
  (2) palette migration preserves every signal path, both test migrations same
  commits; (3) engine busy from real receipts, caps kept, deadline shipped or
  stall window documented in KB; (4) zero new tokens, +3 additive fixtures,
  manifest bump reviewed; (5) phase-2 tasks.list only as admin-gated ZARA/1
  message with tests, no event-guesswork rows (enforced by test); (6)
  settings.py re-base onto QueryConsoleController behavior-identical, gated by
  t/test_desktop_settings_window.py before old path removed.
