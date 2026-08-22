# Zara RAGE

Use this skill when the user explicitly asks for a RAGE run in Zara.

RAGE is an issue-driven research-to-merge workflow. The GitHub Issues list is the queue; the repo is not permitted to hallucinate its own target because a README looked lonely.

## 1. Consume the issue queue

- Read roadmap issue #1 plus the relevant epic and open child issues.
- Select the first open issue whose dependencies are satisfied and whose ordering/priority makes it eligible.
- Treat an epic as a queue/container unless it explicitly defines atomic implementation work.
- Record the consumed issue and exact starting commit in `rage/<work-log>.org` before research, design, or implementation.
- One RAGE iteration works one consumed issue.

For the daemon epic (#127), current child ordering is #128 -> #129 -> #130 -> #131, then #132/#133 as dependencies allow, with #134 as the release proof gate.

## 2. Research before design

Research must be capable of disproving the issue's current design. At minimum inspect:

- current source and tests at the recorded start commit;
- issue/epic acceptance criteria and dependencies;
- relevant prior PRs/research without treating them as scripture;
- authoritative upstream documentation for libraries/protocols/platforms;
- known failure modes, resource bounds, security properties, lifecycle behavior, and portability constraints;
- serious alternatives and why they lose or win here.

Write evidence and conclusions into the Org work log and, for substantial work, a dedicated Org research artifact under `rage/`.

## 3. Design from evidence

The design must record:

- problem boundary and non-goals;
- selected architecture;
- rejected alternatives with reasons;
- invariants and ownership rules;
- state machine/lifecycle where relevant;
- failure/threat analysis;
- compatibility/migration constraints;
- acceptance criteria mapped to tests;
- exact local and CI gate.

If implementation discovers a design-level contradiction, update the design/log before continuing. Do not hide architecture changes inside patches.

## 4. Implement

Implement only the consumed issue's slice. Reuse Zara's existing runtime boundaries rather than spawning parallel stacks.

Permanent Zara constraints include:

- ordinary SWI-Prolog stays part of Zara;
- do not add or revive a Prolog-RLM runtime/backend/dependency;
- Nix owns dependencies;
- preserve canonical RuntimeHost/TurnCoordinator/event-command boundaries unless the researched design explicitly and convincingly changes them;
- tests accompany behavior changes.

## 5. Gate exact head

Run focused tests first, then the full repository gate:

```sh
nix develop -c bash scripts/test-all.sh
nix flake check
nix build
```

Also run every issue-specific security/lifecycle/protocol/soak test required by the consumed issue. After opening/pushing the PR, verify GitHub Actions for the exact candidate SHA. Older green runs are stale.

## 6. Merge or trash the attempt

- If the complete exact-head gate passes, merge the PR, close/update the consumed issue, and append the merge SHA to the work log.
- If a failure shows the architecture/design is wrong, record the failure, preserve the evidence, discard the failed implementation attempt, and begin a new RAGE iteration from research/design.
- Ordinary local coding mistakes can be fixed within the same iteration. A disproven design cannot be patched forever merely to manufacture green CI.

## 7. Continue only by consuming the next issue

After a successful issue, re-read the GitHub issue queue and select the next eligible issue. Do not silently jump ahead, merge an epic wholesale, or substitute a prose TODO for the queue.
