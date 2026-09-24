# ZARA-VERIFY/1 — exact-source code verification

Status: implementation contract, version 1.0.0. This is a verification-policy and
workflow gate, not a mathematical proof of arbitrary Zara programs and not merge
authority. Parent architecture: ZARA-EXPERT/1, existing repository/Nix/Android
acceptance, and the trusted-verifier boundary in Zara issue #662.

## Authority

Prolog determines mandatory evidence. The host observes source and executes fixed
registered checks. Core's existing `ExpertRegistry` activates `zara:verifier` and
owns dispatch, identity, cancellation/generation fences and the zero-model budget.
OpenCode requests verification; it cannot select an easier gate set, submit a
passing receipt, choose a shell command, or turn a plan into an assertion.

The policy module is `verification/zara_verify.pl`. The static command catalog is
`spec.json`. `verification/zara_verify_runner.py` is a bounded effect adapter,
not another policy engine. `verification/zara_verifier_expert.py` registers with
Core; it does not implement a competing expert registry.

The registered expert operations are `verify.plan`, `verify.explain` and
`verify.assert`. All have empty input schemas. Observations are injected by the
trusted host, never carried in model tool arguments. A successful plan means only
that planning succeeded. Only `verify.assert` with `data.verified=true` can satisfy
the OpenCode completion gate. Every result has integer `model_calls=0` and
`provider_calls=0`; missing Core or Prolog is BLOCKED, never a model fallback.

## Source and attempt identity

Each run binds a random 128-bit run ID, bounded OpenCode session ID, exact Git HEAD,
resolved base and merge-base, canonical workspace path, worktree digest, and
verifier-policy digest. Source hashing reads contents, not timestamps or file
lengths, and includes tracked files, non-ignored untracked files, executable bits,
symlink text, deletions, index state and status. Rename detection is disabled for
impact analysis so both old and new paths retain their required checks.

Gitlinks fail closed: submodules need a dedicated collector. Non-UTF8, control,
absolute and traversal paths are rejected. Ignored files, external symlink targets,
external dependencies and hostile same-UID mutations are not fully captured by this
fingerprint. Run reviewed code in an appropriately isolated environment; Nix/package
checks and independent CI evidence remain necessary. A clean Git status alone is
never a verification result.

A receipt expires ten minutes after completion. Changing HEAD, base, index,
source contents, policy bytes or workspace invalidates it. OpenCode also maintains
a process-local mutation epoch: write-then-revert (ABA), unknown/effectful tools,
and subagent file edits invalidate earlier receipts across sessions. Receipts
cannot be reused in a different OpenCode session. Restart loses receipt authority.

## Deterministic impact plan

Every change requires the verifier's own tests, full repository gate, existing
formal gate, Nix checks and package build. Additional evidence is cumulative:

| Changed surface | Additional mandatory evidence |
| --- | --- |
| Python | Canonical coverage ratchet; deep regression seeds 1 and 31337 |
| Prolog or expert contracts | Existing expert/Prolog conformance suite |
| Android, including Code/IPC | Android/Wear build/runtime gate and exact-source UI evidence |
| Desktop UI | Exact-source desktop rendered-state evidence |
| Auth, permissions, security, secrets or IPC | Runtime security suite and independent review |
| Verifier, OpenCode policy or workflow | Independent review; never self-approve evaluator changes |
| Version/release paths | Signing, artifact/source and release provenance evidence |
| Unknown scope | Explicit independent scope review |

The host schedules the selected dependency DAG without letting a failed parent run
its dependent child. Duplicate IDs, missing dependencies, cycles and unknown gates
are blockers. No retry converts an earlier failed attempt into success; a new run
has a new identity and must collect its own evidence.

## Observations and verdicts

Local evidence records contain gate ID, run/source digest binding, terminal state,
observed exit status, duration, bounded artifact path/bytes and SHA-256. Repository
JUnit evidence counts actual test cases, not self-reported summary attributes.
Empty, all-skipped, malformed, DTD/entity-containing or failed test results cannot
pass. A suite with real passing tests and optional skipped cases is not described
as proving those skipped cases. Required gates themselves cannot be skipped.

`verified` requires every planned gate exactly once, current source, terminal
passed state, integer exit code zero, and matching artifact/attempt identity.
Missing, pending, cancelled, stale, duplicate, unknown and unavailable evidence is
not green. `failed` identifies observed test failures; `blocked` means the complete
claim cannot be established. Reasons and evidence remain inspectable, not replaced
with an opaque confidence score. All local verdicts have `merge_authorized=false`.

The runner writes preliminary `observations.json` under the Git metadata directory.
The CLI then invokes Core's verifier expert before returning its final report.
Schema: `report.schema.json`. JSON validity is not provenance. Never load a user-
written report file and treat its `verified` field as host authority.

## Execution budgets and lifecycle

Source limits: 20,000 files, 16 MiB per file, 256 MiB total, 4,096 changed paths.
Policy input/output and each command log: 2 MiB. Catalog: at most 64 gates, 32 argv
items and 4,096 characters per argument. Per-command timeout: at most 3,600 seconds;
whole-run deadline: 7,200 seconds. Worktree observation and Prolog calls also time
out. Bounds are intentionally conservative; exceeding one blocks the claim.

Commands use argv, not interpolated shell strings. Each runs in a separate POSIX
process group with private HOME/XDG state and a credential-stripped environment.
Timeout, output overflow, cancellation and normal exit clean up descendants. These
are resource/lifecycle controls, not a sandbox against malicious native code.
OpenCode admits one verification run at a time and retains at most 64 session
receipts. A new mutation during a run invalidates its result. Cancellation cannot
mint or revive a receipt.

## OpenCode integration

Local plugin: `.opencode/plugins/zara-verify.js`. OpenCode's local-plugin loader
loads this file for this repository. The tool is `zara_verify` with only
`operation=plan|run|status`; `/zara-verify` delegates to the `zara-verifier` subagent.
The subagent has verification/read permissions, not mutation authority.

Run OpenCode from the repository root in the Nix development environment, with
Node and the normal OpenCode plugin SDK available. The fixed OpenCode base is
`origin/release/0.3.x`; a missing or shallow base blocks instead of silently reducing
coverage. Operator CLI usage:

```sh
nix develop -c python verification/zara_verify_runner.py plan --base origin/release/0.3.x
nix develop -c python verification/zara_verify_runner.py run --base origin/release/0.3.x
```

The plugin verifies evaluator bytes against its startup pin before/after runs and
assertions. A changed evaluator needs a reviewed restart; continuing with stale
plugin code is blocked. A completed `todowrite` is rejected without a current
receipt. The final-text hook replaces unverified completion text with a BLOCKED
notice. It does not hide text already streamed, prevent all shell-based publication,
or survive a user disabling/editing the plugin. The final-text hook is experimental
upstream and requires compatibility testing against the installed OpenCode version.
Repository protection, trusted CI and independent reviewers remain the real merge
boundary. No claim is made that this PR configures GitHub branch protection.

## Integration boundaries still requiring evidence

Version 1 reserves external gates for Android/desktop screenshots, independent
review, scope review and release provenance. **This slice does not yet implement a
trusted external-attestation collector.** Those gates stay missing/BLOCKED instead
of accepting caller-authored JSON or treating emulator output as physical-device
acceptance. Do not disable these gates to unblock completion.

The catalog consumes the canonical `scripts/test-coverage.sh` from the retained
release coverage implementation. The coverage work is integrated on `release/0.3.x`;
this verifier consumes that authority rather than creating a competing metric or
lowering the +2 percentage-point minimum / +10 target described by the operator.

The CLI uses a real standalone Core registry; embedding applications call
`register_verifier_expert(existing_registry, host_observations, policy_root)`.
Automatic registration into every running Zara app and an isolated same-UID-safe
verifier broker are not supplied by this slice. Same-process/private Python objects
are not unforgeable capabilities (issue #662).

## Required verification of this implementation

`bash scripts/test-zara-verify.sh` requires Python tests, the actual Core registry
integration, SWI-Prolog plunit and Node hook tests. Missing tools are errors, not
skips. The dedicated `Zara Verifier Contract` workflow runs this at the exact head.
Existing repository, Android, Nix and live-evidence gates remain required separately.
A contract-suite pass does not attest the entire product or a device installation.

Adversarial fixtures cover same-length edits, untracked/deleted/renamed files,
symlinks, source changes during tests, cross-session and stale evidence, ABA edits,
unknown/duplicate/missing gates, false/boolean numeric values, process failure and
flood/timeout, cancelled runs, empty JUnit, unsafe paths, model-supplied verdicts,
Core dispatch and mutation after receipt delivery.

## Upstream references

- OpenCode plugins: https://opencode.ai/docs/plugins/
- OpenCode custom tools: https://opencode.ai/docs/custom-tools/
- Hook contract inspected: anomalyco/opencode `packages/plugin/src/index.ts`,
  blob `edfa0139dfcaf0e877ab906fabe8e0527afc3915`.
- Tool context inspected: `packages/plugin/src/tool.ts`,
  blob `9c6daa34d04ab61dd206360c1f187cfd694e668a`.
- Zara trust boundary: https://github.com/lost-rob0t/zara/issues/662
