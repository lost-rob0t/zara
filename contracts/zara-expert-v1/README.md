# ZARA-EXPERT/1 — expert descriptor, activation and invocation contract

Owner: issue #1233 under epic #1232. Activation semantics owner: #897. Entry
points owner: #1156. Execution isolation boundary: #663. Canonical symbol
identity: #986. This contract is a versioned facade over existing expert
implementations; it is not a second registry, expert engine, or network
protocol.

## Authority rules

1. Discovery grants nothing. `expert.list`, `expert.describe` and
   `expert.match` are pure projections: no package downloads, process starts,
   installation hooks, or host effects.
2. Activation is not installation, enablement, trust, or authorization.
   Activation of an installed compatible expert emits an activation receipt
   bound to an authenticated principal, workspace, exact expert
   version/digest, and registry/runtime generations.
3. A symbolic expert (`reasoning_kind = symbolic`) performs no model
   inference anywhere in its admission or execution. Symbolic does not mean
   network-free: egress and external services are separate
   `possible_effects` constraints.
4. Payload fields never choose principal or node identity; the carrier
   authenticates and the registry binds handles server-side.
5. This revision makes no execution-isolation claims. Same-interpreter
   handler dispatch inherits the current Zara plugin threat model until the
   #663 boundary lands.

## Operation vocabulary

`expert.list`, `expert.describe`, `expert.match`, `expert.activate`,
`expert.status`, `expert.invoke`, `expert.explain`, `expert.cancel`,
`expert.deactivate`. Domain operation ids (for example `route.diagnose`,
`adb.diagnose`, `rlm.query`) are declared per descriptor and validated
against the declared input field schemas at admission; undeclared operations
fail with `unsupported_operation` instead of fabricating behavior.

## Lifecycle

`inactive -> activating -> active -> draining -> inactive`, with
`activating -> failed`, `active -> failed`, and `active -> unavailable` on
backend loss; `unavailable -> active` is the only backend-recovery edge.
Authorization revocation immediately blocks new admitted effects. Reload
validates a staged registration generation before publication and retains
the last-good registration on failure.

## Request/result envelopes

Requests carry `protocol`, `request_id`, `operation`, `activation_id`,
`expert_id`, `expert_operation`, `expected_registry_generation`,
`expected_runtime_generation`, `input`, `limits` (`timeout_ms`,
`max_results`, `max_output_bytes`, `max_model_calls`), and an optional
idempotency key. Limits are validated against host ceilings before
admission; `max_model_calls = 0` with a `model_inference` effect fails with
`budget_exceeded` before dispatch. Results carry resolved identities,
versions and generations, a domain verdict, bounded typed data, evidence
refs, usage, and effect receipts. Execution completion is distinct from the
domain verdict: a completed invocation may still report verdict `unknown`.

Idempotency keys bind to principal + expert operation + normalized input +
relevant generations. A retry returns prior durable state or an explicit
unknown state; the same key with changed input is a conflict. Cancellation
fences future dispatch and stale responses; it never reverses committed
effects.

## Verdicts and errors

Verdicts (from lost-rob0t/prolog-rlm#376, adopted verbatim):
`succeeded`, `failed`, `unknown`, `blocked`, `unsupported`, `cancelled`,
`error`. Missing evidence is `unknown`, never `failed`, and never prose.

Closed error codes: `invalid_input`, `ambiguity`, `unsupported_operation`,
`unsupported_backend`, `incompatible_protocol`, `denied`,
`approval_required`, `stale_generation`, `unavailable`, `deadline_exceeded`,
`budget_exceeded`, `cancelled`, `interrupted`, `unknown_external_outcome`.

## Compatibility

Unknown protocol majors fail closed (`ZARA-EXPERT/2` in the shared fixture).
Within major `/1`, additive descriptor fields and operations are allowed
under the schema rules; unknown enum values or keys fail closed in every
codec. The Python authority is `zara/experts.py`; the portable Prolog
projection is `modules/expert_contract.pl`; the Android mirror is
`ai.zara.app.expert.ExpertContract`. All three languages consume the
dependency-free `descriptors.tsv` fixture in conformance tests, following
the ZARA-RUNTIME/1 twin-test pattern.

## Fixture

`descriptors.tsv` rows: `zara:expert/todo` (symbolic, ready),
`zara:expert/android-troubleshooting` (hybrid, available),
`zara:expert/prolog-rlm` (service, absent — optional-runtime precedent),
`zara:expert/future` (incompatible major 2). `registry_generation` is
runtime state and deliberately not a TSV column.
