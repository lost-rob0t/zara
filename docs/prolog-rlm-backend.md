# Experimental Prolog-RLM agent backend

Zara can run conversational turns through an experimental Prolog-RLM backend.
LangGraph remains the default and existing configurations continue to select it.

## Integration contract

This branch pins `lost-rob0t/prolog-rlm` at:

```text
4cdc9854a510a2d07b559e9ae34491d43d81301a
```

The Prolog-RLM runtime does **not** execute inside Zara's long-lived PySWIP
runtime. Each Prolog-RLM turn launches a separate SWI-Prolog sidecar process and
uses JSON Lines over stdin/stdout. This prevents a long model/RLM operation from
holding Zara's process-wide PySWIP lock.

The first integration uses one sidecar per request. That is intentionally
boring: request identity, cancellation, crash isolation, and concurrent turns
remain process-local. A persistent sidecar can replace this later without
changing the `RuntimeBackend` boundary.

## Enable it

Run Zara through the Nix development shell or packaged Nix wrappers so the
pinned Prolog-RLM source path is injected reproducibly.

Add this to Zara's TOML configuration:

```toml
[agent]
backend = "prolog_rlm"

[prolog_rlm]
mode = "rlm"
model = "openrouter/free"
max_recursion_depth = 0
request_timeout = 45.0
```

Set the provider credential in the environment:

```sh
export OPENROUTER_API_KEY='...'
nix develop -c zara --agent
```

Do not put the API key in Zara's TOML. The sidecar passes only
`env('OPENROUTER_API_KEY')` into Prolog-RLM's provider contract.

To return to the existing backend:

```toml
[agent]
backend = "langgraph"
```

Removing `agent.backend` also selects LangGraph.

## Process and protocol boundary

The runtime path is:

```text
RuntimeHost
  -> RuntimeBackend
  -> PrologRLMBackend
  -> one SWI-Prolog process per turn
  -> modules/rlm_sidecar.pl
  -> pinned prolog-rlm
  -> OpenRouter
```

The Python/Prolog wire protocol is JSON Lines. Important messages are:

```json
{"type":"invoke","request_id":"...","turn_id":"...","query":"...","options":{}}
{"type":"started","request_id":"..."}
{"type":"model_started","request_id":"...","role":"root_planner"}
{"type":"tool_call","request_id":"...","tool_call_id":"...","tool":"calculator","args":{}}
{"type":"tool_result","request_id":"...","tool_call_id":"...","status":"ok","value":"..."}
{"type":"cancel","request_id":"..."}
{"type":"completed","request_id":"...","result":{}}
{"type":"failed","request_id":"...","error":{}}
{"type":"cancelled","request_id":"...","error":{}}
```

Human-readable process output is never scraped as application data.

## Cancellation

Zara's canonical `turn_id` maps to one generated RLM `request_id` and one child
process. A cancellation writes a structured `cancel` message. The sidecar calls
`rlm_cancel/1` on the request's cancellation token. If the worker does not exit
inside the cancellation grace period, Zara terminates and finally kills only
that request's sidecar.

This means cancelling one concurrent Prolog-RLM turn does not cancel another.

## Tool bridge

The first bridge exposes exactly one existing Zara capability: `calculator`.

The RLM runtime registers it as `tool(calculator)`. Arguments are schema checked
in Prolog and checked again in Python before Zara's existing calculator tool is
invoked. The bridge bounds expression length, AST size/depth, exponent size, and
output bytes.

The backend does **not** expose:

- arbitrary Python;
- shell commands;
- unrestricted Prolog `call/1`;
- the complete Zara tool registry.

## Runtime events and traces

Zara continues to emit its normal turn/agent completion, failure, and
cancellation events through `RuntimeHost`. Calculator execution emits the
existing provider-neutral tool lifecycle events.

The pinned `rlm_completion/4` boundary does not provide a live callback for
every nested plan-model or recursive-child transition. The sidecar therefore
does not invent those events. Structured model trajectory, recursion statistics,
usage metadata, and transition counts are returned in `RuntimeTurnResult.metadata`
after the RLM operation completes.

Provider token or cost fields remain unknown when the provider does not report
them.

## Tests

Deterministic backend/process tests:

```sh
nix develop -c python -m pytest -q t/test_prolog_rlm_backend.py
```

Full Zara regression gate:

```sh
nix develop -c bash scripts/test-all.sh
```

Sidecar module-load probe:

```sh
nix develop -c swipl -q -f none -s modules/rlm_sidecar.pl -- \
  --probe "$ZARA_PROLOG_RLM_ROOT"
```

The probe must report the exact integration revision.

## Live OpenRouter acceptance

The live test is opt-in and has no fake-provider fallback:

```sh
export OPENROUTER_API_KEY='...'
export ZARA_RLM_LIVE=1
nix develop -c python scripts/test-prolog-rlm-live.py
```

It performs two real fixtures:

1. Zara -> RLM planner -> direct OpenRouter model step -> Zara result.
2. Zara -> RLM planner -> capability-gated Zara calculator -> one depth-1 RLM
   child -> OpenRouter -> Zara result.

Prompts and generation budgets are intentionally small.

## Initial benchmark

Run the fixed fixture harness with:

```sh
nix develop -c python scripts/benchmark-agent-backends.py --backend both
```

It records success/failure, deterministic fixture correctness, wall time, tool
call count, and model/token/cost metadata when those values are actually
reported.

A comparison is meaningful only when the two configured provider/model paths
are comparable. The harness does not claim Prolog-RLM is faster, cheaper, or
more accurate.

## Current limitations

- Experimental; not enabled by default.
- One SWI process per request rather than a persistent sidecar.
- Conversation attachments are not wired into the RLM context yet.
- Zara memory is unchanged and is not replaced by RLM artifacts.
- Only the calculator bridge is exposed.
- Ordinary turns default to recursion depth 0; depth 1 must be explicitly
  enabled.
- Nested model/recursive transitions are reported from structured trajectory
  data after completion rather than as fake live progress.
- Durable artifact handoff is not part of the initial backend path.
