# Assistant runtime contract

Zara's assistant runtime boundary is **backend-neutral**. The built-in Python
runtime is the default. Prolog-RLM is optional and is discovered only when a
compatible local installation is actually reachable.

The architecture authority and acceptance matrix are tracked in
`lost-rob0t/zara#1046`.

## Dependency direction

```text
Zara -> ZARA-RUNTIME/1 adapter -> Prolog-RLM
AgentProlog -> Prolog-RLM
```

Zara does not depend on AgentProlog. Prolog-RLM does not depend on Zara.
AgentProlog may advertise an optional profile through Prolog-RLM, but it does
not replace the generic runtime ABI.

When Prolog-RLM is selected, Prolog-RLM owns provider/model calls, planning,
recursive execution, budgets, cancellation and runtime traces. Zara remains
the host for UI, project/chat context, plugins, principal/approval checks and
platform integration. The desktop Python process must not run a second LLM or
planner behind Prolog-RLM.

## ZARA-RUNTIME/1 descriptor

Every discovered runtime projects this bounded semantic descriptor:

```text
id
display_name
protocol = ZARA-RUNTIME/1
runtime_version
implementation_version
installed
available
health = starting | ready | busy | degraded | failed | stopped
locality = embedded | local_process | local_sidecar | remote
transport = in_process | stdio | loopback_http | binder | zara_remote
capabilities[]
profiles[]
provider_control = runtime | zara | mixed
model_control = runtime | zara | mixed
supports_streaming
supports_cancel
supports_context_handles
supports_host_tools
```

Discovery is observation only. It grants no tool, provider, filesystem,
principal, plugin or process authority. Duplicate runtime identities and
unknown protocol major versions fail closed.

The UI lists only discovered, compatible, selectable runtimes. A stale
preference stores only a stable runtime id and is restored only after fresh
discovery confirms that id.

## Host operations

The semantic host contract is:

```text
runtime.discover() -> [RuntimeDescriptor]
runtime.health(runtime_id) -> RuntimeHealth
runtime.select(runtime_id) -> RuntimeSelection
runtime.current() -> RuntimeSelection
runtime.capabilities(runtime_id) -> RuntimeDescriptor

runtime.open_session(runtime_id, options) -> Session
runtime.generate(session, request) -> stream<RuntimeEvent>
runtime.cancel(request_id) -> CancelResult
runtime.close_session(session_id) -> CloseResult
runtime.shutdown(runtime_id) -> ShutdownResult
```

The current Prolog-RLM first slice uses buffered loopback HTTP and truthfully
advertises `supports_streaming=false`. Consumers must not invent streaming
events until the sidecar advertises that capability.

## Request envelope

```text
RuntimeRequest {
  request_id
  session_id?
  mode = chat | direct | rlm | agent
  messages[]
  system?
  model_selection?
  provider_selection?
  context_handles[]
  inline_context?
  host_tools[]
  budgets {
    wall_time_ms
    max_output_bytes
    max_tokens?
    max_cost?
    max_tool_calls?
    max_model_calls?
    max_recursion_depth?
  }
  metadata {
    principal_id?
    project_id?
    conversation_id?
    trace_id?
  }
}
```

Provider/model fields are preferences only when the selected runtime declares
that ownership mode. Zara's Prolog-RLM adapters currently omit provider/model
credentials entirely: the sidecar owns them. Literal provider secrets are not
part of the wire contract.

## Runtime events

A streaming-capable runtime emits one ordered vocabulary:

```text
accepted
text_delta
status
usage
host_tool_request
host_tool_result
completed
cancelled
failed
```

Exactly one terminal event is allowed. Raw provider SSE, hidden reasoning,
arbitrary Prolog terms, Python objects, stack traces and secrets do not cross
into UI code.

## Error kinds

```text
unavailable
incompatible_protocol
invalid_request
unauthorized
capability_denied
provider_error
model_error
context_error
tool_error
budget_exceeded
timeout
cancelled
transport_error
runtime_error
```

Errors expose only bounded safe messages.

## Context and plugins

Context is host-owned input. Zara may gather project/chat/plugin/platform
context in Python or Kotlin and hand it to the selected runtime as bounded
inline context today or opaque handles when the runtime advertises
`supports_context_handles`.

Plugins remain owned by Zara's canonical registry. Prolog-RLM cannot invoke a
host plugin until it advertises `supports_host_tools` and requests a typed
tool through the runtime contract. Zara still validates principal, capability,
generation and approval before dispatch.

Model output never gains ambient Python `eval`, shell, filesystem, plugin
registry or arbitrary Prolog execution.

## Desktop

Desktop configuration:

```toml
[runtime]
backend = "zara-python"
prolog_rlm_endpoint = "http://127.0.0.1:18765"
discovery_timeout = 0.35
request_timeout = 30.0
```

The endpoint is restricted to explicit loopback HTTP. Opening Assistant
settings performs bounded discovery and adds Prolog-RLM to the Runtime menu
only when it returns a compatible live descriptor.

## Android

Android always has the `embedded-local` runtime. It probes only the fixed
loopback Prolog-RLM sidecar at `127.0.0.1:18765`; it does not scan LAN
endpoints and does not bundle SWI-Prolog into the APK.

The selected runtime id is persisted separately from the existing
`Auto / Local / Remote` routing policy. Normal local-language turns use the
selected assistant runtime. Explicit `?-`, `/prolog`, and `/expert`
commands remain direct operations against Zara's local symbolic workspace.

If the sidecar fails, Zara re-runs discovery and the registry falls back to
`embedded-local` if Prolog-RLM is no longer reachable.

## Prolog-RLM sidecar

The matching Prolog-RLM implementation is developed in
`lost-rob0t/prolog-rlm#448` and currently exposes:

```text
GET  /zara-runtime/v1/discover
GET  /zara-runtime/v1/health
POST /zara-runtime/v1/generate
POST /zara-runtime/v1/cancel
```

It binds loopback only and rejects literal provider credentials.
