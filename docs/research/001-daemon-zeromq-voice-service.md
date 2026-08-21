# RAGE Research 001 — Zara daemon, ZeroMQ protocol, multi-user isolation, and Voice Mode replacement

Status: research baseline
Date: 2026-08-21
Scope: architecture/research only; no daemon behavior is implemented by this document.

## 1. Research question

Design the next Zara architecture around a long-lived `zara --daemon` process that ordinary Zara clients can connect to, using ZeroMQ as the transport, while preserving Zara's existing actor/runtime work and creating a credible path to replace the day-to-day ChatGPT Voice experience with Zara.

Required end goals:

1. a client that connects to a long-lived daemon;
2. ZeroMQ as the wire transport;
3. a versioned Zara application wire frame defined before implementation;
4. an explicit feature map for replacing ChatGPT Voice with Zara;
5. multiple authenticated users with isolated conversations, memory, configuration, and events;
6. authentication, authorization, encryption, abuse limits, and other security controls suitable for local and remote use.

## 2. Current Zara reality

This work should extend the current runtime rather than create another assistant stack.

Relevant existing architecture:

- `zara/runtime/host.py` already provides a Qt-neutral `RuntimeHost` that owns a dedicated asyncio runtime thread, a backend, the existing `TurnCoordinator`, plugin lifecycle, typed commands, startup/shutdown state, and event publication.
- `zara/actors.py` already implements actor-style turn state, bounded mailboxes, cancellation, stale-turn rejection, and typed voice/LLM/TTS events.
- issues #82 and #83 already established a desktop-neutral runtime event/command boundary and the process-local `RuntimeHost` abstraction.
- issues #24-#30 already cover actor turn coordination, streaming VAD/STT, acknowledgement, interruption/cancellation, LLM streaming, phrase TTS streaming, and warm-start latency.
- issue #51 is the canonical context-management direction.
- issue #124 is the canonical embedded Prolog LLM provider boundary.
- `zara/memory.py` already provides `MemoryManager`, in-memory session state, optional Chroma persistence, summaries, retrieval, and a fail-open local fallback, but it is not currently an authenticated multi-user store.
- `zara/pets/ipc.py` already uses pyzmq, but only as a small unauthenticated localhost PUB/SUB compatibility bridge. It sends one JSON event per message and intentionally drops messages when no subscriber exists. It must not become the daemon protocol.

The key architectural fact is that the daemon does not need to invent Zara's command/event model. It should expose the existing runtime boundary over authenticated ZeroMQ and then make the runtime boundary multi-tenant.

## 3. External protocol research

### 3.1 ZeroMQ topology

ZeroMQ's stable request/reply specification defines DEALER as an asynchronous replacement for REQ and ROUTER as the server-side asynchronous replacement for REP. ROUTER maintains a separate queue for each peer and prepends the peer routing identity to messages received by the application. This is a strong fit for Zara because voice, tool approvals, cancellations, text deltas, audio chunks, and lifecycle events are asynchronous and cannot be represented cleanly by lock-step REQ/REP.

References:

- ZeroMQ REQ/REP, DEALER, ROUTER: https://rfc.zeromq.org/spec/28/
- ZMTP framing/security metadata: https://rfc.zeromq.org/spec/23/
- current ZMTP 3.1 draft: https://rfc.zeromq.org/spec/37/

Decision: the initial daemon protocol should use:

```text
Zara client: DEALER  <---- ZMTP ---->  ROUTER :Zara daemon
```

Do not use PUB/SUB for private conversation traffic. PUB/SUB is useful for lossy fan-out but is a bad authority boundary for per-user private events, request correlation, backpressure, or tool approval. The existing Pet PUB/SUB transport remains a compatibility adapter.

### 3.2 CURVE and ZAP

ZMTP-CURVE provides peer authentication and confidentiality. ZeroMQ's ZAP protocol allows the server to authorize an incoming CURVE public key and return a user identity. PyZMQ exposes CURVE key generation/configuration and authentication helpers.

References:

- ZMTP-CURVE: https://rfc.zeromq.org/spec/25/
- ZAP: https://rfc.zeromq.org/spec/27/
- PyZMQ CURVE: https://pyzmq.readthedocs.io/en/latest/howto/curve.html

Decision:

- every remote-capable daemon has a long-term CURVE server keypair;
- each client has a distinct CURVE client keypair;
- the client pins the daemon public key;
- ZAP maps each allowed client public key to an internal Zara principal and role set;
- application payloads do not get to choose their own authenticated `user_id`;
- authorization is still checked at the Zara command/capability layer after CURVE authentication.

PLAIN authentication must not be used for remote Zara sessions because ZeroMQ documents PLAIN as lacking confidentiality. NULL is acceptable only for explicitly local development fixtures.

### 3.3 Backpressure, failure detection, and message limits

PyZMQ/libzmq expose HWM, `MAXMSGSIZE`, send/receive timeouts, `ROUTER_MANDATORY`, connection timeouts, and heartbeat options. These should be part of the protocol implementation rather than left at library defaults.

Reference:

- PyZMQ socket options: https://pyzmq.readthedocs.io/en/latest/api/zmq.html

Decision:

- every socket and per-client actor/mailbox is bounded;
- the daemon rejects oversized envelopes/payloads before deserialization or model work;
- one slow client cannot block all clients;
- private control events must never be silently reassigned to another connection;
- audio streams use bounded queues and explicit sequence numbers so gaps/overflow are observable;
- `LINGER=0` on teardown is appropriate for ephemeral audio/event delivery, while durable side effects rely on application request IDs/idempotency rather than socket linger;
- ZeroMQ heartbeat options are used for transport liveness; a small application-level `ping`/`pong` remains useful for protocol/capability diagnostics but is not the primary heartbeat.

## 4. Proposed daemon process model

### 4.1 CLI contract

Proposed commands:

```text
zara --daemon
zara --daemon --listen tcp://127.0.0.1:35620
zara --daemon --listen ipc://$XDG_RUNTIME_DIR/zara/daemon.sock
zara --connect <endpoint> ...
zara --standalone ...
```

Normal `zara` becomes a client-capable entry point. The migration should preserve an explicit standalone path until the daemon release gate proves parity.

Recommended default behavior once the daemon client is stable:

1. resolve configured daemon endpoint;
2. attempt a short authenticated connection/handshake;
3. if connected, execute through the daemon;
4. if the daemon is unavailable, fail with a clear diagnostic or use a configured `prefer-daemon` fallback policy;
5. never silently spawn a second microphone-owning runtime if doing so could conflict with an existing daemon.

The first implementation slice should not silently expose a network listener. Remote TCP listening must be opt-in.

### 4.2 Runtime ownership

Proposed ownership:

```text
ZaraDaemon
  |
  +-- ZmqGatewayActor / asyncio task
  |     +-- CURVE + ZAP authentication
  |     +-- frame validation
  |     +-- request/event routing
  |
  +-- PrincipalRegistry
  |     +-- client key -> principal -> roles/capabilities
  |
  +-- UserRuntimeRegistry
        +-- UserRuntime(principal A)
        |     +-- conversation/context service
        |     +-- MemoryManager scoped to A
        |     +-- turn coordinator/state scoped to A
        |     +-- tools/approval policy scoped to A
        |
        +-- UserRuntime(principal B)
              +-- isolated conversation/context/memory/turn state
```

Existing provider/model resources may eventually be pooled where doing so does not mix user state, but stateful objects must not be process-global merely for convenience.

The current `RuntimeHost` is the best seam for the daemon work. It already owns application commands and events off the UI thread. The daemon should either:

- evolve `RuntimeHost` into a principal-aware host; or
- add a daemon-level supervisor that owns a `UserRuntime`/host-like boundary per principal while sharing explicitly stateless provider resources.

Do not bypass `RuntimeHost` and wire ZeroMQ directly to AgentManager, Prolog, MemoryManager, TTS, or desktop objects.

## 5. Zara wire protocol v1

### 5.1 Design goals

The application protocol must be:

- versioned independently of ZMTP;
- asynchronous;
- binary-safe for audio;
- easy to inspect during development;
- explicit about correlation and sequencing;
- safe to reject before dispatch;
- independent of Python class serialization;
- independent of LangChain/provider response classes;
- compatible with future non-Python clients.

### 5.2 ROUTER-visible framing

A DEALER client sends the following application multipart message:

```text
Frame 0  ASCII protocol signature:  "ZARA/1"
Frame 1  UTF-8 JSON envelope
Frame 2+ opaque payload frames, optional
```

The daemon ROUTER receives an additional leading routing-id supplied by ZeroMQ:

```text
Frame 0  ROUTER routing id           [transport-owned]
Frame 1  "ZARA/1"                    [application signature]
Frame 2  JSON envelope               [application metadata]
Frame 3+ opaque payloads              [audio/blob chunks]
```

The routing id is not serialized into the envelope and is not treated as a user identity.

JSON is selected for the v1 envelope because Zara already uses JSON on its existing ZMQ compatibility bridge and because debuggability matters more than shaving a few hundred bytes from control messages. Binary audio stays out of JSON. A later version may negotiate CBOR/MessagePack if measurement justifies it.

### 5.3 Envelope

Required fields for all v1 messages:

```json
{
  "type": "audio.input.chunk",
  "id": "018f...",
  "session_id": "018f...",
  "seq": 42,
  "timestamp_ns": 1787351234567890000,
  "payload_count": 1
}
```

Conditionally required/common fields:

```json
{
  "reply_to": "request-id",
  "conversation_id": "...",
  "turn_id": "...",
  "stream_id": "...",
  "trace_id": "...",
  "content_type": "audio/pcm;rate=16000;channels=1;format=s16le",
  "flags": ["end_of_input"]
}
```

Rules:

- `type` is a closed, versioned message name, not an arbitrary Python import/class name.
- `id` is globally unique enough for request correlation and idempotency. UUIDv7 or another time-sortable random identifier is preferred.
- replies use `reply_to` rather than overloading ZeroMQ routing identities.
- `session_id` identifies one authenticated client session and is assigned/accepted by the daemon during handshake.
- `conversation_id` identifies durable conversational context.
- `turn_id` reuses Zara's canonical turn identifier; no second turn-id system is introduced.
- `stream_id` identifies one audio/text stream inside a turn.
- `seq` is monotonic within the relevant session/stream and detects gaps, stale chunks, and replay mistakes.
- `timestamp_ns` is diagnostic ordering metadata, not an authorization input.
- `payload_count` must equal the number of remaining multipart payload frames.
- `content_type` is required for binary media payloads.
- authenticated user/principal identity is transport/server state. A payload `user_id` field, if ever accepted for display metadata, must never override the authenticated principal.

### 5.4 Initial message vocabulary

Handshake/session:

```text
hello
hello.ok
session.resume
session.resumed
ping
pong
error
```

Conversation/runtime commands:

```text
conversation.open
conversation.close
conversation.list
turn.submit
turn.cancel
tool.approve
tool.reject
runtime.status
```

Voice/audio ingress:

```text
voice.start
voice.stop
voice.mute
audio.input.start
audio.input.chunk
audio.input.commit
audio.input.cancel
```

Daemon events/egress:

```text
voice.state
stt.partial
stt.final
turn.started
assistant.text.delta
assistant.text.done
audio.output.start
audio.output.chunk
audio.output.done
tool.started
tool.approval_required
tool.completed
turn.cancelled
turn.completed
runtime.error
```

This vocabulary should map to the existing desktop-neutral runtime commands/events rather than becoming another domain model.

### 5.5 Handshake and capability negotiation

Client first sends `hello` containing:

- supported application protocol versions;
- client name/version;
- desired codecs;
- feature capabilities such as binary audio and event replay;
- optional prior session id for resumable state.

Daemon responds with `hello.ok` containing:

- selected protocol version;
- newly assigned/current session id;
- authenticated principal display id or opaque subject id;
- server capabilities;
- selected audio input/output codecs and sample rates;
- enforced payload/message limits;
- heartbeat/liveness settings;
- whether session replay/resume is available.

Unknown major versions fail closed. Unknown optional envelope fields are ignored only when the negotiated major version allows forward-compatible extension. Unknown message `type` values return a typed protocol error and are never dispatched dynamically.

### 5.6 Audio v1

Initial wire codec should be deterministic PCM16 for correctness and easy fixture testing.

Recommended baseline:

- input: signed 16-bit little-endian mono PCM, 16 kHz;
- output: signed 16-bit little-endian PCM at the selected TTS/player rate;
- chunks: small bounded frames, typically tens of milliseconds rather than seconds;
- `seq` is mandatory per audio stream;
- late chunks after cancel/commit are rejected;
- client and daemon may negotiate Opus in a later slice for remote bandwidth efficiency.

The daemon must never buffer an unbounded amount of microphone audio while model work stalls.

### 5.7 Delivery semantics

ZeroMQ provides ordered delivery between immediate connected peers, but Zara still needs application semantics across disconnect/reconnect and side effects.

v1 should define:

- control requests: at-least-once retry is allowed only with a stable request id and idempotent daemon handling;
- side-effecting tool requests: request id/idempotency record is mandatory;
- audio chunks: no automatic replay; sequence gaps cancel or degrade the affected stream explicitly;
- server events: ordered per session/turn; optional bounded replay buffer can be a later capability;
- reconnect: conversation state may resume, but in-flight audio/turn execution is not assumed resumable unless the server explicitly reports that capability.

## 6. Multi-user memory and conversation isolation

Current `MemoryManager` has one collection name, process-local session dictionary, and one `current_session_id`. That shape is unsafe as the multi-user daemon boundary.

Required invariant:

```text
principal A cannot retrieve, summarize, mutate, enumerate, infer existence of,
or subscribe to principal B's conversation or memory state.
```

Recommended model:

```text
Principal
  +-- user-scoped configuration
  +-- conversations
  +-- context state
  +-- learned memories
  +-- memory embeddings/vector namespace
  +-- tool grants/approval policy
  +-- audit records
```

Implementation direction:

- create a `principal_id`/owner key at the persistence boundary, not only in UI code;
- every conversation/message/memory query requires the authenticated principal scope;
- instantiate `MemoryManager` through a user-scoped memory service or make its persistence API require principal scope explicitly;
- use separate Chroma collections/directories per principal initially if that produces a clearer hard boundary; a shared collection is acceptable only if every operation is structurally forced to include an owner filter and regression tests prove no unscoped query path exists;
- remove process-global `current_session_id` assumptions from daemon-owned state;
- temporary/guest sessions get explicit ephemeral principals that cannot see durable memory;
- deletion/export operates per principal and is auditable.

The daemon should support several authenticated users concurrently, but a single voice device belongs to the client, not globally to the server. This becomes especially important for remote clients.

## 7. Security model

### 7.1 Threat model

At minimum, defend against:

- an unauthenticated network client invoking Zara tools;
- one legitimate user reading another user's conversations/memory;
- a client spoofing another `user_id` in an envelope;
- replaying a side-effecting request;
- malformed/oversized frames exhausting memory or parser time;
- a slow client causing daemon-wide unbounded queues;
- a compromised client key retaining access forever;
- logs exposing audio, transcripts, secrets, authorization headers, CURVE private keys, or provider credentials;
- remote binding becoming enabled accidentally;
- stale turn/audio output speaking after cancellation or reconnect.

### 7.2 Required controls

Transport/authentication:

- CURVE encryption/authentication for any TCP endpoint beyond an explicitly isolated development fixture;
- ZAP allowlist/database mapping client public keys to principals;
- server public-key pinning on clients;
- client key rotation/revocation;
- secure key files with owner-only permissions;
- no secrets in CLI process listings where avoidable.

Authorization:

- authenticated principal -> roles/capabilities;
- command-specific authorization after frame parsing and before runtime dispatch;
- existing tool approval/security policy remains authoritative;
- daemon administrative actions use a separate admin capability, not merely "connected locally".

Isolation:

- principal scope attached server-side to every command/event/context lookup;
- route private events only to sessions belonging to that principal;
- per-principal rate, connection, mailbox, model, tool, and audio quotas;
- no global mutable conversation id/current session.

Protocol hardening:

- strict frame count/signature/version checks;
- bounded JSON envelope size;
- `MAXMSGSIZE` plus application payload limits;
- content-type allowlist;
- numeric range validation for sequence/timestamps/rates;
- reject duplicate invalid IDs and illegal state transitions;
- no pickle/eval/dynamic import deserialization;
- malformed clients receive bounded errors then may be disconnected/rate limited.

Operations/privacy:

- structured security audit records contain principal id, action type, result, request/turn ids, and timing but not raw transcript/audio/secrets;
- explicit diagnostics redaction reuse from desktop work;
- opt-in remote listen; safe default is local IPC/loopback;
- daemon refuses wildcard/public bind without secure authentication configured;
- key revocation takes effect without requiring a full data migration.

## 8. Client responsibilities

A proper Zara client is not just a thin `send_json()` helper. It owns client-side interaction state:

- endpoint discovery/configuration;
- CURVE key loading and server-key pinning;
- handshake/version negotiation;
- reconnect with bounded backoff;
- request correlation;
- session/conversation selection;
- local audio capture/playback;
- immediate local stop of playback on interruption/cancel;
- stream sequencing;
- rendering typed events;
- local UI/CLI cancellation;
- sanitized diagnostics.

For local desktop/voice parity, microphone and speaker device ownership should migrate toward the client edge. The daemon should receive microphone chunks and return output audio chunks. This avoids a process-global microphone and makes remote/mobile/multiple-user clients possible.

Wake-word operation can remain a local client concern: detect wake locally, then open/start a voice session and stream the bounded post-wake audio window to the daemon. That is more private and scalable than continuously streaming every user's microphone to the daemon.

## 9. Replacing ChatGPT Voice: functional target

This section is a feature-parity target, not a claim about ChatGPT's internal implementation.

OpenAI's current public Voice documentation describes natural free-form voice inside a chat, simultaneous listen/speak in Live, interruption, streamed text alongside speech, memory, web search, text/image input, background conversations, and—under the older Advanced mode on supported mobile clients—video and screen sharing. The official Realtime SDK documentation likewise emphasizes long-lived sessions, incremental audio/text, tools, history, interruption events, and playback tracking.

References:

- ChatGPT Voice: https://help.openai.com/en/articles/20001274
- OpenAI Realtime Agents guide: https://openai.github.io/openai-agents-python/realtime/guide/
- Realtime transport: https://openai.github.io/openai-agents-python/realtime/transport/

### 9.1 Zara already has or has roadmap coverage for

- streaming VAD/STT;
- actor-scoped turns and cancellation;
- barge-in;
- immediate acknowledgement;
- LLM/tool execution;
- phrase-sized streaming TTS roadmap;
- typed runtime events;
- persistent conversations/desktop UI roadmap;
- local/remote-capable model/provider abstractions;
- memory retrieval/persistence;
- Prolog deterministic routing;
- desktop context/Local Recall integration roadmap;
- tool approvals;
- latency benchmarking.

### 9.2 Minimum Voice replacement milestone

A user should be able to launch a Zara client and have:

1. persistent authenticated session to `zara --daemon`;
2. microphone input streamed as bounded audio chunks;
3. fast speech-start/speech-end detection;
4. partial/final transcription visible while speaking;
5. turn processing without closing the connection;
6. assistant text streaming as it is generated;
7. phrase/audio chunks played before the whole answer completes;
8. speaking over Zara immediately stops local playback and cancels/truncates stale work;
9. text typed into the same conversation while voice is active;
10. durable transcript/conversation history;
11. per-user long-term memory retrieval with explicit controls;
12. tool calls and approval events during voice;
13. web/search/tool results represented through typed events rather than spoken-only output;
14. reconnect that preserves the durable conversation even if the in-flight turn is lost;
15. background daemon operation without an open GUI window;
16. multiple authenticated users without memory/event leakage;
17. measurable latency/soak targets at least as strict as the existing Zara voice roadmap.

### 9.3 Later parity/advantage milestones

- image attachment in the same live conversation;
- Local Recall/screen-context attachment with explicit permission;
- live screen/video stream protocol if Zara later needs it;
- multi-device clients;
- telephony/SIP adapter;
- per-user model/provider preferences;
- local/offline STT+LLM+TTS profile;
- multiple simultaneous independent users/conversations;
- agent/task progress voice notifications;
- stronger deterministic Prolog/tool control than a voice-only assistant surface.

Video/screen streaming must not be forced into protocol v1 merely for parity. The v1 multipart media design should make additional media content types possible later without destabilizing the voice transport.

## 10. Migration plan

### Phase A — protocol and daemon skeleton

- implement daemon CLI/lifecycle;
- ROUTER gateway + DEALER client;
- v1 handshake/frame validation;
- local text command/typed event round trip;
- no audio yet;
- preserve standalone mode.

### Phase B — authentication and multi-user ownership

- CURVE/ZAP principal mapping;
- user/session registry;
- per-principal conversation/context/memory persistence;
- ACL and audit layer;
- cross-user isolation tests.

This phase should land before any remote listening is presented as production-safe.

### Phase C — streaming voice transport

- client-owned mic/player;
- `audio.input.*` and `audio.output.*` frames;
- bridge into existing streaming STT/VAD, turn coordinator, LLM, and TTS paths;
- cancellation/barge-in across process boundary;
- latency tracing preserved end to end.

### Phase D — client UX parity

- CLI client;
- desktop client uses the same daemon client library;
- persistent background session;
- transcript/text + voice simultaneously;
- tool approval/events;
- reconnect/resume.

### Phase E — release/security gate

- multi-user concurrency soak;
- reconnect storms;
- slow/malicious client tests;
- oversized/malformed frame fuzz fixtures;
- CURVE/ZAP auth failure/revocation tests;
- cross-user memory/event non-interference tests;
- voice interruption latency tests;
- full Zara deterministic/Nix gate.

## 11. Non-goals for the first daemon epic

- no replacement of Pykka/TurnCoordinator merely because ZeroMQ is introduced;
- no second LLM/provider stack;
- no second memory implementation independent from the existing MemoryManager/context roadmap;
- no pickle/Python-object wire protocol;
- no unauthenticated public listener;
- no global shared `current_user` mutable variable;
- no separate desktop-only daemon protocol;
- no requirement for video/screen streaming in protocol v1;
- no claim that ZeroMQ transport alone provides application authorization/idempotency;
- no hidden auto-execution authority for remote clients beyond Zara's existing tool/capability policy.

## 12. Required implementation issues derived from this research

The epic should create dependency-ordered child issues for:

1. daemon lifecycle + process supervisor boundary;
2. ZeroMQ v1 framing, handshake, client SDK, and typed command/event bridge;
3. CURVE/ZAP authentication, key lifecycle, roles, quotas, and security audit;
4. multi-user conversation/context/memory persistence isolation;
5. streaming audio ingress/egress with client-owned devices and cross-process barge-in;
6. desktop/CLI client migration and reconnect/session UX;
7. Voice replacement release matrix, soak/latency/security/fuzz gates.

The daemon work should integrate with #51, #82/#83, #28/#29/#30/#31, and #124 rather than duplicating them.

## 13. Research conclusion

The daemon direction is justified and fits Zara's existing architecture well.

The strongest design is not "turn the current process into a socket server." It is:

```text
client-owned interaction/device edge
        |
 authenticated DEALER
        |
  ZARA/1 over ZeroMQ
        |
 authenticated ROUTER gateway
        |
 principal-scoped runtime boundary
        |
 existing Zara actors/context/Prolog/LLM/tools/memory/voice pipeline
```

ZeroMQ ROUTER/DEALER gives the required asynchronous multiplexing. CURVE+ZAP gives a native transport authentication/encryption path. Zara's actor/runtime work already provides the cancellation and bounded-state primitives needed behind the gateway. The main architectural work is therefore principal scoping, an explicit wire contract, client-side device ownership, and proving that private state cannot cross user boundaries.

That path can realistically replace the core ChatGPT Voice workflow for Zara while also enabling things ChatGPT's one-account client model is not optimized for: self-hosting, local/offline providers, explicit Prolog routing, multiple independent users, and user-controlled persistence/security policy.
