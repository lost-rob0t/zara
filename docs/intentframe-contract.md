# IntentFrame Contract — Version 1 (frozen by #154)

Status: **FROZEN for #155/#156/#172 implementation.** Owner: epic #150 (semantic intents), consumed by #172 (portable Prolog core), #173 (ZARA/1 enrollment), #196-#204 (Android/Samsung children).

Change rule: any field addition is a minor version (`"if": 2`); any removal/rename/semantic change of an existing field is a major version and requires a migration. Additions must not break older readers.

## 1. What the frame is

An `IntentFrame` is the single semantic result of resolving one user utterance (or one follow-up turn) against the semantic vocabulary. It is a pure value: it owns no state, performs no execution, and holds no reference to any provider, shell, or device identity.

**Semantic facts** (inside the frame):

- intent identity: namespace + name
- typed slots with per-slot origin
- dialogue status (complete / missing / ambiguous / invalid / cancelled / superseded)
- evidence: how the frame was derived (resolver rule provenance; never model internals)

**Runtime correlation metadata** (envelope only, never inside the frame):

- `frame_id`, `turn_id`, `conversation_id`, `session_id`, `principal`, `trace_id`, `issued_at_ns`, `supersedes` (frame_id), `request_id` (idempotency for side-effectful execution)

This split answers #154's "which fields are semantic facts vs runtime correlation metadata": if a field would differ when the same words are said by the same user in the same conversation, it is correlation metadata, not semantics.

## 2. Prolog representation (portable subset)

Plain compounds and lists only. **No dicts** (Trealla 2.106.1 rejects `a{b:1}` — verified empirically, see rage/154-intentframe-research.org), no SWI library dependencies beyond `library(lists)`, no attributed-variable requirements in the contract terms, no floats in slot identity.

```prolog
frame(Intent, Slots, Status)
intent(ns(NS), name(Name))                 % NS in {device, app, message, web,
                                           %        memory, skill, conversation,
                                           %        dictation, media}
slot(name(SlotName), value(V), origin(O))  % O in {utterance, follow_up, context, default, correction}

% V (typed slot values — never raw foreign objects):
text(Atom)            % human text, never executed
number(Number)        % finite number
duration(Seconds)     % non-negative integer seconds
datetime(Year,Month,Day,Hour,Min,Sec)
ref(kind(K), id(Id))  % typed reference (app alias, capability id); never a raw Intent
boolean(Bool)

% Status:
complete
missing(MissingNames)          % list of slot-name atoms
ambiguous(Alternatives)        % list of alternative intent/1 or value atoms
invalid(SlotName, Reason)      % typed reason atom, e.g. invalid(value(duration), negative)
cancelled
superseded(ByFrameId)          % Only meaningful with the envelope's supersedes field
```

Resolver API (the only Prolog entry point that exists after #156):

```prolog
resolve_frames(+Text, +State, +Context, -Frames).
% State in {passive, conversation, dictation} (unchanged gate semantics from intent_resolver).
% Context: [] or partial_frame(Frame0, Missing) when a clarification session is open.
% Frames: list of frame/3 terms; exactly one frame for normal cases.
```

`resolve_frames` is **pure**: no assert/retract, no dynamic pending store, no process spawn, no network. Side effects live exclusively in execution plans (#157), not in resolution.

## 3. Wire representation (single normalized serializer)

JSON mirror with one serializer and one comparator implementation shared by SWI/server and the Android runtime (#172 owns the implementation; this contract fixes the shape):

```json
{
  "if": 1,
  "intent": {"ns": "device", "name": "timer.set"},
  "slots": {
    "duration": {"value": {"duration": 120}, "origin": "utterance"},
    "label":    {"value": {"text": "tea"},       "origin": "utterance"}
  },
  "status": "complete",
  "evidence": {"resolver": "prolog.intent_resolver", "steps": ["verb_head:timer", "unit:normal"]}
}
```

Envelope (separate, existing ZARA/1 envelope keys): `turn_id`, `conversation_id`, `principal`, `trace_id`, `supersedes`, `request_id`.

Rules:

- slot values are the tagged JSON forms above; no nested arbitrary objects; no provider/package identifiers inside semantic slots (app aliases are `ref(kind(app_alias), id("firefox"))` resolved per platform later).
- `text(...)` is human text. It is never a shell command, never an Intent term, never a path.
- Comparators: two frames are equivalent iff intent, status, and slot set (name→(value, origin)) are equal; evidence is informational and excluded from fixture comparison (one normalized serializer/comparator, see design artifact).

## 4. Status semantics and dialogue composition

- `missing([...])` — the frame is *open*. The runtime host (conversation-scoped clarification session, #155) may submit follow-up utterances with `context(partial_frame(Frame))`. Follow-up slot values get `origin(follow_up)`.
- Clarification prompt ownership is the runtime host's, generated from the frame's missing list — never prompt-managed by an LLM, never a global pending store (rejected alternatives; see research artifact).
- Correction — a follow-up utterance that re-resolves to the same intent with a conflicting slot value replaces the open frame; the new frame's corrected slots carry `origin(correction)`, and the envelope's `supersedes` references the prior frame id.
- Cancellation — `turn.cancel` (or a stop phrase resolving to `conversation.end`) closes the clarification session and emits `status: cancelled` for the open frame. Cancelled frames are never executed.
- `ambiguous(...)` — resolver found several equally-ranked readings; resolution may not silently pick one. The clarification session asks; no default picking at resolve time.
- `invalid(slot, reason)` — a slot parsed but violates its type/bounds (e.g. negative duration). Open for re-elicitation like `missing`.

## 5. Required examples (normative)

Each example shows the wire form (compact) that the corpus (#156) and cross-runtime fixtures (#172) must reproduce equivalently on SWI and Trealla.

1. **Complete timer** — "set a timer for 2 minutes called tea"
   `intent {device,timer.set}`, slots `duration={duration:120,utterance}`, `label={text:"tea",utterance}`, `complete`.
2. **Bare timer → missing duration** — "timer"
   `intent {device,timer.set}`, slots `{}`, `status missing([duration])`.
3. **Follow-up duration** — (open timer frame) then "2 minutes"
   same intent, `duration={duration:120, follow_up}`, status `complete`.
4. **Correction** — (open timer frame) "actually 5 minutes"
   `duration={duration:300, correction}`, envelope `supersedes: <frame-id>`, status `complete`.
5. **Cancellation** — (open timer frame) "cancel"
   new frame `intent {conversation,cancel}`, status `complete`; open frame becomes `superseded`; nothing executes.
6. **Open app** — "open firefox"
   `intent {app,open}`, slot `target={ref(kind(app_alias), id("firefox")), utterance}` (alias resolution is capability-layer, not semantic).
7. **Bare open** — "open"
   `status missing([target])`.
8. **Text with missing message** — "text alice"
   `intent {message,send}`, slots `recipient={ref(kind(contact), id("alice")), utterance}`, `status missing([message])`.
9. **Server search** — "search prolog dictionaries"
   `intent {web,search}`, slots `query={text:"prolog dictionaries", utterance}`; identical frame whether search executes server-side or on-device — only the ExecutionPlan differs.
10. **Device screenshot** — "take a screenshot"
    `intent {device,screen.capture}`, slots `{}`; on Android the plan resolves to the device screenshot capability, on Linux to the configured screenshot provider, else typed unavailable.
11. **Server memory query** — "what did I ask you yesterday"
    `intent {memory,query}`, slots `query={text:"what i asked yesterday", utterance}` — resolved identically on server and client; execution is server-side.
12. **Unavailable capability** — complete frame for `{app,open}` where no provider can satisfy `target`
    frame remains `complete`; the ExecutionPlan carries `unavailable(target, reason)` — never a fabricated semantic status.
13. **Same input, different platform availability** — "open termux"
    Linux: plan `unavailable` (no Termux provider) unless Termux installed; Android: plan `open_app(com.termux)` via RUN_COMMAND contract. One frame, two plans — divergence is an explicit platform-provider difference, not parser drift.

## 6. Non-goals and hard boundaries

- The frame never contains: raw shell/provider strings, package ids, principal/session ids, turn ids, LLM outputs as authority, or execution results.
- Resolution never executes. Execution consumes frames only via typed ExecutionPlans (#157) that name registered capabilities.
- No second context manager (#51): clarification sessions reference conversation context by id; they do not duplicate or own conversation history.
- #122's rewritten text feeds resolution as input only — the rewriter never becomes semantic authority.
