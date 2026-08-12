# Zara Desktop Copilot: Research and Architecture

**Status:** proposed architecture and implementation plan  
**Research baseline:** `master` at `48b2e482080ec4a6b8916ccc8ee892af980177b1`  
**Primary target:** Linux desktop, with Windows/macOS kept architecturally possible  
**UI toolkit decision:** PySide6 / Qt for Python

## Executive decision

Zara Desktop should be a native Qt view/controller over the existing Zara runtime. It must not become a second assistant stack.

The target architecture is:

```text
                         Zara Desktop Shell
                  (one QApplication, one main tray)
                              |
             +----------------+----------------+
             |                                 |
       Quick Copilot                      Full Chat
       summon window                    conversation UI
             |                                 |
             +---------------+-----------------+
                             |
                    Desktop Controller
                             |
                    Qt <-> Runtime Bridge
                             |
                 Zara Application Runtime
                             |
          +----------+-------+--------+---------+
          |          |                |         |
        Voice      Prolog            LLMs      Tools
          |          |                |         |
          +----------+-------+--------+---------+
                             |
                    neutral runtime events
                             |
               +-------------+-------------+
               |                           |
             Desktop                       Pets
```

The most important architectural change is not a widget: Zara needs a stable, desktop-neutral application boundary. Qt widgets should issue application commands and consume structured runtime events. They should not call `AgentManager`, `PrologEngine`, wake internals, or individual tools directly.

## Key decisions

| Area | Decision |
|---|---|
| Qt binding | PySide6 |
| Desktop process | One `QApplication` and one canonical Zara tray |
| Quick/full UI | Separate windows backed by the same conversation/context models |
| Runtime integration | In-process application boundary first; optional IPC only where useful |
| Concurrency | Qt GUI thread stays UI-only; Zara runtime owns a dedicated worker/async thread |
| Event model | Promote provider-neutral events out of the pet subsystem into a runtime-level contract |
| Cancellation | Turn-ID-scoped, building on `TurnCoordinator` |
| Streaming | UI supports deltas immediately; real provider streaming reuses the existing streaming-LLM roadmap rather than creating a desktop-only LLM path |
| Persistence | Existing SQLite `DatabaseManager` for conversation data; existing TOML config for settings |
| Context | Explicit attachments/capabilities; no silent screen, clipboard, microphone, or broad filesystem capture |
| Plugins | Capability descriptors, not arbitrary access to Qt internals |
| Pet | Optional runtime-event consumer; integrated with the main desktop tray when desktop shell is active |
| Rich text | Native Qt text/code widgets first; do not embed an entire browser application |

---

# 1. Current Zara architecture

Zara is already a hybrid Python/Prolog assistant rather than a monolithic voice loop.

Important existing components include:

- `zara/__main__.py`: CLI/runtime mode selection.
- `zara/wake.py`: wake-word listener and primary voice interaction lifecycle.
- `zara/streaming_stt.py`, `zara/transcription.py`, `zara/audio.py`: speech input pipeline.
- `zara/prolog_engine.py`: serialized PySWIP/SWI-Prolog application boundary.
- `zara/agent/`: conversational agent, LangGraph loop, provider selection, tools, history.
- `zara/agent/tools/registry.py`: built-in and user tool registry.
- `zara/actors.py`: bounded Pykka actors, typed turn events, turn IDs, cancellation.
- `zara/database.py`: shared SQLite database/migration layer.
- `zara/config.py`: XDG TOML configuration.
- `zara/notifications.py`: existing headless/Linux notification adapter.
- `zara/pets/`: PySide6 pet UI, state actor, ZMQ transport, import formats and tray.

This means most desktop-enabling primitives already exist. The missing piece is an application API that sits above voice/agent/tool implementation details.

## Existing runtime strengths to preserve

1. Prolog is already isolated behind `PrologEngine`.
2. Tool loading already has a registry boundary.
3. Conversation orchestration already lives in `AgentManager` and LangGraph.
4. Pykka is already used for bounded actor-style state ownership.
5. `TurnCoordinator` already owns turn IDs and cancellation state.
6. The pet subsystem already proves PySide6, `QSystemTrayIcon`, multi-monitor placement, and Qt/ZMQ event consumption work in this repository.
7. Nix already supplies PySide6, Pykka and ZeroMQ.

---

# 2. Current runtime lifecycle

The current CLI selects a mode such as wake, console, dictate, agent, or Pets. Wake mode owns a long-lived voice loop. Pet mode may run beside it and consume runtime state over a small event bridge.

A simplified current voice lifecycle is:

```text
startup
  |
  +--> config + Prolog + memory + providers + tools
  |
  +--> audio / wake listener
          |
          v
       PASSIVE
          |
       wake detected
          |
          v
        ACTIVE
          |
       capture speech
          |
       transcribe
          |
       route command/conversation
          |
       execute Prolog or LLM/tool turn
          |
       response / TTS
          |
       conversation grace / passive return
```

The desktop shell should become another lifecycle owner without deleting headless modes. A long-term executable layout should support:

```text
zara                 # existing CLI compatibility during migration
zara --wake          # headless voice runtime
zara-pets            # standalone pet compatibility
zara-desktop         # canonical native desktop shell
```

A future release may choose to make `zara` launch the desktop shell by default, but that should be a migration decision after the desktop path is mature.

---

# 3. Existing functionality reusable by the GUI

The GUI should reuse rather than recreate:

- wake-word and voice capture state machine;
- faster-whisper / streaming STT pipeline;
- TTS engine and barge-in behavior;
- Prolog intent resolution and command execution;
- LangGraph agent and provider configuration;
- tool registry and user tool loading;
- memory manager;
- SQLite database layer;
- XDG TOML configuration;
- notification fallback;
- Pykka turn coordinator/cancellation;
- pet state model, sprite importer and animation system;
- existing PySide6 multi-monitor/transparent-window code;
- Nix packaging and CI conventions.

Desktop code should not fork these implementations.

---

# 4. PyQt6 versus PySide6

## Decision: PySide6

PySide6 is the correct choice.

Reasons:

- Zara already depends on and ships PySide6 for Pets.
- The Nix development environment already contains PySide6.
- PySide6 is Qt's official Python binding and tracks Qt APIs directly.
- Qt/PySide offers LGPL/GPL/commercial licensing options; Zara itself is GPL-3.0, so there is no reason to add the extra PyQt-specific licensing/tooling surface.
- The existing tray, window flags and screen handling code can be reused instead of translated.
- Qt Widgets, Qt Quick, Qt Multimedia, QtDBus and QtWebEngine remain available if specific future surfaces justify them.

PyQt6 would add migration cost with no architectural advantage for this project.

## Widgets versus Qt Quick

Use Qt Widgets for the first desktop shell, tray, settings, history, command palette and chat chrome because the repository already uses Widgets and they are easy to exercise with deterministic Qt tests.

Qt Quick is not prohibited. It may be useful later for highly animated surfaces, but it should not become a prerequisite for core desktop functionality.

---

# 5. Qt architecture

Proposed repository shape:

```text
zara/
  runtime/
    commands.py
    events.py
    service.py
    bridge.py

  desktop/
    app.py
    controller.py
    state.py
    qt_bridge.py

    tray/
      tray.py
      state.py

    windows/
      copilot.py
      chat.py
      history.py
      settings.py
      diagnostics.py
      command_palette.py

    models/
      conversation.py
      context.py
      tools.py

    services/
      shortcuts.py
      notifications.py
      context.py
      permissions.py

    widgets/
      message.py
      code_block.py
      tool_call.py
      context_chip.py

  pets/
    ... existing implementation ...
```

This hierarchy is a target, not a requirement to move unrelated code prematurely.

`zara.runtime` should be desktop-neutral. `zara.desktop` may import `zara.runtime`; the reverse dependency must not exist.

---

# 6. Desktop Copilot UX research

Current desktop assistants converge on several useful patterns:

- a global summon shortcut;
- a small keyboard-first quick window;
- a richer persistent conversation window;
- one-action handoff from quick to full mode while preserving conversation state;
- explicit file/screenshot/application context;
- streaming status instead of frozen spinners;
- visible tool/action execution;
- stop/cancel controls;
- persistent recent chats/history;
- background operation with a small tray/menu-bar presence;
- native permission prompts for screen/microphone/application access.

Raycast's split between Quick AI and full AI Chat is particularly suitable for Zara: quick interaction should not become a miniature second conversation implementation. Microsoft Copilot and ChatGPT likewise demonstrate quick companion surfaces that coexist with a full application.

## Adopt

- instant summon and autofocus;
- `Escape` to hide quick UI;
- `Enter` to submit and `Shift+Enter` for newline;
- quick/full handoff preserving the exact conversation/context;
- explicit attachment chips;
- compact tool progress with expandable details;
- stop/cancel at all long-running stages;
- keyboard navigation throughout;
- native background/tray behavior;
- opt-in screenshot/window/application context.

## Explicitly avoid

- cloning another assistant's visual identity;
- treating a web browser shell as the entire application;
- silently reading the screen or clipboard;
- hiding meaningful tool side effects behind vague "thinking" states;
- dumping raw internal logs into normal chat;
- creating a second LLM/tool configuration stack;
- animated tray noise for every token/event.

---

# 7. Tray architecture

There should be one canonical Zara desktop tray whenever the desktop shell is active.

Existing `zara.pets.qt_overlay` already demonstrates important behavior:

- `QApplication.setQuitOnLastWindowClosed(False)`;
- `QSystemTrayIcon`;
- left-click activation handling;
- context menus;
- native tray messages;
- cleanup on `aboutToQuit`;
- multi-monitor-aware UI.

The desktop shell should extract/generalize that lifecycle rather than run an independent pet tray beside a desktop tray.

Suggested menu, capability-derived at runtime:

```text
Open Zara
New Chat
Quick Copilot
Command Palette
Voice Mode

Status
  LLM: connected
  Voice: ready
  Prolog: ready
  Tools: ready

Timers / Alerts
Recent Conversations

Pet
  Show Pet
  Pet Settings

Settings
Diagnostics / Logs

Restart Runtime
Quit Zara
```

Unavailable capabilities should be disabled or absent, not fabricated.

## Tray state

Use a small stable set of states:

- idle;
- listening;
- thinking;
- tool running;
- needs input;
- ready;
- disconnected/error.

Prefer icon/tooltip changes over constant animation.

Linux tray support varies by desktop environment. The application must call Qt's tray availability APIs and degrade gracefully when a tray is unavailable.

---

# 8. Global shortcut architecture

Qt does not provide one fully portable global-hotkey abstraction that behaves identically across modern Linux desktops.

Create a `GlobalShortcutService` interface with platform backends.

## Wayland

Prefer the XDG Desktop Portal GlobalShortcuts interface. The compositor/portal owns user authorization and can return activation tokens for correct window activation behavior.

Do not emulate a global shortcut with X11-only key grabbing on Wayland.

## X11

Provide an X11 backend. Existing `pynput` may be reusable if its global-key behavior is reliable under the supported environments; otherwise use a focused native backend. Keep X11 implementation behind the service interface.

## Failure behavior

If the environment cannot provide a global shortcut:

- Zara still works from tray and application launcher;
- settings clearly report the shortcut as unavailable;
- no crash or infinite retry occurs.

---

# 9. Conversation architecture

Quick Copilot and Full Chat should be separate Qt windows backed by the same `ConversationModel` and `ConversationService`.

Why separate windows:

- they have materially different geometry and focus semantics;
- quick mode may use tool/frameless/always-on-top flags;
- full chat is a normal resizable application window;
- maintaining one giant widget that continually transforms between modes creates unnecessary layout/state coupling.

The handoff operation should be approximately:

```text
QuickWindow(conversation_id = C)
        |
      expand
        v
FullChatWindow.open(conversation_id = C)
        |
QuickWindow.hide()
```

No message/context copy is needed because both surfaces reference the same model.

## Persistence

Use the existing SQLite `DatabaseManager` and migrations for:

- conversations;
- messages;
- attachment metadata;
- tool runs;
- titles/renames;
- timestamps;
- provider metadata where useful.

Do not create a second database solely for desktop chat.

---

# 10. Streaming

The desktop model should support streaming deltas from its first implementation:

```text
AssistantStarted
AssistantDelta("first chunk")
AssistantDelta(" next chunk")
AssistantComplete(...)
```

However, the current LangGraph path uses `ainvoke()` and buffers a complete `AIMessage`. The repository already tracks a streaming-LLM roadmap item. Desktop implementation must reuse that backend work rather than introduce a second provider client only for the GUI.

Therefore:

- build the runtime/UI event shape for deltas now;
- test it with deterministic fake events;
- wire it to real provider streaming when the runtime streaming issue is implemented;
- until then, emit one buffered delta/complete pair without pretending it is true token streaming.

Cancellation must stop provider generation when supported and always suppress stale late results by turn ID.

---

# 11. Voice integration

Voice remains owned by Zara's existing runtime.

Desktop-visible states should derive from runtime events:

```text
Idle
Listening
Transcribing
Thinking
Speaking
Error
```

The UI should be able to request:

- start manual voice capture;
- stop capture;
- cancel current turn;
- mute/unmute speech output;
- open voice settings.

Wake-word mode may continue while all normal windows are hidden. Closing the chat window must not implicitly stop wake mode.

The GUI must not create a second microphone capture stack.

---

# 12. Runtime event bus

This is the architectural P0.

Zara currently has two related event systems:

1. `zara.actors` contains typed, turn-scoped runtime/latency events and a `TurnCoordinator` with cancellation.
2. `zara.pets.events` contains provider-neutral UI-ish events and `zara.pets.runtime_bridge` publishes them in-process and over ZMQ.

The desktop should generalize these into a runtime-level contract.

Example domain events:

```text
RuntimeStarted
RuntimeStopped
RuntimeError

TurnStarted
TurnCancelled

AssistantStarted
AssistantDelta
AssistantComplete
AssistantFailed

VoiceStateChanged
TranscriptReady

IntentResolved
PrologQueryCompleted

ToolQueued
ToolStarted
ToolProgress
ToolWaitingForUser
ToolCompleted
ToolFailed
ToolCancelled

ProviderChanged
ProviderUnavailable
NotificationRequested
```

All turn-scoped events must carry `turn_id`. Conversation events also carry `conversation_id`. Transport envelopes should provide sequence/timestamp metadata so consumers can order events without scraping logs.

## Boundary rule

Widgets consume runtime events and issue runtime commands. Widgets never reach through the bridge to mutate low-level assistant objects.

## Pets migration

Pets should become one adapter/consumer of generic runtime events. The runtime must no longer need to import `zara.pets.runtime_bridge` from the agent graph.

The existing ZMQ transport can remain for a standalone pet process; it becomes an adapter transport rather than the canonical runtime event definition.

---

# 13. Context attachments

Context must be explicit and inspectable.

Suggested model:

```text
ContextAttachment
  id
  kind              file | directory | clipboard | screenshot | window |
                    shell | git-repository | url | selected-text
  display_name
  source
  permission_scope  once | conversation | persistent
  metadata
  created_at
```

The composer shows active context as removable chips/cards.

Example:

```text
Context
[x] README.org
[x] zara/runtime/events.py
[x] Clipboard
[ ] Current screen

+ Add context
```

## Permission principles

- file attachment does not imply directory access;
- directory attachment does not imply unrestricted filesystem access;
- clipboard is read only on explicit attachment/action;
- screenshot/window access is user-triggered and respects desktop portal permission flows;
- shell output is attached explicitly or comes from a visible tool run;
- URL access goes through existing network/tool policy.

Existing context-management roadmap work should become the backend where appropriate; desktop code should supply UI/adapters, not create a competing context engine.

---

# 14. Tools

Tool execution needs a first-class structured model.

States:

```text
queued
running
waiting-for-user
completed
failed
cancelled
```

Normal chat rendering should be compact:

```text
Searching files...
✓ 17 files examined

Querying Prolog...
✓ 4 solutions
```

Expandable details may include:

- tool name;
- sanitized arguments;
- elapsed time;
- bounded output preview;
- exit status;
- error category;
- approval record.

Do not expose secrets or internal stack noise by default.

Side-effecting tools should be able to enter `waiting-for-user` and carry a structured approval request instead of inventing modal dialogs inside tool implementations.

---

# 15. Prolog integration

Prolog remains behind `PrologEngine`.

Important runtime constraint: PySWIP exposes process-wide SWI-Prolog state and Zara serializes access with a lock. A Qt widget must never call a Prolog query synchronously on the GUI thread.

The normal UI should only expose user-relevant results.

Optional advanced inspector:

```text
Reasoning
  Intent: set_timer
  Resolver: Prolog
  Result: timer(600)
  Status: completed
```

Debug facts/solutions can be expandable, bounded and sanitized. The primary chat must not become a Prolog IDE.

---

# 16. Plugins

Existing user plugins primarily register LangChain tools. Desktop extensions need a capability-oriented registration layer.

Potential descriptors:

```text
register_command(...)
register_context_provider(...)
register_tray_action(...)
register_notification_handler(...)
register_settings_section(...)
register_chat_action(...)
register_event_listener(...)
```

Plugins should return descriptors/callbacks through a controlled API. They should not receive arbitrary `QApplication`, tray, or window objects by default.

Permissions and crashes must be isolated at the plugin boundary. One failing plugin should not take down the main window or runtime.

---

# 17. Pet integration

Zarathushtra Pets is already optional and PySide6-based. Keep it optional.

When launched under Zara Desktop:

- Pet is controlled from the main tray;
- Pet consumes the same generic runtime events as chat/tray status;
- no second system tray should be required;
- pet position/scale/reduced-motion state continues to use its existing settings/storage;
- standalone `zara-pets` remains possible for compatibility.

## ChatGPT pet imports

The repository currently contains adapters named `ChatGPTSpriteV1` and `ChatGPTSpriteV2`, including assumptions about sprite sheet dimensions and a `pet.json` package. Research of current official OpenAI desktop documentation did not locate a published pet-asset schema defining those fields/dimensions.

Therefore these adapters must be treated as observed/reverse-engineered compatibility formats, **not** as a guaranteed official contract. Do not expand or silently accept new layouts based on guesses. Keep foreign formats isolated behind adapters, validate them strictly, and test against known fixtures.

---

# 18. Persistence

Use existing persistence surfaces:

- TOML/XDG config for configuration;
- SQLite `DatabaseManager` + migrations for durable desktop application data;
- existing memory subsystem for semantic/assistant memory;
- existing Pets state storage for sprite-position state unless/until unified deliberately.

Do not store API keys in conversation tables or diagnostic exports.

Conversation retention and deletion should be explicit operations and eventually coordinate with memory semantics rather than assuming deleting a chat means deleting all learned memories.

---

# 19. Security

Desktop convenience must not widen Zara's effective capabilities silently.

Required principles:

1. **Explicit capability acquisition.** Screenshot, clipboard, microphone, files and directories are separately visible capabilities.
2. **Least scope.** Prefer one file / one conversation / one screen capture over broad persistent permission.
3. **Structured side effects.** Shell/process/file-write/desktop-automation tools expose their intended action before approval where policy requires it.
4. **No secret rendering.** Tool args, logs and diagnostics have redaction before UI display/export.
5. **No implicit plugin trust.** Desktop plugin capability registration is allowlisted and auditable.
6. **Cancellation is authoritative.** A cancelled turn cannot update chat or trigger speech later.
7. **URL/network operations remain runtime tools.** The desktop does not create a second unrestricted fetch stack.

---

# 20. Concurrency

The Qt GUI thread must remain UI-only.

Recommended topology:

```text
Qt main thread
  |
  | queued commands / queued event delivery
  v
RuntimeHost thread
  |
  +-- owns asyncio loop / application service lifecycle
  +-- AgentManager async work
  +-- network providers
  +-- voice orchestration adapters
  |
  +--> existing worker threads/processes/actors
       - Pykka actors
       - Prolog serialized lock
       - audio callbacks
       - tool subprocesses
```

Do not move Zara's entire asyncio architecture onto the Qt event loop in P0.

QtAsyncio exists and may become useful for narrower integration, but coupling the full assistant runtime to the GUI loop would make headless modes harder to preserve and would increase the chance that provider/tool latency blocks UI behavior.

Cross-thread UI delivery uses Qt queued signals/slots. Long work never runs directly from a button handler.

## Cancellation

Build on `TurnCoordinator`:

- every user turn receives a turn ID;
- cancellation is idempotent;
- provider/tool tasks receive cancellation when supported;
- late events for cancelled turn IDs are rejected before UI/TTS side effects.

---

# 21. Linux, Wayland and X11 behavior

## Wayland

Assume stricter compositor control:

- global shortcut through XDG portal where available;
- screenshot/screen/window capture through portals where practical;
- do not assume the app may arbitrarily focus/position itself after an external event;
- use activation tokens supplied by the portal when available.

## X11

Support the same service interfaces with X11 implementations. Existing xdotool/pynput dependencies may help, but desktop UI code must not directly depend on them.

## Tray environments

`QSystemTrayIcon` behavior differs across GNOME/KDE/Xfce/etc. Treat unavailable activation reasons or absent tray support as degraded capability, not fatal startup failure.

## Multi-monitor

Reuse the Pet code's `QScreen.availableGeometry()` / `screenAt()` style. Remember logical placement conservatively and recover windows that would otherwise reopen offscreen after display changes.

## Suspend/resume

The runtime service should expose suspend/resume hooks so microphone/network/provider state can be revalidated rather than assuming long-lived handles remain valid.

---

# 22. Packaging

Nix remains authoritative.

Desired additions:

- `zara-desktop` Python entry point;
- `zara-desktop` Nix package/app;
- desktop `.desktop` launcher;
- installed icons/resources;
- Qt platform/plugin dependencies required for X11/Wayland;
- portal/DBus support needed by global shortcuts and screen capture;
- optional autostart integration.

Target workflow:

```text
nix develop
zara-desktop

# and packaged
nix run .#zara-desktop
```

Do not install ad-hoc global Python packages.

PySide6 is already present. If `pytest-qt` is adopted for GUI tests, add it to both Python packaging metadata and Nix in the same focused change.

---

# 23. Testing

## Unit tests

Cover:

- runtime event schemas and conversion;
- desktop state reducers/models;
- command palette registration/filtering;
- conversation persistence models;
- context attachment permission state;
- tray-state mapping;
- tool-state mapping;
- settings validation;
- shortcut backend selection;
- pet-event adapter.

## Qt tests

Use Qt's testing facilities, preferably pytest + `pytest-qt` if added consistently to Nix/packaging.

Cover:

- quick/full window creation;
- close-to-hide lifecycle;
- explicit quit;
- keyboard submit/newline/Escape;
- focus/autofocus;
- fake streaming updates;
- cancellation;
- tool state cards;
- settings changes;
- drag/drop;
- context chips;
- command palette navigation.

CI should use an offscreen Qt platform where possible. Tray integration itself must be abstracted enough to test without a real system tray host.

## Runtime integration

Use deterministic fake LLM/tool providers to validate the real desktop-runtime boundary.

Failure tests:

- LLM unavailable;
- Prolog unavailable;
- microphone unavailable;
- network offline;
- malformed tool result;
- plugin crash;
- runtime crash/restart;
- cancelled request;
- background task failure.

The invariant is: failure produces explicit state; it must not freeze or silently disappear.

---

# 24. Migration plan

## Stage 0 — Runtime-neutral events

Promote generic runtime events and stop making core agent code depend on `zara.pets.*`.

## Stage 1 — Desktop runtime host

Create application commands, dedicated runtime host/thread and Qt-safe event bridge.

## Stage 2 — Desktop shell and tray

Create `QApplication`, canonical tray, close-to-hide lifecycle and runtime status.

## Stage 3 — Conversation UI

Create shared models, full chat and deterministic streaming/tool rendering.

## Stage 4 — Quick Copilot

Add quick summon surface and conversation-preserving handoff.

## Stage 5 — Desktop context and voice

Integrate existing context roadmap, clipboard/files/screens, voice states and notifications.

## Stage 6 — Extensions and Pets

Move Pets onto generic events and unify the tray. Add command palette/settings/diagnostics/plugin descriptors.

## Stage 7 — Packaging/platform polish

Desktop file, autostart, Wayland/X11 edge cases, Windows/macOS backend stubs/implementation.

Headless Zara remains supported throughout.

---

# 25. Dependency-ordered implementation slices

The issue boundaries should follow current repository dependencies rather than one giant GUI branch.

| Priority | Slice | Depends on |
|---|---|---|
| P0 | Promote desktop-neutral runtime event contract and pet adapter | current `TurnCoordinator` |
| P0 | RuntimeHost + Qt/runtime command bridge | runtime events |
| P0 | PySide6 desktop shell + canonical tray lifecycle | RuntimeHost |
| P0 | Shared conversation model + full chat event rendering | RuntimeHost/events |
| P1 | Quick Copilot window + shared handoff | shell + conversation model |
| P1 | Linux global shortcut service (portal + X11 backend) | quick window |
| P1 | Context attachment desktop service/UI | existing context-management backend roadmap |
| P1 | Structured tool run + approval UI | events + conversation UI |
| P1 | Voice state + desktop notifications | shell + events |
| P2 | Command palette + desktop extension capability registry | shell + runtime commands |
| P2 | Settings + diagnostics | shell + shared config/runtime status |
| P2 | Unify Pets with desktop runtime events/tray | generic events + shell |
| P3 | Packaging, autostart and cross-platform polish | stable desktop shell |

Existing issues for runtime LLM streaming and context management should be dependencies/integration points, not duplicated as desktop-specific backend rewrites.

---

# Proposed application boundary

A concrete command/event vocabulary should be introduced incrementally.

## UI -> runtime commands

```text
SubmitTurn(conversation_id, text, context_ids)
CancelTurn(turn_id)
NewConversation()
StartVoice()
StopVoice()
MuteSpeech(enabled)
ApproveTool(tool_run_id)
RejectTool(tool_run_id)
AddContext(attachment)
RemoveContext(context_id)
RestartRuntime()
```

## Runtime -> UI events

```text
RuntimeStatusChanged
TurnStarted
AssistantStarted
AssistantDelta
AssistantComplete
AssistantFailed
VoiceStateChanged
TranscriptReady
IntentResolved
ToolQueued
ToolStarted
ToolProgress
ToolWaitingForUser
ToolCompleted
ToolFailed
ToolCancelled
NotificationRequested
TurnCancelled
RuntimeError
```

The precise classes should reuse/extend existing `zara.actors` semantics instead of cloning them.

---

# Startup, hide, quit and crash semantics

## Startup

1. Start `QApplication`.
2. Initialize minimal desktop state/tray immediately.
3. Start `RuntimeHost` off the GUI thread.
4. Publish component readiness as it becomes known.
5. Restore optional Pet and window state after shell is responsive.

## Hide

- closing Quick Copilot hides it;
- closing Full Chat normally hides the window;
- wake/runtime/tray continue;
- Pet continues if enabled.

## Quit

Explicit Quit:

1. stop accepting new turns;
2. cancel/drain active runtime work;
3. stop voice capture/TTS;
4. stop Pet adapter/overlay;
5. persist UI state;
6. close runtime services;
7. quit `QApplication`.

## Runtime crash

The GUI remains alive when feasible, changes tray/status to disconnected/error, exposes diagnostics and offers restart. A runtime crash must not make the application disappear silently.

## GUI crash

If the desktop shell and runtime initially share a process, both may exit. The architecture should not pretend otherwise. A later supervisor/separate-runtime process is possible, but it is unnecessary distributed-system complexity for P0.

---

# Diagnostics

Provide a sanitized diagnostics window/export containing:

- desktop version/build commit;
- runtime up/down state;
- configured provider names/models without secrets;
- Prolog loaded/unavailable state;
- microphone/voice readiness;
- enabled plugin names;
- recent bounded structured errors;
- relevant platform (`xcb`, `wayland`, etc.);
- tray/shortcut availability;
- redacted recent tool failure metadata.

Never export API keys, raw authorization headers or arbitrary environment variables.

---

# Research references

Primary external references used for interaction/technology decisions:

- Qt for Python / PySide6 official documentation and licensing.
- Qt `QSystemTrayIcon`, signal/slot, thread and QtAsyncio documentation.
- XDG Desktop Portal GlobalShortcuts specification.
- OpenAI official ChatGPT desktop/companion-window and screenshot documentation.
- Microsoft official Copilot desktop/Quick View documentation.
- Anthropic official Claude Desktop and extension documentation.
- Raycast official Quick AI / AI Chat / extensions manuals.
- Apple official Spotlight documentation.
- Perplexity official desktop/file context documentation.

The external products are pattern references only. Zara should preserve its own interaction model and security boundaries.

---

# Final target experience

```text
                    Zara always running
                            |
                       system tray
                            |
          +-----------------+-----------------+
          |                                   |
   global shortcut                         tray click
          |                                   |
          v                                   v
 +----------------+                   +----------------+
 | Quick Copilot  | ---- expand ----> | Full Zara Chat |
 +-------+--------+                   +-------+--------+
         |                                    |
         +----------------+-------------------+
                          |
                   Runtime boundary
                          |
        +----------+------+------+-----------+
        |          |             |           |
      Voice      Prolog         Tools        LLM
        |          |             |           |
        +----------+------+------+-----------+
                          |
                    runtime events
                          |
                   +------+------+
                   |             |
                 Desktop        Pet
```

The UI is a surface over Zara. The runtime remains Zara.