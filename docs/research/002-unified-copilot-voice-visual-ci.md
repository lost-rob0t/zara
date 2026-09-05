# Unified Copilot, Voice Conversation Continuity, and Visual CI

**Research baseline:** `16b1eda8dd4a0b1e4a3dba7742787e8a75192347`
**Date:** 2026-09-05
**Status:** design-ready research
**Primary target:** Linux desktop, preserving portable client/runtime boundaries

## Executive decision

The current desktop presentation split between `QuickCopilotWindow` and `FullChatWindow` should be retired.

Zara should expose **one canonical adaptive Copilot window** over the existing durable conversation service and daemon-client runtime boundary. The same process-owned surface should move between:

- `hidden`
- `compact`
- `expanded`

Quick Copilot is not a second or reduced assistant. It is the primary Zara desktop UI. Expanded mode exposes history, larger conversation layout, diagnostics/settings entry points, and other secondary panels without handing the conversation to a second top-level chat implementation.

The runtime, daemon/client transport, conversation store, tool lifecycle, Prolog integration, and voice stack remain shared and authoritative. This work is a presentation/lifecycle consolidation, not a new assistant runtime.

A second major decision follows from the current daemon-client migration: **voice session conversation ownership and automatic title/summary generation belong at the durable runtime/conversation boundary, not inside Qt widgets.** Desktop and Android should observe the same conversation metadata and message history.

A third decision is to add screenshot-producing CI immediately. Automated UI work should generate deterministic visual evidence in every relevant run so reviewers can inspect real rendered states instead of treating widget tests as proof of acceptable layout.

---

## 1. Current state and root cause

### 1.1 Two top-level chat surfaces are still process-owned

`DesktopController` currently constructs both:

- `FullChatWindow`
- `QuickCopilotWindow`

Both receive the same `ConversationService`, and the controller manually resynchronizes them after conversation and runtime events. Quick-to-full expansion selects the same durable conversation ID in Full Chat and then hides Quick Copilot.

This is internally consistent, but it creates duplicated presentation state, duplicated message renderers, duplicated geometry/focus behavior, duplicated header/composer decisions, and a permanent concept of handoff between two UIs.

The original desktop research explicitly chose separate windows because compact and full modes have different geometry/focus semantics. That assumption is now the main design decision to supersede. The product requirement has changed: Quick Copilot is no longer a temporary launcher for a separate full application. It is the application.

### 1.2 Current Quick Copilot chrome is too fragmented

The compact surface currently contains separate permanent regions for:

- Zara brand
- `Quick Copilot` title
- provider/model label
- New Chat button
- Full Chat button
- Settings button
- runtime-status rail
- command error region
- message scroll area
- composer shell

This produces unnecessary vertical and horizontal fragmentation in a window whose main job is conversation. The status rail and multiple text/button regions compete with the messages and composer.

The current compact view also limits itself to the last six messages, reinforcing the idea that it is a partial projection rather than a native conversation surface.

### 1.3 Tray activation is wired as show, not toggle

`QuickCopilotWindow` already implements `toggle_visibility()` and close-to-hide behavior.

`ZaraTray`, however, emits `quick_requested` from primary activation, and the controller connects that signal to `show_quick_copilot()`. Therefore a second tray click does not use the existing visibility toggle path.

The desired behavior requires no new window lifecycle concept:

```text
primary tray activation
  visible   -> hide
  hidden    -> show + raise + focus composer
```

Explicit `Quit Zara` remains process shutdown. Window close remains hide.

### 1.4 The conversation model is durable enough to support consolidation

The existing desktop conversation records already provide:

- stable conversation ID
- title
- timestamps
- provider/model metadata
- durable ordered messages
- active turn state

This is the correct state authority to preserve. The UI does not need a new store to become adaptive.

### 1.5 Runtime voice events already carry the important correlation fields

The runtime event base includes optional `turn_id` and `conversation_id`.

The event vocabulary already includes:

- `VoiceStateChanged`
- `VoiceSpeechStarted`
- `VoiceTranscriptPartial`
- `VoiceSpeechEnded`
- `VoiceTranscriptFinal`
- `AssistantStarted`
- `AssistantDelta`
- `AssistantComplete`
- tool lifecycle events
- audio output events

Therefore voice-to-chat integration should not invent a second transcript event system. The missing piece is a durable voice-session-to-conversation binding and commit policy for final transcripts.

### 1.6 CI already has the artifact transport needed for screenshots

The main CI job defines `ARTIFACT_DIR` and uploads `artifacts/` with `if: always()`.

The missing work is fixture rendering and stable file naming, not a new artifact backend.

---

## 2. Target desktop architecture

### 2.1 One canonical `CopilotWindow`

Replace the presentation split with one process-owned top-level window:

```text
DesktopController
  |
  +-- ZaraTray
  +-- CopilotWindow  <--- exactly one chat top-level window
  +-- SettingsWindow / diagnostics as secondary workspaces
  |
  +-- ConversationService
  +-- QtRuntimeBridge / ZaraClient
```

Suggested internal shape:

```text
CopilotWindow
  |
  +-- CopilotHeader
  +-- ConversationViewport
  +-- Composer
  +-- optional side/history panel
  +-- optional detail/tool/context panels
```

The window owns a presentation mode enum such as:

```text
COMPACT
EXPANDED
```

`hidden` remains ordinary QWidget visibility state rather than a third renderer.

The same conversation widgets and composer survive mode changes. No messages are copied. No second renderer must be synchronized.

### 2.2 Mode behavior

#### Compact

Purpose: immediate summon, short focused interaction, voice status, fast hide.

Recommended characteristics:

- approximately current Quick Copilot geometry
- keyboard-first
- minimal header
- no permanent side history
- no permanent runtime rail
- Escape hides
- optional always-on-top/tool behavior according to desktop platform research
- same complete conversation state, with viewport simply showing the latest scroll position

#### Expanded

Purpose: durable chat browsing and management without opening another chat implementation.

Recommended characteristics:

- normal resizable application geometry
- conversation history selector/panel
- wider message measure
- room for tool/context details
- settings/diagnostics entry points
- no `Full Chat` handoff button

Mode-specific geometry should be saved independently so compact placement does not overwrite expanded placement.

Qt window-flag changes can recreate/hide a native top-level window. Implementation research should therefore minimize flag churn. Prefer one stable top-level type if acceptable; if compact-only flags are required, wrap flag changes in an explicit mode transition that restores visibility/focus and is covered by Qt tests.

### 2.3 Remove duplicated presentation code

The implementation should extract reusable pieces from `windows/chat.py` before deleting the second top-level path. Candidate reusable parts include:

- conversation/history selector
- message viewport behavior
- composer actions
- status/error presentation
- conversation rename/delete actions

Completion should leave one canonical message rendering path for normal desktop chat.

---

## 3. Native-feeling visual specification

### 3.1 Main rule

Conversation content gets the visual priority. Status and product chrome are secondary.

### 3.2 Header

Compact header should normally contain only:

- current conversation title or Zara when untitled
- a compact runtime/connection indicator
- one overflow/action affordance when necessary

Do not permanently display separate `ZARA`, `Quick Copilot`, provider/model, New Chat, Full Chat, and Settings labels across the top row.

Provider/model remains inspectable via status/overflow/settings and may be shown transiently when changed.

### 3.3 Runtime status

Remove the permanent full-width runtime rail in normal healthy operation.

Use a small state indicator for:

- connected/ready
- reconnecting
- degraded
- disconnected/error

Show verbose status detail only when the state is abnormal or explicitly expanded.

### 3.4 Messages

Use natural message grouping rather than a separate heavy card for every row.

Requirements:

- bounded readable text measure
- tighter vertical rhythm
- consecutive same-role messages may group visually
- code/tool blocks remain structured
- streaming state is visible without a large spinner region
- errors are near the affected turn
- user and assistant are distinguishable without excessive boxes
- scrolling never introduces permanent blank regions around short conversations

### 3.5 Composer

Composer is anchored to the bottom and remains visible during ordinary scrolling/resizing.

Requirements:

- one text area
- one primary send/stop control
- optional attachment/voice/context controls only when available
- compact minimum height
- controlled growth for multiline input
- `Enter` submit, `Shift+Enter` newline
- Escape hides in compact mode

### 3.6 Scrollbars

Prefer overlay/subtle scrollbars or a style that avoids a prominent permanent track. Do not remove keyboard/mouse-wheel scroll accessibility.

### 3.7 Accessibility

Every visual reduction must preserve:

- keyboard focus visibility
- accessible names/descriptions for icon-only controls
- sufficient contrast
- logical tab order
- screen-reader discoverability of runtime/error state
- no color-only distinction for error/active/approval state

---

## 4. Voice sessions become first-class chats

### 4.1 Conversation ownership

A voice conversation session must have exactly one durable `conversation_id`.

Recommended lifecycle:

```text
conversation mode enter
  -> create or explicitly resume durable conversation C
  -> bind voice-session ID/generation to C

final STT utterance
  -> append one USER message to C
  -> route normal semantic/agent turn using C

assistant/tool events
  -> carry C
  -> append/update the same durable conversation

conversation mode exit
  -> finalize session metadata
  -> enqueue title/summary generation if eligible
```

Partial transcripts are ephemeral display state and must never create duplicate durable user messages.

### 4.2 Session selection rules

- Voice invoked from no explicit chat: create a new voice conversation by default.
- Voice invoked while a specific conversation is explicitly active and the user requests continuation: bind to that conversation.
- Background/system voice invocation must not guess an unrelated visible desktop conversation merely because it was last focused.
- Reconnect/restart must use explicit session/conversation continuity information, not UI presence.

### 4.3 Stale-event rule

Every voice stream/turn must be generation/correlation checked.

A late final transcript, assistant delta, or audio chunk from an old stream/session cannot mutate/play in the newly active conversation.

### 4.4 UI representation

When the desktop Copilot is visible during voice mode:

- partial transcript may appear as a temporary live composition row
- final transcript converts atomically into the normal user message
- assistant text streams into the normal assistant message
- voice state is shown compactly near the composer/header

When the UI is hidden, the same durable messages are committed and appear when Copilot is later shown.

---

## 5. Automatic title and summary

### 5.1 Authority

Now that desktop is moving onto the daemon client, automatic title/summary generation should be owned by the durable runtime/conversation service, not by `CopilotWindow`.

This avoids desktop/Android races and ensures all clients observe the same metadata.

### 5.2 Metadata model

Extend conversation metadata with fields equivalent to:

```text
summary: str
summary_updated_at: timestamp | null
title_source: auto | user | fallback
metadata_generation: int or revision marker
```

Exact schema naming is implementation-owned.

### 5.3 Rules

- A user rename permanently wins unless the user explicitly requests re-auto-title.
- Automatic generation never inserts a fake assistant message into history.
- Generation is bounded to a safe transcript/message projection.
- Conversation-mode shutdown does not wait for the model call.
- A failed model call produces a deterministic fallback title derived from early user text.
- Retry is idempotent and revision-aware.
- A stale summarization result cannot overwrite metadata after additional conversation mutation without an explicit policy.
- Empty/trivial sessions do not need a model-generated title.

### 5.4 Suggested trigger

Run title/summary finalization after voice conversation exit and optionally after a text conversation reaches a minimum amount of content.

For voice, the first completed session is the primary required trigger.

---

## 6. Desktop visibility control and dotfiles contract

### 6.1 Tray

Primary tray activation is a strict visibility toggle:

```text
if CopilotWindow visible:
    hide
else:
    show + raise + activate + focus composer
```

Close button and Escape in compact mode use the same hide path.

### 6.2 External command

Expose a stable command:

```text
zara --toggle-desktop
```

Desired behavior:

```text
desktop owner absent  -> start desktop owner and show
owner present/hidden  -> request show
owner present/visible -> request hide
```

Do not use the remote assistant `ZARA/1` protocol for client-local window visibility. UI visibility is local process control, not assistant semantic state.

Recommended Linux-first implementation: a small per-user local desktop-control endpoint under `XDG_RUNTIME_DIR`, owned by the desktop process, with singleton semantics and a bounded command vocabulary (`show`, `hide`, `toggle`, optionally `status`). Qt `QLocalServer`/`QLocalSocket` or an equivalent narrow Unix-domain socket service is preferable to adding this concern to the server protocol.

Security requirements:

- per-user runtime location/permissions
- no arbitrary command execution
- stale endpoint recovery
- duplicate desktop process prevention
- bounded connect timeout
- explicit exit status for unavailable/broken owner

### 6.3 Dotfiles integration

The user's canonical Qtile configuration is literate: `.config/qtile/qtile-ai.org` tangles `.config/qtile/config.py`.

Therefore the persistent dotfiles change should be made in the Org source, not only directly in generated `config.py`.

The Qtile surface should invoke only the stable Zara command, for example:

```text
zara --toggle-desktop
```

It must not contain Zara window-class matching or reach into Qt internals.

The exact UX can be a bar dropdown/menu item or shortcut according to the existing Qtile dropdown UI helpers. The important contract is one stable executable action.

---

## 7. Screenshot-producing CI

### 7.1 Feasibility

Qt `QWidget.grab()` renders a widget and its children into a `QPixmap`. `pytest-qt` also provides `qtbot.screenshot(widget)` for capturing a widget during tests.

The repository already uploads `artifacts/` from the main CI job, so screenshots can be generated without a new artifact transport.

### 7.2 Deterministic visual fixture harness

Create a UI fixture module that constructs the Copilot against fake/deterministic conversation/runtime state. It must not require:

- network
- real daemon
- LLM provider
- microphone
- system tray
- live wall-clock content

Each fixture explicitly controls:

- theme
- DPI/font
- window size
- conversation messages
- runtime status
- streaming/tool/voice state

### 7.3 Required screenshots

At minimum:

```text
artifacts/ui/copilot-empty-compact.png
artifacts/ui/copilot-short-chat-compact.png
artifacts/ui/copilot-long-wrap-compact.png
artifacts/ui/copilot-streaming.png
artifacts/ui/copilot-error.png
artifacts/ui/copilot-disconnected.png
artifacts/ui/copilot-tool-running.png
artifacts/ui/copilot-tool-approval.png
artifacts/ui/copilot-voice-listening.png
artifacts/ui/copilot-voice-partial.png
artifacts/ui/copilot-expanded.png
artifacts/ui/copilot-history.png
artifacts/ui/copilot-smallest-supported.png
```

Also emit a small machine-readable manifest containing fixture name, logical state, dimensions, theme, source commit, and PNG path.

### 7.4 CI rendering environment

Prefer a controlled Xvfb-backed Linux job/profile for review screenshots, with the font and Qt packages coming from Nix. `offscreen` may remain useful for logic tests, but visual artifacts should use the rendering path proven most stable in the repository.

Pin/normalize:

- theme
- font family shipped/selected by the Nix environment
- font size
- device pixel ratio where possible
- locale
- timestamps/content
- widget dimensions

### 7.5 Initial gating policy

Do **not** make exact PNG equality the only UI gate initially.

Hard-gate structural invariants such as:

- composer remains visible
- no child widget exceeds viewport unexpectedly
- minimum size works
- message viewport has nonzero useful area
- controls do not overlap
- status/error text is bounded
- compact/expanded transitions preserve conversation ID and text

Always upload screenshots for review.

After rendering is sufficiently deterministic, add perceptual/golden comparison with an intentional baseline-update workflow rather than raw brittle byte equality.

---

## 8. Automated visual reviewer loop

The screenshot artifacts should be consumable by autonomous reviewers.

A UI implementation iteration should produce:

1. deterministic fixture state
2. rendered PNG
3. manifest
4. reviewer inspection
5. findings attached to the issue/PR
6. implementation correction
7. new render

Reviewer rubric:

- excessive gaps
- clipped text
- overlapping controls
- hidden composer
- bad message width
- awkward empty state
- inconsistent spacing
- unreadable status/error state
- oversized chrome
- broken compact/expanded geometry
- poor contrast
- scrollbar/layout regressions

Automated widget tests prove behavior. Screenshot review proves that the behavior is presented acceptably. Neither substitutes for the other.

---

## 9. Migration sequence

### Slice A — unified surface + visual evidence loop

First implementation should:

- create canonical adaptive `CopilotWindow`
- reuse/extract Full Chat history functionality
- remove Full Chat handoff from the primary UX
- make tray activation a true toggle
- preserve one conversation ID through compact/expanded transitions
- add deterministic screenshot fixtures and CI artifact upload
- keep the old Full Chat implementation only as temporary migration code if necessary

This slice is intentionally first so all later styling work has visual review evidence.

### Slice B — native layout overhaul

- remove permanent runtime rail
- simplify header
- tighten spacing/message grouping
- improve composer
- integrate history panel in expanded mode
- visually prove compact, expanded, streaming, error, disconnected, tool, and voice states

### Slice C — voice conversation binding

- introduce explicit voice-session -> conversation binding
- commit final transcripts exactly once
- propagate conversation ID through assistant/tool/audio events
- prove hidden-UI and reconnect behavior

### Slice D — title/summary metadata

- schema migration
- runtime-owned asynchronous finalization
- manual-title precedence
- deterministic fallback
- multi-client race/idempotency tests

### Slice E — external toggle + dotfiles

- local desktop-control endpoint
- `zara --toggle-desktop`
- singleton/race tests
- Qtile literate config integration using the stable command

---

## 10. Rejected alternatives

### Keep Quick and Full as separate windows and only restyle them

Rejected. It preserves the synchronization/handoff architecture that now conflicts with the desired product model and duplicates presentation behavior indefinitely.

### Rewrite desktop in Qt Quick before fixing ownership

Rejected. Qt Quick may be useful later, but changing rendering technology does not solve duplicate top-level surfaces, voice conversation ownership, or lifecycle semantics.

### Put voice transcripts in a separate voice-history table

Rejected. Voice is a modality of the same conversation system, not a separate product history.

### Generate titles in the desktop client

Rejected. With daemon and multiple clients, metadata should be durable/runtime-owned so Android and desktop cannot race or disagree.

### Send `toggle desktop` over the remote Zara daemon protocol

Rejected. Local window visibility is a client process concern and should not pollute authenticated assistant semantics or depend on server availability.

### Exact-pixel screenshot failure from day one

Rejected. Qt/font/raster differences can create noise before the render environment is proven deterministic. Start with guaranteed artifact generation plus structural assertions, then graduate to perceptual/golden gating.

---

## 11. Adversarial test matrix

### Surface/lifecycle

- repeated rapid tray clicks
- toggle while minimized
- close then toggle
- Escape while composer focused
- compact -> expanded -> compact preserves conversation/composer state
- monitor removed while hidden
- saved geometry entirely offscreen
- Wayland activation denied/degraded
- tray unavailable
- desktop control socket stale after crash
- concurrent `--toggle-desktop` invocations

### Conversation

- switch conversations while old assistant delta arrives
- delete/rename active conversation while compact
- user rename races with auto-title result
- daemon reconnect with same conversation
- stale previous-session event rejected

### Voice

- partial transcript never committed
- final transcript committed once
- duplicate final event idempotent
- UI hidden for full session
- UI shown mid-session
- conversation mode exits during provider response
- reconnect between transcript final and assistant response
- old audio/assistant event cannot mutate new conversation
- two devices/principals remain isolated

### Metadata generation

- empty session
- one-word session
- provider timeout
- restart during summary job
- user rename before auto result
- new messages appended before old summary result
- retry after transient failure
- malicious/oversized transcript projection bounded

### Visual CI

- screenshot job cannot reach network
- artifacts emitted on test failure
- deterministic fixed fixture content
- smallest supported window
- long unbroken text
- code block
- large tool error
- high message count
- missing theme/font fails clearly rather than silently substituting an unknown layout

---

## 12. Issue breakdown

Recommended issue structure:

1. **EPIC — Unified native Copilot, voice-chat continuity, and visual CI**
2. **P0 — Collapse Quick/Full Chat into one adaptive Copilot and add screenshot CI**
3. **P1 — Make voice conversation mode write first-class durable chats and auto-finalize title/summary**
4. **P1 — Add local desktop control IPC, true tray toggle, and `zara --toggle-desktop`**
5. **Dotfiles — Add Zara Copilot dropdown/action through the stable toggle command**

Existing issues #87, #88, #89 and #92 integrate into the unified surface rather than continuing to target two separate chat windows. #133 remains the daemon/client continuity authority.

---

## Completion rule

The work is complete when Zara has one canonical native desktop Copilot whose compact and expanded modes render the same durable conversation state; tray/close/external activation behave predictably; voice sessions appear as ordinary durable chats with safe automatic metadata finalization; and every significant UI change produces deterministic screenshot artifacts that automated reviewers can actually inspect.
