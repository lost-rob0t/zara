# Zara Desktop Copilot: Current Architecture

**Status:** canonical desktop architecture after issue #324
**Primary target:** Linux desktop, with portable runtime/client boundaries
**UI toolkit:** PySide6 / Qt Widgets
**Detailed research:** `docs/research/002-unified-copilot-voice-visual-ci.md` and RAGE log `rage/324-unified-copilot.org`

> Historical note: earlier versions of this document intentionally designed Quick Copilot and Full Chat as separate top-level Qt windows. That decision is superseded by #323/#324. Git history preserves the old research; new implementation and review work must use the architecture below.

## Executive decision

Zara Desktop has **one canonical process-owned Copilot chat surface**.

```text
                         Zara Desktop Shell
                  (one QApplication, one main tray)
                              |
                 +------------+-------------+
                 |                          |
             ZaraTray                 CopilotWindow
                                      compact/expanded
                                           |
                                  ConversationService
                                           |
                                  QtRuntimeBridge
                                           |
                                     ZaraClient
                                           |
                                      zara-server
```

`CopilotWindow` has presentation modes rather than separate chat implementations:

```text
hidden <-> compact <-> expanded
```

`hidden` is ordinary QWidget visibility. Compact and expanded modes keep the exact same conversation renderer, composer, durable conversation ID, active turn, cancellation state, and runtime event stream.

There is no Quick-to-Full message copy or top-level-window handoff in the canonical path.

## Why the old split was removed

The previous implementation constructed both `QuickCopilotWindow` and `FullChatWindow` and manually synchronized them through `DesktopController` over one `ConversationService`.

That avoided duplicate durable storage, but it still duplicated:

- top-level window lifecycle;
- focus and show/hide behavior;
- geometry handling;
- message rendering decisions;
- composer presentation;
- provider/status presentation;
- controller forwarding paths;
- tests for handoff between two UIs.

The product direction also changed: Quick Copilot is no longer a temporary launcher into a separate chat application. **The Copilot is the application.** Expanded mode simply reveals more of the same surface.

## Canonical Copilot

The implementation promotes the mature durable Full Chat behavior into `CopilotWindow` and retains compact-window lifecycle ideas from the old Quick implementation.

### Shared behavior in both modes

- one durable `ConversationService`;
- complete selected conversation, not an arbitrary last-N projection;
- streaming assistant message mutation;
- cancellation tied to the canonical active turn;
- tool/runtime event rendering through the existing event boundary;
- durable new-chat and rename behavior;
- one composer;
- one message renderer;
- close-to-hide desktop semantics;
- settings/diagnostics remain secondary workspaces, not parallel assistant state.

### Compact mode

Compact mode is the default summon experience.

It should prioritize conversation and composer and suppress secondary healthy-state chrome:

- history sidebar hidden;
- provider/model text hidden in the normal healthy view;
- full-width runtime rail hidden for healthy `idle`/`ready` states;
- actionable starting/listening/thinking/tool/approval/disconnected/error states remain visible;
- tighter content margins and message spacing;
- smaller composer bounds;
- Escape hides the Copilot;
- compact geometry is persisted independently.

Compact mode uses a stable ordinary top-level Qt window. #324 deliberately does not toggle native `Tool` / `WindowStaysOnTopHint` flags during mode transitions because changing top-level flags can recreate or hide native windows on Wayland/X11. If future UX work requires those flags, it must add explicit platform/lifecycle tests.

### Expanded mode

Expanded mode reveals durable management controls around the same conversation:

- conversation history/search sidebar;
- new/rename/settings controls;
- provider/model detail;
- full runtime status/actions;
- larger independent geometry;
- the exact same message widgets and composer state.

The compatibility action previously called “Open Full Chat” is interpreted as “expand the canonical Copilot.” It must not construct another chat window.

## Controller ownership

Normal `DesktopController` construction owns exactly one chat top-level:

```text
controller.window          -> CopilotWindow
controller.copilot_window  -> same object
controller.quick_window    -> same object (temporary compatibility alias)
```

The identity equality of these references is intentional. Compatibility names must not reintroduce a second renderer.

Runtime events are reduced once, applied once to `ConversationService`, and rendered once by the canonical window.

## Runtime boundary

The desktop remains a client of Zara's shared application/runtime contracts.

Qt widgets may:

- submit typed runtime commands;
- consume provider-neutral runtime events;
- render durable conversation state;
- request local presentation changes.

Qt widgets must not directly call:

- Prolog engines;
- LLM provider clients;
- tool implementations;
- daemon internals;
- a second microphone/voice stack.

The daemon/client migration from #133 remains authoritative. Hiding or closing ordinary desktop windows must not imply stopping the shared daemon.

## Conversation architecture

The durable conversation store remains authoritative for:

- conversation IDs;
- titles;
- messages;
- provider/model metadata;
- timestamps;
- turn/message status.

Desktop presentation mode is not persisted as conversation data.

Voice-session conversation ownership and automatic title/summary finalization are owned by #325 at the runtime/conversation boundary so desktop and Android can observe the same history and metadata.

## Tray and activation

#324 establishes the one-window ownership needed for reliable activation. #326 owns the final local-control contract:

```text
zara --toggle-desktop
```

and true primary tray behavior:

```text
visible -> hide
hidden  -> show + raise + focus
```

Client-local window visibility does not belong in remote `ZARA/1` assistant semantics.

## Visual CI

Native UI work must produce inspectable screenshots, not just passing widget assertions.

The deterministic renderer lives at:

```text
python -m zara.desktop.visual_fixtures
```

The full regression gate runs it offline with `QT_QPA_PLATFORM=offscreen` before broad pytest and writes:

```text
$ARTIFACT_DIR/ui/
  copilot-empty-compact.png
  copilot-short-chat-compact.png
  copilot-long-wrap-compact.png
  copilot-streaming.png
  copilot-error.png
  copilot-disconnected.png
  copilot-tool-running.png
  copilot-tool-approval.png
  copilot-voice-listening.png
  copilot-voice-partial.png
  copilot-expanded.png
  copilot-history.png
  copilot-smallest-supported.png
  manifest.json
```

Fixtures use fake/deterministic state only: no network, real provider, daemon, microphone, user database, or live wall-clock content.

### Initial gating policy

Hard-gate structural behavior such as:

- one controller-owned chat instance;
- state preservation across compact/expanded mode transitions;
- nonzero usable message/composer area at minimum size;
- hidden secondary healthy-state chrome in compact mode;
- durable history available in expanded mode;
- long text remains bounded by the viewport;
- cancellation and runtime events survive mode transitions.

Always upload screenshots for review. Do not use byte-exact PNG equality as the sole quality gate until the Nix/Qt/font raster environment proves stable enough for meaningful golden/perceptual comparison.

## Appearance and settings

The existing semantic theme registry remains shared across desktop surfaces. Built-in themes include Signal Cabin, Dotfiles Outrun, Nord, Dracula, and ChatGPT Neutral.

Settings continue to write through Zara's canonical XDG configuration and Prolog configuration paths. This architecture change does not create a desktop-only configuration database or bypass configuration validation.

## Accessibility invariants

Reducing chrome must not reduce accessibility.

- icon-only controls require accessible names;
- keyboard focus remains visible;
- tab order remains logical;
- runtime/error state is not color-only;
- text remains readable at supported sizes;
- scrolling remains keyboard/mouse accessible;
- compact mode must not hide actionable failure/approval state.

## Non-goals for #324

- no Qt Quick/QML rewrite;
- no second conversation database;
- no new daemon protocol;
- no new LLM/provider client;
- no voice-history implementation (#325);
- no final tray/local IPC toggle implementation (#326);
- no arbitrary plugin access to Qt internals;
- no exact-pixel golden baseline as the only CI criterion.

## Completion rule

The desktop architecture is conformant when normal Zara construction owns one adaptive Copilot, compact and expanded modes preserve the exact conversation state without handoff, and every UI regression run produces deterministic screenshot evidence that autonomous or human reviewers can inspect.
