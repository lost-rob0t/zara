# Product

<!-- impeccable:product-schema 1 -->

## Platform

adaptive

## Users

Zara is primarily for a hands-on user who wants one persistent assistant available across native surfaces. Linux desktop is the most mature client today; Android is an active native client effort.

The desktop user needs a fast keyboard-first summon surface for interruptions, the ability to expand into durable conversation/history without changing assistants, honest runtime state, and voice interaction that remains part of the same conversation.

## Product Purpose

Zara combines symbolic Prolog semantics, voice, LLM conversation, tools, durable history, plugins, and native client interaction behind one canonical runtime/service boundary.

Success means a user can summon Zara quickly, understand what the runtime is doing, continue the same conversation across compact/expanded desktop presentation and voice/text interaction, reconnect without invented continuity, and use other clients such as Android without creating a second assistant runtime.

## Positioning

**One conversation, one runtime, many native surfaces.**

The earlier product model treated Quick Copilot and Full Chat as separate top-level projections with a handoff between them. Current design research supersedes that assumption. The target desktop product is one adaptive Copilot window that moves between compact and expanded presentation while retaining the same conversation renderer, composer, runtime state, and durable conversation identity.

This is a presentation/lifecycle consolidation, not a new runtime. `zara-server`, `ZaraClient`, conversations, tools, Prolog, voice, plugins, and provider execution remain shared authorities below the UI.

## Operating Context

- Native PySide6 desktop application on Linux, with X11/Wayland constraints.
- Keyboard-first compact Copilot summoned over the current desktop.
- Expanded mode for history, conversation management, diagnostics/settings entry points, and richer context without handing off to a second chat implementation.
- Background operation through a tray while the long-lived daemon/runtime can remain active when chat windows are hidden.
- Voice sessions that should bind to durable conversations at the runtime/conversation boundary.
- Native Android client under active development using the same `ZARA/1` service semantics.

## Capabilities and Constraints

- Preserve exact durable conversation identity across compact/expanded desktop mode changes.
- Preserve Enter to send, Shift+Enter for newline, Escape-to-hide in compact mode, streaming updates, cancellation, history, provider/runtime information, and error recovery.
- Primary tray activation should toggle the Copilot surface rather than always forcing it visible.
- Do not keep permanent duplicated Quick/Full message renderers once the adaptive-window migration lands.
- Voice and typed messages should share durable conversation state; automatic voice-session title/summary work belongs below Qt widgets.
- Offer complete selectable desktop themes, including Dotfiles Outrun, Nord, Dracula, and the canonical Zara theme, without fragmenting component behavior.
- Let users tune canonical TOML settings, edit approved Prolog source with syntax highlighting, and add supported managed facts through controls that write to the real XDG `config.pl`.
- Keep desktop UI native; do not replace it with a browser shell or create a second runtime/provider/database/tool configuration path.
- Android remains a native client rather than a web shell or mobile-only runtime fork.
- Never imply access to screen, clipboard, files, applications, microphone, or device capabilities without the corresponding real capability and user/runtime policy.
- Unavailable capabilities remain absent or disabled rather than fabricated.
- Security-sensitive daemon features remain subject to explicit release/adversarial gates; implementation slices are not automatically production claims.

## Brand Commitments

The product is Zara / Zarathushtra: a capable native copilot with a direct, intelligent voice and visible symbolic/runtime structure.

The desktop redesign should feel modern and unusually well crafted without cloning Microsoft Copilot, ChatGPT, Raycast, or another assistant's visual identity. Conversation gets visual priority; runtime truth remains visible without burying the user in permanent chrome.

## Evidence on Hand

- Current implementation and project overview in `README.org`.
- Runtime/client ownership in `docs/architecture.org` and `docs/server.org`.
- Desktop implementation under `zara/desktop/` and regression tests under `t/`.
- Current desktop/voice/visual-CI direction in `docs/research/002-unified-copilot-voice-visual-ci.md`.
- Desktop semantic design system in `DESIGN.md`.
- Android implementation under `android/` and status documentation in `wiki/android.org`.

Historical desktop design documents may contain assumptions superseded by newer dated research. Detailed design text is not implementation evidence by itself.

## Product Principles

- One conversation, one runtime, many surfaces.
- Compact and expanded are modes of the Copilot, not different assistants.
- Fast to summon, calm to operate, explicit about state.
- Native desktop/mobile behavior over imitation web chrome.
- Deterministic intent and runtime truth remain visible through honest status.
- Symbolic decisions should be explicit and testable where practical.
- Voice is conversation, not an ephemeral side mode.
- Reconnect must report truthfully what resumed and what did not.
- Keyboard access and readable contrast are part of the product, not polish.
- Screenshot-producing visual CI is part of desktop quality, not optional decoration.

## Accessibility & Inclusion

Maintain WCAG AA text contrast, visible keyboard focus, meaningful control labels, readable error/connection states, and full keyboard operation across desktop surfaces. Android surfaces should preserve equivalent semantic labels and platform accessibility behavior as they mature.
