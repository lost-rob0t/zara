# Product

<!-- impeccable:product-schema 1 -->

## Platform

adaptive

## Users

Zara is primarily for a hands-on Linux user who summons an assistant while working at the desktop. The user needs a fast keyboard-first answer surface for interruptions and a durable full conversation surface for longer work.

## Product Purpose

Zara combines voice, Prolog intent resolution, LLM conversation, tools, persistent history, and native desktop interaction in one assistant. Success means a user can summon it quickly, understand what the runtime is doing, continue the same conversation in Full Chat, and stop or recover work without losing context.

## Positioning

Quick Copilot and Full Chat are two native projections of the same conversation and runtime state, not separate assistant implementations. Zara can resolve deterministic intents through Prolog before falling back to the conversational agent while preserving one canonical runtime boundary.

## Operating Context

- Native PySide6 desktop application on Linux, with X11 and Wayland constraints.
- Keyboard-first Quick Copilot summoned over the current desktop.
- Persistent Full Chat for history, search, rename, streaming responses, code, cancellation, and runtime diagnostics.
- Background operation through a tray and a shared runtime that can remain active while windows are hidden.

## Capabilities and Constraints

- Preserve exact-conversation handoff from Quick Copilot to Full Chat.
- Preserve Enter to send, Shift+Enter for a newline, Escape to hide Quick Copilot, streaming updates, cancellation, history, provider information, runtime state, and error recovery.
- Offer complete selectable desktop themes, including the user's Dotfiles Outrun palette, Nord, Dracula, and a neutral conversational theme, without fragmenting component behavior.
- Let users tune canonical TOML settings, edit approved Prolog source with syntax highlighting, and add every supported user fact through guided controls that write to the real XDG `config.pl`.
- Keep the desktop UI native; do not replace it with a browser shell or create a second runtime, provider, database, or tool configuration path.
- Never imply access to screen, clipboard, files, or applications without explicit user action.
- Unavailable capabilities remain absent or disabled rather than fabricated.

## Brand Commitments

The product is Zara / Zarathushtra: a capable desktop copilot with a direct, intelligent voice. The redesign must feel modern and unusually well crafted without cloning Microsoft Copilot, ChatGPT, Raycast, or another assistant's visual identity. The explicit user mandate is a complete Copilot redesign that looks genuinely excellent.

## Evidence on Hand

- Implemented behavior and product copy in `README.org`, `zara/desktop/`, and the desktop tests under `t/`.
- Architecture and UX constraints in `docs/desktop-copilot-design.md`.
- No approved logo, illustration system, customer proof, commercial claims, or existing visual design system is present in the repository; future work must not invent them.

## Product Principles

- One conversation, many surfaces.
- Fast to summon, calm to operate, explicit about state.
- Native desktop behavior over imitation web chrome.
- Deterministic intent and runtime truth remain visible through honest status.
- Keyboard access and readable contrast are part of the product, not polish.

## Accessibility & Inclusion

Maintain WCAG AA text contrast, visible keyboard focus, meaningful control labels, readable error states, and full keyboard operation across the desktop surfaces.
