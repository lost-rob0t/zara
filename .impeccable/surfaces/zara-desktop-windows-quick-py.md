---
version: 1
slug: "zara-desktop-windows-quick-py"
primary_target: "zara/desktop/windows/quick.py"
related_targets: ["zara/desktop/windows/chat.py","zara/desktop/windows/settings.py"]
---

Scope and mode: Native Quick Copilot, Full Chat, and Settings; focused operate-and-read surfaces for conversation and configuration work.

Audience and job: A keyboard-first Zara user needs to summon the assistant, understand live runtime state, continue one conversation at two scales, personalize the full desktop, and maintain Prolog behavior without learning Prolog syntax.

Primary tasks: Submit or stop one streaming turn through the unified composer action; choose and persist a complete theme; tune canonical runtime settings; edit allowlisted Prolog source; add, edit, or delete supported facts in the actual XDG config.pl.

Proof and content: Real provider/runtime labels, incremental message state, canonical config.toml values, allowlisted source paths, loader-supported fact types, and the shared conversation trace. Never imply access Zara does not have.

Constraints: Preserve native Qt behavior, readable contrast in all five themes, Enter and Shift+Enter semantics, Escape-to-hide in Quick Copilot, explicit restart truth, atomic bounded writes, source syntax validation, and one runtime/configuration path.

Chosen direction: Signal Cabin remains the component and route grammar. Signal Cabin, Dotfiles Outrun, Nord, Dracula, and ChatGPT Neutral are complete semantic palettes over that grammar; color meaning travels by role, not by one fixed hue.

Memorable moment: The single upward-arrow action becomes a stop square while the same assistant message streams, while Settings makes the user's real Prolog configuration legible beside a guided facts list.

Unresolved decisions: None for this implementation.
