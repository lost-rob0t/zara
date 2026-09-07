# Zara Android — canonical symbolic workspace design

Status: **FROZEN IMPLEMENTATION TARGET**

Canonical visual reference: [`design/reference-2026-09-07.svg`](design/reference-2026-09-07.svg)

Tracking epic: [#648](https://github.com/lost-rob0t/zara/issues/648)

This document and the reference image are the Android UI authority. The current plain Material `ZaraApp.kt` is an implementation baseline, **not** the target design.

## Product shape

Zara Android is a chat-first symbolic assistant. It should feel like a compact personal operating environment rather than a generic Material demo or a ChatGPT clone.

The approved reference freezes three key states:

1. **Empty chat / symbolic home** — chat composer remains primary; otherwise-empty space may show the abstract Zara sigil, bounded symbolic status, small action tiles, and restrained Outrun ambience.
2. **Full-height navigation drawer** — secondary surfaces plus pinned/recent conversations live here, not on the main chat screen.
3. **Active conversation** — symbolic home decoration collapses away; messages own the vertical space; the composer remains compact and always reachable.

## Visual contract

Default theme: **Outrun**.

- near-black/navy ground;
- restrained magenta, cyan, violet and blue accents;
- thin luminous outlines and small status lights;
- no giant gradient/glass cards;
- no permanent giant `ZARA` wordmark;
- abstract sigil/avatar instead of portrait or mascot art;
- dense enough to feel capable, sparse enough that chat stays dominant;
- background ambience must never reduce text contrast or hit targets;
- decorative symbolic material disappears once it competes with real conversation.

The reference image is compositional authority, not a bitmap skin. Rebuild the UI with native Compose components and semantic tokens.

## Semantic theme tokens

All screens consume the same semantic token layer. Do not hard-code theme-specific colors inside screen composables.

Minimum roles:

```text
background
surface
surface_elevated
surface_input
border
border_active
primary
secondary
accent_magenta
accent_cyan
text
text_muted
success
warning
error
focus
ambient_glow
```

Built-in themes required by #648:

1. Outrun — default
2. StarIntel — black/gold
3. Midnight — dark violet/blue
4. Terminal — black/green
5. Light — neutral light
6. System — follows Android

Every built-in must render the same component hierarchy. Reduced-glow mode must preserve hierarchy without relying on bloom.

## App shell

The shell owns:

- compact top bar;
- left full-height drawer;
- route/breadcrumb presentation where useful;
- conversation/history selection;
- local/remote status presentation;
- bottom composer;
- in-app navigation state that survives rotation/recreation.

### Top bar

The primary chat surface uses:

- menu button on the left;
- optional `Chat / Project / Context` breadcrumb in the center area;
- compact abstract Zara/runtime action on the right.

Do not put a giant product title above every route.

### Drawer

Ordered destinations:

```text
Chat
Logic
Voice
Projects
Remote
Scheduled
Plugins
Themes
Diagnostics
Settings
About
```

Below primary destinations:

- Pinned conversations with `See all`;
- Recent conversations with `See all`;
- new-chat affordance;
- small local/remote runtime state.

Pinned and recent conversations never permanently consume main-chat vertical space.

## Chat surface

### Empty state

Before the first real turn, the main surface may show:

- abstract Zara sigil;
- `Symbolic intelligence / On your terms` style copy;
- bounded runtime labels such as `LOCAL`, `PRIVATE`, `EXTENSIBLE`;
- `Run a query`;
- `Inspect logic`;
- `Build something`;
- `Analyze data`;
- `Summarize`;
- `Explore`;
- restrained symbolic landscape/constellation ambience.

These are real affordances, not decorative buttons. A tile must either invoke a supported action or be hidden/disabled with an explicit reason.

### Active chat

Once a conversation contains real user/assistant content:

- empty-state hero, tiles and large ambience collapse;
- messages receive the main viewport;
- assistant messages may use a subtle bordered semantic block but not giant cards;
- user messages remain compact and visually distinct;
- streaming/tool progress can use the left semantic spine/status dots shown in the reference;
- prose, code, lists and symbolic results remain readable at small widths;
- conversation state survives process recreation where the runtime supports it.

### Composer

The composer is always a primary control and must remain reachable with IME open.

Required affordance slots:

- add/attach/action menu;
- text input;
- contextual tool/settings control;
- microphone;
- voice-mode action.

Keep it compact. Do not turn it into a thick multi-row slab unless content expansion requires it.

## Logic

Logic is a first-class product surface. It exposes the symbolic runtime instead of hiding Prolog as an implementation detail.

Minimum views:

- effective configuration;
- user facts;
- runtime facts;
- intent rules;
- plugin rules;
- sources/modules.

Provide a read-only source viewer with:

- module/source identity;
- actual origin/path where available;
- line numbers;
- syntax highlighting;
- load state;
- provenance;
- copy;
- open-file handoff where the platform/runtime can support it safely.

Distinguish source text from effective loaded runtime state.

## Voice

Voice uses the same shell and tokens. It must expose real state for:

- mic permission;
- listening/capturing;
- partial transcript;
- final transcript;
- assistant audio playback;
- cancel/barge-in where supported;
- offline/local versus remote voice path;
- explicit degraded/error state.

The large circular voice action shown in the reference is an action/state control, not merely decoration.

## Projects

Projects provide persistent user-facing work contexts. Zara owns the Android UX first.

Minimum Android contract:

- list/open project;
- project-scoped chat/context;
- project breadcrumb in chat;
- bounded project metadata/status;
- explicit source/runtime scope;
- no implicit filesystem crawl.

If the reusable symbolic runtime lacks a generic project/source contract, implement the smallest coherent Zara-side adapter first and open a Prolog-RLM extraction/reuse issue that points to the working Zara contract and tests.

## Remote

Remote is broader than a single connection settings page.

Expose:

- local symbolic runtime state;
- configured remote endpoint/profile;
- authentication/enrollment readiness;
- connected/reconnecting/offline/degraded states;
- which capabilities are local versus remote;
- safe reconnect and profile actions.

Per #622, remote failure must not tombstone the whole app when local symbolic capability is available.

## Scheduled

Scheduled work is a user-facing projection of Zara long-horizon tasks.

Minimum Android contract:

- list upcoming/active/completed/failed tasks;
- inspect schedule and next run;
- inspect bounded state/evidence;
- pause/resume/cancel where authorized;
- create/edit only through the existing validated task/scheduling boundary;
- no second Android-only scheduler.

When a generic reusable scheduler/task protocol is missing upstream, prove it in Zara first and file the Prolog-RLM extraction issue against the concrete Zara behavior.

## Plugins

Plugins are first-class in the drawer.

Minimum Android contract:

- installed/discovered plugin list;
- local versus remote source;
- enabled/disabled state;
- capability/permission summary;
- configuration entry point where safe;
- health/diagnostic state;
- clear unavailable/degraded state;
- no secret values rendered by diagnostics.

Zara remains the first implementation/proving ground. Generic Prolog-owned plugin/tool selection, capability or metadata contracts should be extracted into Prolog-RLM only after Zara has executable tests demonstrating the reusable seam.

## Themes

Themes screen provides preview cards and immediate semantic-token switching. Theme state survives process recreation and restart.

Do not ship six divergent component implementations. One component system, six token sets.

## Diagnostics

Diagnostics is an operator surface, not a dump of internals.

Expose bounded useful state:

- app/source build identity;
- local symbolic runtime readiness;
- remote connection/session state;
- conversation identity;
- voice stream state;
- plugin health summary;
- task runner state;
- last bounded failures;
- safe copy/export actions.

Never expose credentials, raw private plugin config, transcripts by default, or unbounded logs.

## Settings

Settings owns user-controlled configuration. Runtime facts and source code belong under Logic/Diagnostics unless they are directly editable user settings.

Use grouped mobile forms with the same tokens and compact hierarchy as the rest of the app.

## Implementation boundary and reuse rule

**Implement and prove product behavior in Zara first.** Do not stall the Android product waiting for Prolog-RLM to invent an abstraction in the dark.

When a Zara implementation reveals a genuinely domain-neutral symbolic/runtime contract:

1. land or have an executable Zara implementation plus tests/fixtures;
2. document the reusable seam and what is Zara-specific;
3. open a focused `lost-rob0t/prolog-rlm` issue that links directly to the Zara issue/code/test contract;
4. Prolog-RLM extracts/generalizes the contract without taking ownership of Android/UI/product policy;
5. Zara later consumes the upstream reusable contract when doing so reduces duplication without regressing behavior.

Dependency direction after extraction:

```text
Zara product / Android UI
        -> thin Zara adapter
        -> reusable Prolog-RLM contract
```

Never move Compose UI, Android lifecycle, visual themes, product navigation, or Zara-specific policy into Prolog-RLM.

## Screenshot-driven implementation

Every visual implementation issue must produce deterministic screenshot evidence for at least:

- empty symbolic home;
- drawer open;
- active short chat;
- long/wrapped chat;
- streaming/tool-running state;
- disconnected/degraded local-capable state;
- voice state;
- smallest supported layout with IME/composer visible;
- one representative alternate theme.

Compare screenshots against the canonical reference for hierarchy, density, geometry and visual language. Pixel identity is not required; structural drift is.

## Acceptance gate

The Android UI overhaul is not complete until:

- the canonical reference is represented by native Compose components;
- the drawer and chat behavior match the frozen hierarchy;
- all required routes are real surfaces or explicitly gated placeholders tied to implementation issues;
- empty-state symbolic content collapses after the first real turn;
- local/offline symbolic capability remains usable where supported;
- Logic exposes real Prolog/runtime data;
- project/scheduled/plugin/theme/diagnostic flows use shared architecture rather than screen-local hacks;
- accessibility, font scale, rotation/recreation and IME behavior are tested;
- deterministic screenshot artifacts exist;
- Android/Core exact-head CI is green.
