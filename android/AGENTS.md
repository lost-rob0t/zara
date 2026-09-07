# AGENTS.md — Zara Android

This file supplements the repository root `AGENTS.md` for all work under `android/`.

## Canonical UI authority

Before changing Android UI, navigation, feature surfaces, screenshots, or theme code, read:

1. `android/DESIGN.md`
2. `android/design/reference-2026-09-07.svg`
3. GitHub epic #648 and the specific child issue being consumed

The reference is now **frozen implementation input**. Do not continue treating it as an external mockup or optional inspiration.

## Worker rule

Android workers are expected to move the app toward the frozen reference while implementing real behavior behind the visible surfaces.

Do not:

- preserve the current plain bottom-navigation Material shell merely because it already exists;
- create a ChatGPT clone;
- render fake feature buttons without behavior or an explicit disabled state tied to an issue;
- hide Prolog/Logic as an internal-only concern;
- add an Android-only plugin runtime, scheduler, task engine, authority layer, history owner, or second symbolic runtime;
- wait for a hypothetical Prolog-RLM abstraction before proving a needed product contract in Zara.

## Zara-first, Prolog-RLM reuse

For missing reusable symbolic/runtime capabilities:

1. implement the smallest coherent product behavior in Zara with RED-first tests;
2. identify the domain-neutral seam from executable behavior, not speculation;
3. open/update a focused `lost-rob0t/prolog-rlm` issue linking to the Zara issue and concrete implementation/tests;
4. keep Android/Compose/product policy in Zara;
5. later replace Zara-local generic logic with the upstream reusable contract when it is green and compatible.

A Prolog-RLM issue must not become a blocker for Zara UI/product progress unless Zara literally cannot implement the behavior safely without the missing runtime primitive.

## Screenshot gate

Every visual child issue under #648 must generate deterministic screenshot evidence. At minimum include the states required by `android/DESIGN.md` that the issue touches.

Review screenshots for:

- hierarchy against the canonical reference;
- composer visibility with IME;
- density and wasted space;
- drawer geometry/history placement;
- empty-state collapse after first turn;
- readable long messages/code;
- theme-token correctness;
- minimum-width/font-scale/accessibility failures;
- local/offline versus remote status clarity.

Do not claim visual completion from Compose tests alone.

## Implementation order

Unless issue dependencies say otherwise, prefer this order:

1. shared semantic theme/component primitives;
2. app shell + drawer + navigation state;
3. chat empty/active states + compact composer;
4. Logic/source inspection;
5. Voice/Remote/Diagnostics restyle and state projection;
6. Projects;
7. Scheduled;
8. Plugins;
9. Themes/settings polish;
10. accessibility, screenshot matrix, real-device hardening.

Keep each slice independently testable and usable. Preserve #622 offline-first behavior and all root repository security/runtime invariants.
