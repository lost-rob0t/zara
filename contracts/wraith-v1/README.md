# WRAITH/1

WRAITH/1 is Zara's runtime-neutral subagent contract.

It describes durable logical agent identity, parent/child relationships, tasks,
messages, budgets, and the binding to an already-selected `ZARA-RUNTIME/1`
runtime generation. It is not a second runtime registry and does not itself
execute tools, models, processes, or side effects.

## Authority split

```
Org / Prolog policy
    -> admits spawn, delegation, capabilities, budgets
WRAITH/1
    -> typed logical agent/task/message state
ZARA-RUNTIME/1
    -> selected execution runtime + generation fencing
backend
    -> Python/Pykka initially, other runtimes later
```

The active backend never becomes the durable identity of an agent. A Pykka
actor, subprocess, Prolog-RLM session, Android sidecar, or future Zara-language
actor is an executor for the Wraith logical record.

## Python wire types

The initial Python contract lives in `zara/wraith/contracts.py`.

- `AgentSpec`
- `RuntimeBinding`
- `Budget`
- `TaskRecord`
- `WraithMessage`

Wire projections contain only bounded identifiers, opaque references, enum
values, non-negative integer budgets, and positive runtime generations.

`canonical_json()` provides deterministic trace/fixture serialization.

## Prolog authority primitives

The first Prolog contract lives in `modules/wraith.pl`.

It defines:

- known roles, states, terminal states, and message types;
- bounded identifier/reference validation;
- budget validation;
- agent-spec validation;
- task lifecycle transitions;
- a minimal deterministic spawn-admission predicate.

These predicates grant no ambient authority. Later slices should resolve
capabilities and parent/child policy against Zara's canonical Prolog
configuration and permission state before asking a backend to execute anything.

## Lifecycle

Tasks begin in `created`.

Allowed transitions:

```
created -> ready | failed | cancelled
ready   -> running | failed | cancelled
running -> paused | completed | failed | cancelled
paused  -> running | failed | cancelled
```

`completed`, `failed`, and `cancelled` are terminal.

## Context snapshots

Wraith's initial contract was designed against exact read-only snapshots of
existing project code rather than inventing a parallel agent model:

- `lost-rob0t/prolog-rlm@9c475fe44c24ca58795c6b5cb5e002f2e2decc5e`
  - supervised mailbox processing and Prolog-native `agent_spawn*` /
    `agent_send*` APIs;
- `lost-rob0t/agentProlog@e0e02f05614265ae8ccfe707d26256cc2ef291e3`
  - product/profile composition above Prolog-RLM and the typed-subagent
    integration seam;
- `lost-rob0t/starintel-pro-actors@a5567bc983634df521533128093ddbd9c37940ab`
  - bounded Pykka supervisor/worker-pool patterns.

These repositories are context, not vendored dependencies. The intended local
development workflow is to fork or mirror them into isolated worktrees when Git
access is available.

## Next slices

1. Prolog-backed profile/capability/spawn policy.
2. Python/Pykka backend implementing the Wraith executor interface.
3. Persistent snapshots and recovery semantics.
4. Skill routing and typed tool delegation.
5. Desktop/Android agent-tree diagnostics.
6. #1098 scored trajectories and reward vectors.
7. Alternate backend implemented by the future Zara language.
