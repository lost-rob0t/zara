# ZARA-RUNTIME/1 conformance contract

This directory is the transport-neutral contract shared by Zara desktop/Core and Android.

`ZARA-RUNTIME/1` is the only runtime registry/selection identity in Zara. `Auto`, `Local`, and `Remote` remain routing policy and are not runtime ids. Runtime discovery is observation only and never grants tool, plugin, principal, secret, filesystem, shell, Python-eval, or Prolog `call/1` authority.

A runtime descriptor is selectable only when all of the following are true:

- `protocol == ZARA-RUNTIME/1`;
- `installed == true`;
- `available == true`;
- health is not `failed` or `stopped`.

Unknown protocol majors fail closed. Duplicate runtime ids make a discovery refresh fail atomically. A refresh that republishes the same descriptor set is a semantic no-op even when observation order differs: it must not advance the canonical generation or stale an in-flight invocation binding. A refresh with any descriptor-set change advances the generation; if that change makes the selected runtime missing or unselectable, active selection is cleared. This keeps polling harmless while still fencing stale runtime/session publications whenever discovery state actually changes.

Invocation work is bound to the exact selected runtime generation and to an opaque host-issued `ctx:<id>` reference. Optional `cap:<id>` values are references into Zara's canonical host authority only; they are not permissions and the runtime registry never resolves or grants them. Reusing a binding under another context, runtime identity, or stale generation fails closed. Principal, plugin, approval, secret, filesystem, shell, eval, and Prolog authority therefore remain outside the runtime registry and must be re-checked by their canonical owners before privileged execution.

The shared fixture `descriptors.tsv` is intentionally dependency-free so Python and Kotlin conformance tests consume the exact same observations. It contains a selectable built-in Zara runtime, an absent optional Prolog-RLM runtime, and an incompatible future-protocol runtime. The fixture does **not** imply Prolog-RLM is installed.

`runtime-descriptor.schema.json` is the wire-shape schema for descriptor projection. Concrete runtime adapters may add transport-private state internally, but UI and conversation code must consume only bounded descriptor/selection snapshots from the canonical registry.
