# ZARA-EXPERT/1 adapter compatibility inventory (#1238)

This inventory pins the producer/consumer boundaries used by the portable/native
expert adapter work. It is intentionally an inventory, not another registry,
planner, provider runtime, permission system, budget ledger, effect executor, or
conversation store.

## Pinned revisions inspected

| Component | Revision | Role |
| --- | --- | --- |
| `lost-rob0t/zara` | `4a7ad7673910e901a09a9e79c81e60bd2aeed8aa` | canonical ZARA-EXPERT/1 registry/activation/invocation owner |
| `lost-rob0t/zara-plugins` | `00d1c569f025894c70fb65f9ab161c16557b4316` | downstream registered-predicate host/adapters |
| `lost-rob0t/prolog-rlm` | `4715b5ee53d0e7a26c55705ea85c0d16c3916504` | reusable generic expert/language/style semantics |
| `lost-rob0t/dotfiles` PR #308 | `4fc75059cd0eb242559e0ec2786e7d78875a310d` | data-only StyleExpert producer |
| `lost-rob0t/zara` PR #1282 | `6c1b9280f97adf087e36335d48ae11d4b9e569dd` | Android pure-symbolic conversation consumer |

## Authority map

| Surface | Path / owner | Status | Contract / arity | Authority / effect seam |
| --- | --- | --- | --- | --- |
| Core expert registry | `zara/experts.py` + `zara/_experts_v1.py` | implemented | `activate(principal, workspace, expert_id, ...)`, `invoke_request(request)`, `generation`, `runtime_generation` | Sole Core registry/lifecycle/invocation/usage owner. Effects remain outside the adapter behind canonical Zara capability/approval/tool execution. |
| Consumer invocation port | `zara/expert_port.py` | implemented by this slice | `active_activation(principal, workspace, expert_id)`, `invoke(request)`, `current_registry_generation()`, `current_runtime_generation()` | Read/invoke-only projection over the existing registry. It cannot register, activate, reload, deactivate, grant authority, or execute arbitrary goals. |
| Android consumer contract | PR #1282 `android/app/src/main/java/ai/zara/app/expert/CanonicalExpertInvocationPort.kt` | implemented consumer seam | `activeActivation/3`, `invoke/1`, registry/runtime generation reads | Consumer-only. Missing owner fails closed. Must not be backed by `LocalZaraServer.query(...)` or a locally-created `ActivationHandle`. |
| Android production owner composition | #897 + #663 + #1238 -> `AndroidAppSession.canonicalExpertInvocationPort()` | missing / blocked | same four consumer operations | Must bind the already-owned activation and registered-predicate authority. No Android-local registry/issuer/executor. |
| Registered-predicate plugin host | `zara-plugins/plugins/zara-expert/` | implemented downstream host; exact adapter capabilities remain independently gated | documented bounded expert host callbacks / registered predicates | #663 remains the non-reconstructible executable-predicate authority requirement. Plugin code must not widen predicate selection or bypass canonical effects. |
| Generic expert composition | `lost-rob0t/prolog-rlm` #376 and #492-#504 | upstream reusable semantics | registry/applicable/select/invoke/explain and style/language semantics as exposed by the pinned public API | Zara consumes this through adapters; it is not copied into a product-local planner/provider runtime. |
| Style producer | `lost-rob0t/dotfiles` PR #308 | ready but governance-blocked | inert `style_rules/4`, `resolve`/`explain` producer data | Data-only; no precedence, provider, permission, scheduler, budget, effect, or conversation authority. |

## Native/portable binding rule

The portable/native adapter may expose only an already-existing canonical owner:

```text
#897 activation owner
    + #663/#1273 registered expert execution authority
    -> one CanonicalExpertInvocationPort
    -> Android/Desktop/native consumers
```

The adapter is not allowed to mint `ActivationHandle` values, choose arbitrary
Prolog goals, create a second registry, or reinterpret handler success as verified
effect success. Effect-dependent success still requires fresh canonical
postcondition evidence.

## Explicitly rejected

- `object : CanonicalExpertInvocationPort` in Android UI composition;
- local `ActivationHandle(...)` construction outside the lifecycle owner;
- routing expert execution through `LocalZaraServer.query(...)` or another raw-goal path;
- a second expert registry, scheduler/planner, permission plane, provider runtime,
  budget ledger, evidence owner, or conversation-history store;
- claiming remote/model-free execution without enforceable zero-model capability
  and an exact zero usage ledger;
- treating MCP/A2A metadata or tool annotations as authority.

## Verification commands

The focused Core adapter gate is:

```sh
pytest -q t/test_expert_invocation_port.py
```

The existing Core contract regression gates remain authoritative:

```sh
pytest -q \
  t/test_expert_registry_v1.py \
  t/test_expert_registry_symbolic_fences.py \
  t/test_expert_registry_descriptor_limits.py
./scripts/test-expert-contract.sh
```

Full repository promotion still requires the repository-wide gate plus the
installed Android/Desktop zero-provider conversation acceptance owned by #1254.
