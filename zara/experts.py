"""Public ZARA-EXPERT/1 registry facade with host-owned dispatch fences.

The contract implementation lives in :mod:`zara._experts_v1`; this facade keeps
``zara.experts`` as the stable public import while tightening the pure-symbolic
host boundary. There is still exactly one registry instance/state machine: the
subclass only adapts trusted host dispatch, enforces nested delegation authority,
and validates the shared usage ledger before accepting a terminal result.
"""

from __future__ import annotations

import inspect
import threading
from collections.abc import Mapping
from dataclasses import replace
from typing import Any, Optional

from . import _experts_v1 as _impl
from ._experts_v1 import *  # noqa: F401,F403


_ORIGINAL_HANDLER_ATTR = "__zara_original_expert_handler__"
_RESERVED_HOST_INPUTS = frozenset({"expert_operation"})


class _DelegationFrame:
    """Thread-local authority and budget inherited by nested expert calls."""

    __slots__ = (
        "delegated_model_calls",
        "delegation_policy",
        "expert_id",
        "invocation_id",
        "max_output_bytes",
        "max_results",
        "principal",
        "remaining_model_calls",
        "timeout_ms",
        "workspace",
    )

    def __init__(
        self,
        *,
        expert_id: str,
        invocation_id: str,
        principal: str,
        workspace: str,
        delegation_policy: DelegationPolicy,
        limits: ExpertLimits,
    ) -> None:
        self.expert_id = expert_id
        self.invocation_id = invocation_id
        self.principal = principal
        self.workspace = workspace
        self.delegation_policy = delegation_policy
        self.timeout_ms = limits.timeout_ms
        self.max_results = limits.max_results
        self.max_output_bytes = limits.max_output_bytes
        self.remaining_model_calls = limits.max_model_calls
        self.delegated_model_calls = 0


class ExpertRegistry(_impl.ExpertRegistry):
    """Canonical registry with dispatch, delegation, generation and usage fences."""

    def __init__(self, *args: Any, **kwargs: Any) -> None:
        super().__init__(*args, **kwargs)
        # Expert handlers may synchronously delegate back into this registry.
        # Reentrancy preserves the one canonical state machine without forcing a
        # second dispatcher or out-of-band composition path.
        self._lock = threading.RLock()
        self._delegation_state = threading.local()
        # These are lineage indexes over the canonical invocation records, not a
        # second scheduler/history store. They exist solely so root cancellation
        # can fence every still-live nested invocation atomically.
        self._invocation_parent: dict[str, str] = {}
        self._invocation_children: dict[str, set[str]] = {}

    def _delegation_stack(self) -> list[_DelegationFrame]:
        stack = getattr(self._delegation_state, "stack", None)
        if stack is None:
            stack = []
            self._delegation_state.stack = stack
        return stack

    def _delegation_parent(self) -> Optional[_DelegationFrame]:
        stack = self._delegation_stack()
        return stack[-1] if stack else None

    @staticmethod
    def _handler_declares_host_operation(handler: Any) -> bool:
        try:
            parameter = inspect.signature(handler).parameters.get("expert_operation")
        except (TypeError, ValueError):
            return False
        return parameter is not None and parameter.kind is not inspect.Parameter.POSITIONAL_ONLY

    @staticmethod
    def _original_handler(handler: Any) -> Any:
        return getattr(handler, _ORIGINAL_HANDLER_ATTR, handler)

    def _validate_handler(
        self,
        descriptor: ExpertDescriptor,
        handler: Optional[Any],
    ) -> None:
        for operation in descriptor.operations:
            reserved = _RESERVED_HOST_INPUTS.intersection(
                spec.name for spec in operation.input_fields
            )
            if reserved:
                field_name = sorted(reserved)[0]
                raise ExpertInvalidInputError(
                    f"operation input field {field_name!r} is reserved host metadata"
                )

        if handler is None:
            return
        if not callable(handler):
            raise ExpertInvalidInputError("expert handler must be callable")
        handler = self._original_handler(handler)
        try:
            signature = inspect.signature(handler)
        except (TypeError, ValueError) as error:
            raise ExpertInvalidInputError(
                f"expert handler signature is not inspectable: {error}"
            ) from error

        declares_operation = self._handler_declares_host_operation(handler)
        for operation in descriptor.operations:
            payload = {spec.name: None for spec in operation.input_fields}
            if declares_operation:
                payload = {"expert_operation": operation.operation_id, **payload}
            try:
                signature.bind(**payload)
            except TypeError as error:
                raise ExpertInvalidInputError(
                    f"handler for expert {descriptor.expert_id!r} does not accept "
                    f"the declared input fields of {operation.operation_id!r}: {error}"
                ) from error

    def _admit_limits_unlocked(
        self,
        handle: ActivationHandle,
        limits: Optional[ExpertLimits],
    ) -> ExpertLimits:
        """Intersect caller, descriptor and active parent-delegation ceilings."""

        if limits is None:
            requested = ExpertLimits()
        elif isinstance(limits, ExpertLimits):
            requested = limits
        else:
            raise ExpertInvalidInputError("limits must be an ExpertLimits instance")

        descriptor = self._descriptors.get(handle.expert_id)
        descriptor_limits = descriptor.resource_limits if descriptor is not None else None
        if descriptor_limits is None:
            admitted = requested
        else:
            admitted = ExpertLimits(
                timeout_ms=min(requested.timeout_ms, descriptor_limits.timeout_ms),
                max_results=min(requested.max_results, descriptor_limits.max_results),
                max_output_bytes=min(
                    requested.max_output_bytes,
                    descriptor_limits.max_output_bytes,
                ),
                max_model_calls=min(
                    requested.max_model_calls,
                    descriptor_limits.max_model_calls,
                ),
            )

        parent = self._delegation_parent()
        if parent is None:
            return admitted
        parent_invocation = self._invocations.get(parent.invocation_id)
        if parent_invocation is None or parent_invocation.state != "dispatching":
            raise ExpertDeniedError(
                "nested expert delegation denied: parent invocation is no longer live"
            )
        if parent.delegation_policy is DelegationPolicy.NEVER:
            raise ExpertDeniedError(
                f"expert {parent.expert_id!r} does not permit expert delegation"
            )
        if handle.principal != parent.principal or handle.workspace != parent.workspace:
            raise ExpertDeniedError(
                "nested expert delegation cannot cross principal/workspace identity"
            )

        return ExpertLimits(
            timeout_ms=min(admitted.timeout_ms, parent.timeout_ms),
            max_results=min(admitted.max_results, parent.max_results),
            max_output_bytes=min(admitted.max_output_bytes, parent.max_output_bytes),
            max_model_calls=min(
                admitted.max_model_calls,
                parent.remaining_model_calls,
            ),
        )

    def _invoke_unlocked(
        self,
        handle: ActivationHandle,
        expert_operation: str,
        input: dict[str, Any],
        limits: Optional[ExpertLimits],
        idempotency_key: Optional[str],
        request_id: Optional[str],
    ) -> ExpertResult:
        """Invoke through the existing state machine with live authority fences.

        ``expert_operation`` stays trusted host metadata. Handler execution does
        not monopolize the registry lock: cancellation, reload, and generation
        changes may proceed while user code runs. Late success is then fenced
        before it can be accepted as current state. Nested calls inherit the
        parent's authority and remaining model-call budget automatically.
        """

        admitted_registry_generation = self._registry_generation
        admitted_runtime_generation = self._runtime_generation
        delegation_parent = self._delegation_parent()
        admitted_limits = self._admit_limits_unlocked(handle, limits)
        registered_handler = self._handlers.get(handle.expert_id)
        handler = (
            self._original_handler(registered_handler)
            if registered_handler is not None
            else None
        )
        raw_outcome: dict[str, Any] = {}
        injected = False
        dispatch: Optional[Any] = None

        if handler is not None:
            injected = self._handler_declares_host_operation(handler)

            def dispatch_handler(**payload: Any) -> Any:
                invocation_id = next(reversed(self._invocations))
                invocation = self._invocations[invocation_id]
                if (
                    invocation.expert_id != handle.expert_id
                    or invocation.expert_operation != expert_operation
                    or invocation.state != "dispatching"
                ):
                    raise ExpertInvalidInputError(
                        "expert dispatch lost its canonical invocation record"
                    )

                descriptor = self._descriptors.get(handle.expert_id)
                if descriptor is None:
                    raise ExpertDeniedError(
                        "expert dispatch lost its canonical descriptor"
                    )
                if delegation_parent is not None:
                    parent_invocation_id = delegation_parent.invocation_id
                    parent_invocation = self._invocations.get(parent_invocation_id)
                    if (
                        parent_invocation is None
                        or parent_invocation.state != "dispatching"
                    ):
                        denied = {
                            "verdict": "cancelled",
                            "data": {},
                            "evidence_refs": [],
                            "usage": {"model_calls": 0},
                            "effect_receipts": [],
                        }
                        raw_outcome["value"] = denied
                        raw_outcome["cancelled"] = True
                        return denied
                    self._invocation_parent[invocation_id] = parent_invocation_id
                    self._invocation_children.setdefault(
                        parent_invocation_id, set()
                    ).add(invocation_id)

                frame = _DelegationFrame(
                    expert_id=handle.expert_id,
                    invocation_id=invocation_id,
                    principal=handle.principal,
                    workspace=handle.workspace,
                    delegation_policy=descriptor.delegation_policy,
                    limits=admitted_limits,
                )
                raw_outcome["delegation_frame"] = frame
                stack = self._delegation_stack()
                stack.append(frame)

                self._lock.release()
                try:
                    if injected:
                        outcome = handler(
                            expert_operation=expert_operation,
                            **payload,
                        )
                    else:
                        outcome = handler(**payload)
                finally:
                    self._lock.acquire()
                    popped = stack.pop()
                    if popped is not frame:
                        stack.clear()
                        raise ExpertDeniedError(
                            "expert delegation stack lost canonical invocation order"
                        )

                raw_outcome["value"] = outcome
                current = self._invocations.get(invocation_id)
                if current is not None and current.state == "cancelled":
                    raw_outcome["cancelled"] = True
                    receipts = (
                        outcome.get("effect_receipts", ())
                        if isinstance(outcome, Mapping)
                        else ()
                    )
                    return {
                        "verdict": "cancelled",
                        "data": {},
                        "evidence_refs": [],
                        "usage": {"model_calls": 0},
                        "effect_receipts": receipts,
                    }

                if (
                    self._registry_generation != admitted_registry_generation
                    or self._runtime_generation != admitted_runtime_generation
                ):
                    raw_outcome["stale"] = True
                    receipts = (
                        outcome.get("effect_receipts", ())
                        if isinstance(outcome, Mapping)
                        else ()
                    )
                    return {
                        "verdict": "error",
                        "data": {},
                        "evidence_refs": [],
                        "usage": {"model_calls": 0},
                        "effect_receipts": receipts,
                    }

                return outcome

            setattr(dispatch_handler, _ORIGINAL_HANDLER_ATTR, handler)
            dispatch = dispatch_handler
            self._handlers[handle.expert_id] = dispatch_handler

        try:
            result = super()._invoke_unlocked(
                handle,
                expert_operation,
                input,
                admitted_limits,
                idempotency_key,
                request_id,
            )
        finally:
            if (
                handler is not None
                and dispatch is not None
                and self._handlers.get(handle.expert_id) is dispatch
            ):
                self._handlers[handle.expert_id] = handler

        if result.replayed:
            usage: Any = result.usage
            handler_terminal = True
        else:
            outcome = raw_outcome.get("value")
            usage = outcome.get("usage") if isinstance(outcome, Mapping) else None
            handler_terminal = False
            if isinstance(outcome, Mapping):
                raw_verdict = outcome.get("verdict")
                if isinstance(raw_verdict, str):
                    try:
                        ExpertVerdict(raw_verdict)
                    except ValueError:
                        pass
                    else:
                        handler_terminal = True

        require_actual_usage = (
            result.replayed
            or handler_terminal
            or bool(raw_outcome.get("cancelled"))
            or bool(raw_outcome.get("stale"))
        )
        if not isinstance(usage, Mapping) or "model_calls" not in usage:
            if not require_actual_usage:
                return result
            self._discard_invalid_success(
                result,
                handle,
                expert_operation,
                idempotency_key,
            )
            raise ExpertInvalidInputError(
                "terminal expert outcome must explicitly report usage.model_calls"
            )

        model_calls = usage["model_calls"]
        if type(model_calls) is not int or model_calls < 0:
            self._discard_invalid_success(
                result,
                handle,
                expert_operation,
                idempotency_key,
            )
            raise ExpertInvalidInputError(
                "reported expert usage.model_calls must be a non-negative built-in integer"
            )

        if result.replayed:
            aggregate_model_calls = model_calls
            charge_model_calls = 0
        else:
            frame = raw_outcome.get("delegation_frame")
            delegated_model_calls = (
                frame.delegated_model_calls
                if isinstance(frame, _DelegationFrame)
                else 0
            )
            aggregate_model_calls = model_calls + delegated_model_calls
            charge_model_calls = aggregate_model_calls

        if delegation_parent is not None and charge_model_calls:
            if charge_model_calls > delegation_parent.remaining_model_calls:
                # Actual work already happened. Exhaust the inherited allowance and
                # preserve the full consumed count before failing closed so a caller
                # cannot catch this error and immediately spend the same budget again.
                delegation_parent.delegated_model_calls += charge_model_calls
                delegation_parent.remaining_model_calls = 0
                self._discard_invalid_success(
                    result,
                    handle,
                    expert_operation,
                    idempotency_key,
                )
                raise ExpertBudgetExceededError(
                    "delegated expert aggregate usage exceeds parent model-call budget"
                )
            delegation_parent.remaining_model_calls -= charge_model_calls
            delegation_parent.delegated_model_calls += charge_model_calls

        if aggregate_model_calls > admitted_limits.max_model_calls:
            self._discard_invalid_success(
                result,
                handle,
                expert_operation,
                idempotency_key,
            )
            raise ExpertBudgetExceededError(
                "expert aggregate usage.model_calls exceeds admitted max_model_calls"
            )

        if not result.replayed and aggregate_model_calls != result.usage.get("model_calls"):
            aggregate_usage = dict(result.usage)
            aggregate_usage["model_calls"] = aggregate_model_calls
            result = replace(result, usage=aggregate_usage)
            invocation = self._invocations.get(result.invocation_id)
            if invocation is not None:
                invocation.usage = aggregate_usage
                invocation.result = result

        if raw_outcome.get("stale"):
            self._discard_invalid_success(
                result,
                handle,
                expert_operation,
                idempotency_key,
            )
            raise ExpertStaleGenerationError(
                "expert completion crossed a registry/runtime generation change"
            )

        return result

    def cancel(self, invocation_id: str) -> dict[str, Any]:
        """Cancel one invocation and every still-live canonical descendant."""

        _impl._bounded_pattern(
            invocation_id,
            field_name="invocation_id",
            pattern=_impl._PORTABLE,
            limit=128,
        )
        with self._lock:
            root = self._invocations.get(invocation_id)
            if root is None:
                raise ExpertInvalidInputError(
                    f"unknown invocation: {invocation_id!r}"
                )
            if root.state == "completed":
                _impl.logger.info(
                    "[ExpertRegistry] cancel after commit for %s (no reversal)",
                    invocation_id,
                )
                return {
                    "invocation_id": root.invocation_id,
                    "request_id": root.request_id,
                    "cancelled": False,
                    "committed": True,
                    "verdict": root.verdict.value if root.verdict else None,
                    "effect_receipts": [dict(item) for item in root.effect_receipts],
                }

            pending = [invocation_id]
            visited: set[str] = set()
            while pending:
                current_id = pending.pop()
                if current_id in visited:
                    continue
                visited.add(current_id)
                pending.extend(self._invocation_children.get(current_id, ()))
                current = self._invocations.get(current_id)
                if current is None or current.state == "completed":
                    continue
                current.state = "cancelled"

            _impl.logger.info(
                "[ExpertRegistry] cancelled invocation tree %s (%d records)",
                invocation_id,
                len(visited),
            )
            return {
                "invocation_id": root.invocation_id,
                "request_id": root.request_id,
                "cancelled": True,
                "committed": False,
            }

    def _discard_invalid_success(
        self,
        result: ExpertResult,
        handle: ActivationHandle,
        expert_operation: str,
        idempotency_key: Optional[str],
    ) -> None:
        """Fail closed without leaving a replayable result in the registry."""

        if result.replayed:
            return
        self._invocations.pop(result.invocation_id, None)
        if idempotency_key is None:
            return
        key = (
            handle.principal,
            result.expert_id,
            expert_operation,
            idempotency_key,
        )
        prior = self._idempotency.get(key)
        if prior is not None and prior[1] == result.invocation_id:
            self._idempotency.pop(key, None)
