"""Public ZARA-EXPERT/1 registry facade with host-owned dispatch fences.

The contract implementation lives in :mod:`zara._experts_v1`; this facade keeps
``zara.experts`` as the stable public import while tightening the pure-symbolic
host boundary. There is still exactly one registry instance/state machine: the
subclass only adapts trusted host dispatch and validates the returned usage
ledger before accepting a successful result.
"""

from __future__ import annotations

import inspect
import threading
from collections.abc import Mapping
from typing import Any, Optional

from . import _experts_v1 as _impl
from ._experts_v1 import *  # noqa: F401,F403


_ORIGINAL_HANDLER_ATTR = "__zara_original_expert_handler__"
_RESERVED_HOST_INPUTS = frozenset({"expert_operation"})


class ExpertRegistry(_impl.ExpertRegistry):
    """Canonical registry with non-spoofable operation dispatch and usage fences."""

    def __init__(self, *args: Any, **kwargs: Any) -> None:
        super().__init__(*args, **kwargs)
        # Expert handlers may synchronously delegate back into this registry.
        # Reentrancy preserves the one canonical state machine without forcing a
        # second dispatcher or out-of-band composition path.
        self._lock = threading.RLock()

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
        """Return the intersection of caller limits and descriptor ceilings."""

        if limits is None:
            requested = ExpertLimits()
        elif isinstance(limits, ExpertLimits):
            requested = limits
        else:
            raise ExpertInvalidInputError("limits must be an ExpertLimits instance")

        descriptor = self._descriptors.get(handle.expert_id)
        descriptor_limits = descriptor.resource_limits if descriptor is not None else None
        if descriptor_limits is None:
            return requested

        return ExpertLimits(
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
        before it can be accepted as current state.
        """

        admitted_registry_generation = self._registry_generation
        admitted_runtime_generation = self._runtime_generation
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

        if result.verdict is not ExpertVerdict.SUCCEEDED:
            return result

        if result.replayed:
            usage: Any = result.usage
        else:
            outcome = raw_outcome.get("value")
            usage = outcome.get("usage") if isinstance(outcome, Mapping) else None

        if not isinstance(usage, Mapping) or "model_calls" not in usage:
            self._discard_invalid_success(
                result,
                handle,
                expert_operation,
                idempotency_key,
            )
            raise ExpertInvalidInputError(
                "successful expert outcome must explicitly report usage.model_calls"
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
                "successful expert usage.model_calls must be a non-negative built-in integer"
            )

        if model_calls > admitted_limits.max_model_calls:
            self._discard_invalid_success(
                result,
                handle,
                expert_operation,
                idempotency_key,
            )
            raise ExpertBudgetExceededError(
                "successful expert usage.model_calls exceeds admitted max_model_calls"
            )

        return result

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
