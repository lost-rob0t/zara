"""Public ZARA-EXPERT/1 registry facade with host-owned dispatch fences.

The contract implementation lives in :mod:`zara._experts_v1`; this facade keeps
``zara.experts`` as the stable public import while tightening the pure-symbolic
host boundary.  There is still exactly one registry instance/state machine: the
subclass only adapts trusted host dispatch and validates the returned usage
ledger before accepting a successful result.
"""

from __future__ import annotations

import inspect
from collections.abc import Mapping
from typing import Any, Optional

from . import _experts_v1 as _impl
from ._experts_v1 import *  # noqa: F401,F403


class ExpertRegistry(_impl.ExpertRegistry):
    """Canonical registry with non-spoofable operation dispatch and usage fences."""

    @staticmethod
    def _handler_declares_host_operation(handler: Any) -> bool:
        try:
            parameter = inspect.signature(handler).parameters.get("expert_operation")
        except (TypeError, ValueError):
            return False
        return parameter is not None and parameter.kind is not inspect.Parameter.POSITIONAL_ONLY

    def _validate_handler(
        self,
        descriptor: ExpertDescriptor,
        handler: Optional[Any],
    ) -> None:
        if handler is None:
            return
        if not callable(handler):
            raise ExpertInvalidInputError("expert handler must be callable")
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

    def _invoke_unlocked(
        self,
        handle: ActivationHandle,
        expert_operation: str,
        input: dict[str, Any],
        limits: Optional[ExpertLimits],
        idempotency_key: Optional[str],
        request_id: Optional[str],
    ) -> ExpertResult:
        """Invoke through the existing state machine, adding only trusted metadata.

        ``expert_operation`` never enters the user input mapping.  If a trusted
        adapter explicitly declares the reserved keyword-only parameter, the
        host injects the selected operation immediately before dispatch.  Legacy
        payload-only handlers keep their existing call shape.
        """

        descriptor = self._descriptors.get(handle.expert_id)
        handler = self._handlers.get(handle.expert_id)
        raw_outcome: dict[str, Any] = {}
        injected = False

        if handler is not None:
            injected = self._handler_declares_host_operation(handler)

            def dispatch(**payload: Any) -> Any:
                if injected:
                    outcome = handler(
                        expert_operation=expert_operation,
                        **payload,
                    )
                else:
                    outcome = handler(**payload)
                raw_outcome["value"] = outcome
                return outcome

            self._handlers[handle.expert_id] = dispatch

        try:
            result = super()._invoke_unlocked(
                handle,
                expert_operation,
                input,
                limits,
                idempotency_key,
                request_id,
            )
        finally:
            if handler is not None:
                self._handlers[handle.expert_id] = handler

        if result.verdict is not ExpertVerdict.SUCCEEDED:
            return result

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

        admitted_limits = limits if isinstance(limits, ExpertLimits) else ExpertLimits()
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
        """Fail closed without leaving a replayable success in the registry."""

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
