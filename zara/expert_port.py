"""Consumer-only adapter over Zara's canonical ZARA-EXPERT/1 registry.

This module deliberately owns no registry, activation lifecycle, dispatcher, budget,
permission state, provider runtime, effect executor, evidence store, or history.  It
only projects the already-owned :class:`zara.experts.ExpertRegistry` through the
four operations needed by portable/native conversation consumers.
"""

from __future__ import annotations

import math
from collections.abc import Mapping
from typing import Any, Optional

from . import _experts_v1 as _impl
from .experts import (
    ActivationHandle,
    ExpertDeniedError,
    ExpertInvalidInputError,
    ExpertRegistry,
    ExpertRequest,
    ExpertResult,
    LifecycleState,
)


class CanonicalExpertInvocationPort:
    """Narrow read/invoke view over one existing canonical expert registry.

    The port never creates or mutates activation authority.  A caller may only
    discover one already-active handle for an exact principal/workspace/expert
    identity, invoke a pre-built canonical request, and sample live generations.
    """

    __slots__ = ("_registry",)

    def __init__(self, registry: ExpertRegistry) -> None:
        if not isinstance(registry, ExpertRegistry):
            raise TypeError("canonical expert port requires the public ExpertRegistry")
        self._registry = registry

    def active_activation(
        self,
        principal: str,
        workspace: str,
        expert_id: str,
    ) -> Optional[ActivationHandle]:
        """Return the sole current already-active handle for an exact identity.

        Zero matches is an ordinary fail-closed miss.  Multiple matches are an
        authority ambiguity and are rejected rather than choosing one by order.
        Handles minted under superseded registry/runtime generations are not
        projected as active consumer authority.
        """

        self._require_scope(principal, "principal")
        self._require_scope(workspace, "workspace")
        self._require_scope(expert_id, "expert_id")

        registry = self._registry
        with registry._lock:
            current_registry_generation = registry._registry_generation
            current_runtime_generation = registry._runtime_generation
            matches = [
                record.handle
                for record in registry._activations.values()
                if record.lifecycle is LifecycleState.ACTIVE
                and record.handle.principal == principal
                and record.handle.workspace == workspace
                and record.handle.expert_id == expert_id
                and record.handle.registry_generation == current_registry_generation
                and record.handle.runtime_generation == current_runtime_generation
            ]

        if len(matches) > 1:
            raise ExpertDeniedError(
                "canonical active expert lookup is ambiguous for the requested scope"
            )
        return matches[0] if matches else None

    def invoke(self, request: ExpertRequest) -> ExpertResult:
        """Invoke only through the existing canonical request path.

        Portable/native consumers share one JSON-shaped contract.  Python's JSON
        encoder otherwise permits NaN and infinities by default while Android's
        canonical envelope rejects them.  Fence those values both before dispatch
        and before projecting a result so platform behavior cannot diverge.

        A request admitted with ``max_model_calls=0`` is also not accepted as a
        pure-symbolic success unless the canonical result explicitly proves both
        provider and model usage counters are built-in integer zero.  The port
        never invents a missing provider counter on behalf of the owner.
        """

        self._require_finite_numbers(request.input, "request.input")
        result = self._registry.invoke_request(request)
        self._require_finite_numbers(result.data, "result.data")
        self._require_finite_numbers(result.usage, "result.usage")
        self._require_finite_numbers(result.effect_receipts, "result.effect_receipts")
        if request.limits is not None and request.limits.max_model_calls == 0:
            self._require_exact_zero_usage(result.usage, "provider_calls")
            self._require_exact_zero_usage(result.usage, "model_calls")
        return result

    def current_registry_generation(self) -> int:
        """Return the canonical registry's live generation."""

        return self._registry.generation

    def current_runtime_generation(self) -> int:
        """Return the canonical registry's live runtime generation."""

        return self._registry.runtime_generation

    @staticmethod
    def _require_scope(value: str, field_name: str) -> None:
        _impl._bounded_pattern(
            value,
            field_name=field_name,
            pattern=_impl._PORTABLE,
            limit=128,
        )

    @staticmethod
    def _require_exact_zero_usage(usage: Mapping[str, Any], field_name: str) -> None:
        value = usage.get(field_name)
        if type(value) is not int or value != 0:
            raise ExpertInvalidInputError(
                f"zero-model canonical result must prove usage.{field_name} == 0 "
                "as a built-in integer counter"
            )

    @classmethod
    def _require_finite_numbers(cls, value: Any, field_name: str) -> None:
        if isinstance(value, bool) or value is None or isinstance(value, (int, str)):
            return
        if isinstance(value, float):
            if not math.isfinite(value):
                raise ExpertInvalidInputError(
                    f"{field_name} contains a non-finite number"
                )
            return
        if isinstance(value, Mapping):
            for key, item in value.items():
                cls._require_finite_numbers(item, f"{field_name}.{key}")
            return
        if isinstance(value, (list, tuple)):
            for index, item in enumerate(value):
                cls._require_finite_numbers(item, f"{field_name}[{index}]")
