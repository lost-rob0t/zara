"""Consumer-only adapter over Zara's canonical ZARA-EXPERT/1 registry.

This module deliberately owns no registry, activation lifecycle, dispatcher, budget,
permission state, provider runtime, effect executor, evidence store, or history.  It
only projects the already-owned :class:`zara.experts.ExpertRegistry` through the
four operations needed by portable/native conversation consumers.
"""

from __future__ import annotations

from typing import Optional

from . import _experts_v1 as _impl
from .experts import (
    ActivationHandle,
    ExpertDeniedError,
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
        """Return the sole already-active handle for an exact identity.

        Zero matches is an ordinary fail-closed miss.  Multiple matches are an
        authority ambiguity and are rejected rather than choosing one by order.
        """

        self._require_scope(principal, "principal")
        self._require_scope(workspace, "workspace")
        self._require_scope(expert_id, "expert_id")

        registry = self._registry
        with registry._lock:
            matches = [
                record.handle
                for record in registry._activations.values()
                if record.lifecycle is LifecycleState.ACTIVE
                and record.handle.principal == principal
                and record.handle.workspace == workspace
                and record.handle.expert_id == expert_id
            ]

        if len(matches) > 1:
            raise ExpertDeniedError(
                "canonical active expert lookup is ambiguous for the requested scope"
            )
        return matches[0] if matches else None

    def invoke(self, request: ExpertRequest) -> ExpertResult:
        """Invoke only through the existing canonical request path."""

        return self._registry.invoke_request(request)

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
