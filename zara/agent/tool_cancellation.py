"""Canonical invocation-scoped cooperative cancellation for Zara tools."""

from __future__ import annotations

import threading
from typing import Annotated, Any, FrozenSet, Mapping, Optional

from langchain_core.tools import InjectedToolArg


class ToolCancellation:
    """Read-only cancellation view for one exact tool invocation."""

    __slots__ = ("__event",)

    def __init__(self, event: threading.Event) -> None:
        self.__event = event

    @property
    def cancelled(self) -> bool:
        return self.__event.is_set()

    def wait(self, timeout: Optional[float] = None) -> bool:
        return self.__event.wait(timeout=timeout)


ToolCancellationArg = Annotated[Any, InjectedToolArg]
"""Model-hidden cancellation dependency populated only by Zara Core."""


class _ToolCancellationSignal:
    __slots__ = ("_event", "view")

    def __init__(self) -> None:
        self._event = threading.Event()
        self.view = ToolCancellation(self._event)

    def cancel(self) -> None:
        self._event.set()


def new_tool_cancellation_signal() -> _ToolCancellationSignal:
    """Create Core-owned state for one approved tool invocation."""
    return _ToolCancellationSignal()


def _schema_field_names(schema: Any) -> FrozenSet[str]:
    """Return declared field names for Pydantic or JSON-schema tool inputs."""
    if isinstance(schema, Mapping):
        properties = schema.get("properties")
        if isinstance(properties, Mapping):
            return frozenset(str(name) for name in properties)
        return frozenset()

    fields = getattr(schema, "model_fields", None)
    if fields is None:
        fields = getattr(schema, "__fields__", None)
    if isinstance(fields, Mapping):
        return frozenset(str(name) for name in fields)
    return frozenset()


def inject_tool_cancellation(
    tool_call: Any,
    tool: Any,
    cancellation: ToolCancellation,
) -> Any:
    """Inject Core cancellation only for a tool's hidden cancellation input.

    The full LangChain input schema retains injected arguments while
    ``tool_call_schema`` is the model-facing schema. Requiring the field to
    exist only in the former makes opt-in explicit and prevents Core from
    inventing arguments for ordinary tools.
    """
    full_fields = _schema_field_names(tool.get_input_schema())
    model_fields = _schema_field_names(tool.tool_call_schema)
    if "cancellation" not in full_fields or "cancellation" in model_fields:
        return tool_call

    if not isinstance(tool_call, Mapping):
        raise TypeError("cancellable tool call must be a mapping")
    args = tool_call.get("args")
    if not isinstance(args, Mapping):
        raise TypeError("cancellable tool call args must be a mapping")

    injected = dict(tool_call)
    injected_args = dict(args)
    injected_args["cancellation"] = cancellation
    injected["args"] = injected_args
    return injected


class _CancellationBoundTool:
    """One-invocation delegate that restores Core cancellation at invocation."""

    __slots__ = ("_tool", "_cancellation", "name")

    def __init__(self, tool: Any, cancellation: ToolCancellation) -> None:
        self._tool = tool
        self._cancellation = cancellation
        self.name = tool.name

    def __getattr__(self, name: str) -> Any:
        """Delegate tool metadata required by ToolNode's dynamic-tool path."""
        return getattr(self._tool, name)

    async def ainvoke(self, tool_call: Any, config: Any = None, **kwargs: Any) -> Any:
        if not isinstance(tool_call, Mapping):
            raise TypeError("cancellable tool invocation must be a mapping")
        args = tool_call.get("args")
        if not isinstance(args, Mapping):
            raise TypeError("cancellable tool invocation args must be a mapping")

        bound_call = dict(tool_call)
        bound_args = dict(args)
        bound_args["cancellation"] = self._cancellation
        bound_call["args"] = bound_args
        return await self._tool.ainvoke(bound_call, config, **kwargs)


async def execute_with_tool_cancellation(request: Any, execute: Any) -> Any:
    """Reattach trusted Core cancellation after ToolNode strips injected args.

    The gated graph node overwrites any model value with a Core-owned
    ``ToolCancellation`` before entering ToolNode. ToolNode's wrapper sees that
    trusted value, binds it to a one-invocation delegate, and then calls the
    canonical ToolNode executor exactly once. ToolNode still owns validation,
    injected state/runtime handling, error policy, and response normalization.
    """
    tool = request.tool
    if tool is None:
        return await execute(request)

    tool_call = request.tool_call
    if not isinstance(tool_call, Mapping):
        return await execute(request)
    args = tool_call.get("args")
    if not isinstance(args, Mapping):
        return await execute(request)
    cancellation = args.get("cancellation")
    if not isinstance(cancellation, ToolCancellation):
        return await execute(request)

    bound_tool = _CancellationBoundTool(tool, cancellation)
    return await execute(request.override(tool=bound_tool))


__all__ = ["ToolCancellation", "ToolCancellationArg"]
