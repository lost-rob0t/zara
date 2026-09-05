"""Canonical invocation-scoped cooperative cancellation for Zara tools."""

from __future__ import annotations

import threading
from typing import Annotated, Any, FrozenSet, Mapping, Optional

from pydantic.json_schema import SkipJsonSchema


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


ToolCancellationArg = Annotated[Any, SkipJsonSchema()]
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
    if isinstance(schema, Mapping):
        properties = schema.get("properties")
        if isinstance(properties, Mapping):
            return frozenset(str(name) for name in properties)
        return frozenset()

    model_json_schema = getattr(schema, "model_json_schema", None)
    if callable(model_json_schema):
        rendered = model_json_schema()
        properties = rendered.get("properties") if isinstance(rendered, Mapping) else None
        if isinstance(properties, Mapping):
            return frozenset(str(name) for name in properties)
        return frozenset()

    fields = getattr(schema, "model_fields", None)
    if fields is None:
        fields = getattr(schema, "__fields__", None)
    if isinstance(fields, Mapping):
        return frozenset(str(name) for name in fields)
    return frozenset()


def _declared_field_names(schema: Any) -> FrozenSet[str]:
    fields = getattr(schema, "model_fields", None)
    if fields is None:
        fields = getattr(schema, "__fields__", None)
    if isinstance(fields, Mapping):
        return frozenset(str(name) for name in fields)
    return _schema_field_names(schema)


def inject_tool_cancellation(
    tool_call: Any,
    tool: Any,
    cancellation: ToolCancellation,
) -> Any:
    """Bind Core cancellation to an explicit model-hidden tool parameter."""
    full_fields = _declared_field_names(tool.get_input_schema())
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


__all__ = ["ToolCancellation", "ToolCancellationArg"]
