"""Canonical invocation-scoped cooperative cancellation for Zara tools."""

from __future__ import annotations

import threading
from typing import Annotated, Any, FrozenSet, Mapping, Optional

from langchain_core.tools import BaseTool, InjectedToolArg
from pydantic import PrivateAttr


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


def _plain_input_schema(tool: BaseTool) -> dict[str, Any]:
    schema = tool.get_input_schema()
    if isinstance(schema, Mapping):
        return dict(schema)
    return schema.model_json_schema()


def supports_tool_cancellation(tool: BaseTool) -> bool:
    """Return whether ``tool`` explicitly opts into Zara cancellation."""
    full_fields = _schema_field_names(tool.get_input_schema())
    model_fields = _schema_field_names(tool.tool_call_schema)
    return "cancellation" in full_fields and "cancellation" not in model_fields


class CancellationTransportTool(BaseTool):
    """Stable ToolNode binding that transports Core cancellation to a plugin.

    LangGraph strips caller values for arguments it classifies as injected. Zara
    therefore registers this stable transport once, with a plain full execution
    schema and the original model-facing schema. The model never sees the
    cancellation field, while ToolNode no longer rewrites the trusted value that
    Core adds after approval. The original plugin tool remains the only function
    implementation and receives that value on the final forward.
    """

    _tool: BaseTool = PrivateAttr()
    _model_schema: Any = PrivateAttr()

    def __init__(self, tool: BaseTool) -> None:
        if not supports_tool_cancellation(tool):
            raise ValueError("tool does not declare a hidden cancellation argument")
        super().__init__(
            name=tool.name,
            description=tool.description,
            args_schema=_plain_input_schema(tool),
            return_direct=tool.return_direct,
            response_format=tool.response_format,
            metadata=tool.metadata,
            tags=tool.tags,
        )
        self._tool = tool
        self._model_schema = tool.tool_call_schema

    @property
    def tool_call_schema(self) -> Any:
        return self._model_schema

    def _run(self, **kwargs: Any) -> Any:
        return self._tool.invoke(kwargs)

    async def _arun(self, **kwargs: Any) -> Any:
        return await self._tool.ainvoke(kwargs)


def bind_tool_cancellation_transport(tool: BaseTool) -> BaseTool:
    """Return the one stable ToolNode binding for a registered tool."""
    if isinstance(tool, CancellationTransportTool):
        return tool
    if not supports_tool_cancellation(tool):
        return tool
    return CancellationTransportTool(tool)


def inject_tool_cancellation(
    tool_call: Any,
    tool: Any,
    cancellation: ToolCancellation,
) -> Any:
    """Overwrite any spoofed value with Core's exact invocation view."""
    if not supports_tool_cancellation(tool):
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


async def execute_with_tool_cancellation(request: Any, execute: Any) -> Any:
    """Keep ToolNode as the sole executor; transport is registration-time stable."""
    return await execute(request)


__all__ = ["ToolCancellation", "ToolCancellationArg"]
