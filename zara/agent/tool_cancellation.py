"""Invocation-local cooperative cancellation for service-plugin tools."""

from __future__ import annotations

import threading
from contextvars import ContextVar
from typing import Any, Mapping, Optional

from langchain_core.runnables import RunnableConfig
from langchain_core.tools import BaseTool
from pydantic import PrivateAttr

_CANCELLATION_METADATA_KEY = "zara_supports_cancellation"
_CONFIG_KEY = "__zara_tool_cancellation"
_ACTIVE_CANCELLATION: ContextVar[Optional["ToolCancellation"]] = ContextVar(
    "zara_tool_cancellation",
    default=None,
)


class ToolCancellation:
    """Read-only cancellation state for one exact tool invocation."""

    __slots__ = ("__event",)

    def __init__(self, event: threading.Event) -> None:
        self.__event = event

    @property
    def cancelled(self) -> bool:
        return self.__event.is_set()

    def wait(self, timeout: Optional[float] = None) -> bool:
        return self.__event.wait(timeout=timeout)


class _ToolCancellationSignal:
    __slots__ = ("_event", "view")

    def __init__(self) -> None:
        self._event = threading.Event()
        self.view = ToolCancellation(self._event)

    def cancel(self) -> None:
        self._event.set()


def current_tool_cancellation() -> ToolCancellation:
    """Return the cancellation view bound to the running plugin invocation."""
    cancellation = _ACTIVE_CANCELLATION.get()
    if cancellation is None:
        raise RuntimeError("no active cancellable tool invocation")
    return cancellation


def new_tool_cancellation_signal() -> _ToolCancellationSignal:
    return _ToolCancellationSignal()


def tool_supports_cancellation(tool: BaseTool) -> bool:
    metadata = getattr(tool, "metadata", None) or {}
    marker = metadata.get(_CANCELLATION_METADATA_KEY, False)
    if not isinstance(marker, bool):
        raise ValueError("zara_supports_cancellation tool metadata must be true or false")
    return marker


def with_tool_cancellation(
    config: Optional[RunnableConfig],
    cancellation: ToolCancellation,
) -> RunnableConfig:
    bound = dict(config or {})
    configurable = dict(bound.get("configurable") or {})
    configurable[_CONFIG_KEY] = cancellation
    bound["configurable"] = configurable
    return bound


def _cancellation_from_config(config: RunnableConfig) -> ToolCancellation:
    configurable = config.get("configurable") or {}
    cancellation = configurable.get(_CONFIG_KEY)
    if not isinstance(cancellation, ToolCancellation):
        raise RuntimeError("cancellable tool invocation is missing Core cancellation state")
    return cancellation


def _tool_input(args: tuple[Any, ...], kwargs: Mapping[str, Any]) -> Any:
    if kwargs:
        if args:
            raise TypeError("tool invocation cannot mix positional and keyword input")
        return dict(kwargs)
    if len(args) == 1:
        return args[0]
    if not args:
        return {}
    raise TypeError("tool invocation has unsupported positional input")


class CancellationTransportTool(BaseTool):
    """Core wrapper that binds cancellation inside the actual execution worker."""

    _tool: BaseTool = PrivateAttr()
    _model_schema: Any = PrivateAttr()

    def __init__(self, tool: BaseTool) -> None:
        if not tool_supports_cancellation(tool):
            raise ValueError("tool does not opt into cooperative cancellation")
        super().__init__(
            name=tool.name,
            description=tool.description,
            args_schema=tool.args_schema,
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

    def _run(
        self,
        *args: Any,
        config: RunnableConfig,
        **kwargs: Any,
    ) -> Any:
        cancellation = _cancellation_from_config(config)
        binding = _ACTIVE_CANCELLATION.set(cancellation)
        try:
            return self._tool.invoke(_tool_input(args, kwargs), config=config)
        finally:
            _ACTIVE_CANCELLATION.reset(binding)

    async def _arun(
        self,
        *args: Any,
        config: RunnableConfig,
        **kwargs: Any,
    ) -> Any:
        cancellation = _cancellation_from_config(config)
        binding = _ACTIVE_CANCELLATION.set(cancellation)
        try:
            return await self._tool.ainvoke(_tool_input(args, kwargs), config=config)
        finally:
            _ACTIVE_CANCELLATION.reset(binding)


def bind_tool_cancellation_transport(tool: BaseTool) -> BaseTool:
    if not tool_supports_cancellation(tool):
        return tool
    return CancellationTransportTool(tool)


__all__ = [
    "ToolCancellation",
    "bind_tool_cancellation_transport",
    "current_tool_cancellation",
    "new_tool_cancellation_signal",
    "tool_supports_cancellation",
    "with_tool_cancellation",
]
