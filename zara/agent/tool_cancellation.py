"""Invocation-local cooperative cancellation for service-plugin tools."""

from __future__ import annotations

import asyncio
import threading
from contextvars import ContextVar
from typing import Any, Mapping, Optional

from langchain_core.runnables import RunnableConfig
from langchain_core.tools import BaseTool
from pydantic import PrivateAttr

_CANCELLATION_METADATA_KEY = "zara_supports_cancellation"
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


def tool_supports_cancellation(tool: Any) -> bool:
    metadata = getattr(tool, "metadata", None) or {}
    marker = metadata.get(_CANCELLATION_METADATA_KEY, False)
    if not isinstance(marker, bool):
        raise ValueError("zara_supports_cancellation tool metadata must be true or false")
    return marker


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


def _has_native_async(tool: BaseTool) -> bool:
    if hasattr(tool, "coroutine"):
        return getattr(tool, "coroutine") is not None
    return type(tool)._arun is not BaseTool._arun


def _invoke_sync(
    tool: BaseTool,
    tool_input: Any,
    config: RunnableConfig,
    cancellation: ToolCancellation,
) -> Any:
    binding = _ACTIVE_CANCELLATION.set(cancellation)
    try:
        return tool.invoke(tool_input, config=config)
    finally:
        _ACTIVE_CANCELLATION.reset(binding)


async def _invoke_async(
    tool: BaseTool,
    tool_input: Any,
    config: RunnableConfig,
    cancellation: ToolCancellation,
) -> Any:
    binding = _ACTIVE_CANCELLATION.set(cancellation)
    try:
        return await tool.ainvoke(tool_input, config=config)
    finally:
        _ACTIVE_CANCELLATION.reset(binding)


class CancellationTransportTool(BaseTool):
    """Core wrapper that owns cancellation inside the actual invocation."""

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

    def original_tool(self) -> BaseTool:
        return self._tool

    def _run(
        self,
        *args: Any,
        config: RunnableConfig,
        **kwargs: Any,
    ) -> Any:
        signal = _ToolCancellationSignal()
        return _invoke_sync(
            self._tool,
            _tool_input(args, kwargs),
            config,
            signal.view,
        )

    async def _arun(
        self,
        *args: Any,
        config: RunnableConfig,
        **kwargs: Any,
    ) -> Any:
        signal = _ToolCancellationSignal()
        tool_input = _tool_input(args, kwargs)
        if _has_native_async(self._tool):
            execution = asyncio.create_task(
                _invoke_async(self._tool, tool_input, config, signal.view)
            )
        else:
            execution = asyncio.create_task(
                asyncio.to_thread(
                    _invoke_sync,
                    self._tool,
                    tool_input,
                    config,
                    signal.view,
                )
            )

        try:
            return await asyncio.shield(execution)
        except asyncio.CancelledError:
            signal.cancel()
            execution.cancel()
            try:
                await execution
            except asyncio.CancelledError:
                pass
            raise


def bind_tool_cancellation_transport(tool: Any) -> Any:
    if not tool_supports_cancellation(tool):
        return tool
    if not isinstance(tool, BaseTool):
        raise ValueError("cancellable tools must be LangChain BaseTool instances")
    if isinstance(tool, CancellationTransportTool):
        return tool
    return CancellationTransportTool(tool)


def original_tool(tool: Any) -> Any:
    if isinstance(tool, CancellationTransportTool):
        return tool.original_tool()
    return tool


__all__ = [
    "ToolCancellation",
    "bind_tool_cancellation_transport",
    "current_tool_cancellation",
    "original_tool",
    "tool_supports_cancellation",
]
