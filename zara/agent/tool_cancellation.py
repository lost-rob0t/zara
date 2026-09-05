"""Invocation-local cooperative cancellation for service-plugin tools."""

from __future__ import annotations

import asyncio
import threading
from contextvars import ContextVar
from typing import Any, Optional

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


def _has_native_async(tool: BaseTool) -> bool:
    if hasattr(tool, "coroutine"):
        return getattr(tool, "coroutine") is not None
    return type(tool)._arun is not BaseTool._arun


def _invoke_sync(
    tool: BaseTool,
    tool_input: Any,
    config: Optional[RunnableConfig],
    cancellation: ToolCancellation,
    kwargs: dict[str, Any],
) -> Any:
    binding = _ACTIVE_CANCELLATION.set(cancellation)
    try:
        return tool.invoke(tool_input, config=config, **kwargs)
    finally:
        _ACTIVE_CANCELLATION.reset(binding)


async def _invoke_async(
    tool: BaseTool,
    tool_input: Any,
    config: Optional[RunnableConfig],
    cancellation: ToolCancellation,
    kwargs: dict[str, Any],
) -> Any:
    binding = _ACTIVE_CANCELLATION.set(cancellation)
    try:
        return await tool.ainvoke(tool_input, config=config, **kwargs)
    finally:
        _ACTIVE_CANCELLATION.reset(binding)


class CancellationTransportTool(BaseTool):
    """Core wrapper that owns cancellation at the public invocation boundary."""

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

    def invoke(
        self,
        input: Any,
        config: Optional[RunnableConfig] = None,
        **kwargs: Any,
    ) -> Any:
        signal = _ToolCancellationSignal()
        return _invoke_sync(self._tool, input, config, signal.view, dict(kwargs))

    async def ainvoke(
        self,
        input: Any,
        config: Optional[RunnableConfig] = None,
        **kwargs: Any,
    ) -> Any:
        signal = _ToolCancellationSignal()
        call_kwargs = dict(kwargs)
        if _has_native_async(self._tool):
            execution = asyncio.create_task(
                _invoke_async(self._tool, input, config, signal.view, call_kwargs)
            )
        else:
            execution = asyncio.create_task(
                asyncio.to_thread(
                    _invoke_sync,
                    self._tool,
                    input,
                    config,
                    signal.view,
                    call_kwargs,
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

    def _run(self, *args: Any, **kwargs: Any) -> Any:
        raise RuntimeError("cancellation wrapper must execute through invoke()")


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
