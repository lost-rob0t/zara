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


class _CancellationSignal:
    __slots__ = ("_event", "view")

    def __init__(self) -> None:
        self._event = threading.Event()
        self.view = ToolCancellation(self._event)

    def cancel(self) -> None:
        self._event.set()


def current_tool_cancellation() -> ToolCancellation:
    """Return the cancellation view for the current cancellable invocation."""

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
    if getattr(tool, "coroutine", None) is not None:
        return True
    return type(tool)._arun is not BaseTool._arun


def _invoke_sync(
    tool: BaseTool,
    tool_input: Any,
    config: Optional[RunnableConfig],
    cancellation: ToolCancellation,
    kwargs: dict[str, Any],
) -> Any:
    token = _ACTIVE_CANCELLATION.set(cancellation)
    try:
        return tool.invoke(tool_input, config=config, **kwargs)
    finally:
        _ACTIVE_CANCELLATION.reset(token)


async def _invoke_async(
    tool: BaseTool,
    tool_input: Any,
    config: Optional[RunnableConfig],
    cancellation: ToolCancellation,
    kwargs: dict[str, Any],
) -> Any:
    token = _ACTIVE_CANCELLATION.set(cancellation)
    try:
        return await tool.ainvoke(tool_input, config=config, **kwargs)
    finally:
        _ACTIVE_CANCELLATION.reset(token)


class CancellationTransportTool(BaseTool):
    """Core wrapper that owns cooperative cancellation at public invocation."""

    _inner: BaseTool = PrivateAttr()
    _model_schema: Any = PrivateAttr()

    def __init__(self, inner: BaseTool) -> None:
        if not tool_supports_cancellation(inner):
            raise ValueError("tool does not opt into cooperative cancellation")
        super().__init__(
            name=inner.name,
            description=inner.description,
            args_schema=inner.args_schema,
            return_direct=inner.return_direct,
            response_format=inner.response_format,
            metadata=inner.metadata,
            tags=inner.tags,
        )
        self._inner = inner
        self._model_schema = inner.tool_call_schema

    @property
    def tool_call_schema(self) -> Any:
        return self._model_schema

    def original_tool(self) -> BaseTool:
        return self._inner

    def invoke(
        self,
        input: Any,
        config: Optional[RunnableConfig] = None,
        **kwargs: Any,
    ) -> Any:
        signal = _CancellationSignal()
        return _invoke_sync(self._inner, input, config, signal.view, dict(kwargs))

    async def ainvoke(
        self,
        input: Any,
        config: Optional[RunnableConfig] = None,
        **kwargs: Any,
    ) -> Any:
        signal = _CancellationSignal()
        call_kwargs = dict(kwargs)
        if _has_native_async(self._inner):
            execution = asyncio.create_task(
                _invoke_async(self._inner, input, config, signal.view, call_kwargs)
            )
        else:
            execution = asyncio.create_task(
                asyncio.to_thread(
                    _invoke_sync,
                    self._inner,
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
