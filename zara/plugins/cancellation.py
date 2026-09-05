"""Invocation-scoped cooperative cancellation for service-plugin tools."""

from __future__ import annotations

import asyncio
import contextvars
import threading
from contextlib import contextmanager
from typing import Any, Iterator, Optional

from langchain_core.runnables import RunnableConfig
from langchain_core.tools import BaseTool
from pydantic import PrivateAttr


class ToolCancellation:
    """Read-only view of canonical cancellation for one tool invocation."""

    __slots__ = ("__event",)

    def __init__(self, event: threading.Event) -> None:
        self.__event = event

    @property
    def cancelled(self) -> bool:
        return self.__event.is_set()

    def wait(self, timeout: Optional[float] = None) -> bool:
        return self.__event.wait(timeout=timeout)


_current_tool_cancellation: contextvars.ContextVar[Optional[ToolCancellation]] = (
    contextvars.ContextVar("zara_tool_cancellation", default=None)
)


def current_tool_cancellation() -> Optional[ToolCancellation]:
    """Return cancellation state for the current plugin-tool invocation."""

    return _current_tool_cancellation.get()


@contextmanager
def _bound_cancellation(cancellation: ToolCancellation) -> Iterator[None]:
    token = _current_tool_cancellation.set(cancellation)
    try:
        yield
    finally:
        _current_tool_cancellation.reset(token)


class _CancellationBoundTool(BaseTool):
    _wrapped: BaseTool = PrivateAttr()

    def __init__(self, wrapped: BaseTool) -> None:
        super().__init__(
            name=wrapped.name,
            description=wrapped.description,
            args_schema=wrapped.args_schema,
            return_direct=wrapped.return_direct,
            verbose=wrapped.verbose,
            tags=wrapped.tags,
            metadata=wrapped.metadata,
            response_format=wrapped.response_format,
        )
        self._wrapped = wrapped

    def invoke(
        self,
        input: Any,
        config: Optional[RunnableConfig] = None,
        **kwargs: Any,
    ) -> Any:
        return self._wrapped.invoke(input, config, **kwargs)

    async def ainvoke(
        self,
        input: Any,
        config: Optional[RunnableConfig] = None,
        **kwargs: Any,
    ) -> Any:
        event = threading.Event()
        cancellation = ToolCancellation(event)

        async def execute() -> Any:
            with _bound_cancellation(cancellation):
                return await self._wrapped.ainvoke(input, config, **kwargs)

        execution = asyncio.create_task(execute())
        try:
            return await asyncio.shield(execution)
        except asyncio.CancelledError:
            event.set()
            execution.cancel()
            try:
                await execution
            except asyncio.CancelledError:
                pass
            except Exception:
                pass
            raise

    def _run(self, *args: Any, **kwargs: Any) -> Any:
        raise RuntimeError("cancellation-bound tools execute through invoke/ainvoke")


def bind_tool_cancellation(tool: BaseTool) -> BaseTool:
    """Bind canonical cooperative cancellation without changing tool identity."""

    if isinstance(tool, _CancellationBoundTool):
        return tool
    return _CancellationBoundTool(tool)


__all__ = [
    "ToolCancellation",
    "bind_tool_cancellation",
    "current_tool_cancellation",
]
