"""Principal-scoped coordination for one-shot tool approval decisions."""

from __future__ import annotations

import asyncio
import re
from dataclasses import dataclass
from typing import Callable, Optional

from zara.runtime import bridge as runtime_bridge
from zara.runtime import events


MAX_TOOL_RUN_ID_LENGTH = 256
MAX_TOOL_NAME_LENGTH = 128
_TOOL_NAME_PATTERN = re.compile(r"[A-Za-z0-9_.:-]+\Z")


def valid_tool_name(name: object) -> bool:
    return (
        isinstance(name, str)
        and 0 < len(name) <= MAX_TOOL_NAME_LENGTH
        and _TOOL_NAME_PATTERN.fullmatch(name) is not None
    )


class ToolApprovalError(RuntimeError):
    """A tool approval decision cannot be applied to pending work."""


@dataclass(frozen=True)
class ApprovalRequest:
    tool_run_id: str
    tool_name: str
    turn_id: str
    conversation_id: Optional[str] = None


@dataclass(frozen=True)
class ApprovalResolution:
    decision: str


@dataclass
class _PendingApproval:
    request: ApprovalRequest
    future: asyncio.Future[ApprovalResolution]


class ToolApprovalController:
    """Own bounded pending approvals for one principal runtime."""

    def __init__(
        self,
        *,
        timeout_seconds: float = 300.0,
        max_pending: int = 8,
        publisher: Optional[Callable[[events.RuntimeEvent], object]] = None,
    ) -> None:
        if timeout_seconds <= 0:
            raise ValueError("tool approval timeout must be positive")
        if max_pending < 1:
            raise ValueError("maximum pending tool approvals must be at least one")
        self._timeout_seconds = float(timeout_seconds)
        self._max_pending = int(max_pending)
        self._publisher = publisher or runtime_bridge.publish
        self._pending: dict[str, _PendingApproval] = {}

    @property
    def pending_count(self) -> int:
        return len(self._pending)

    @property
    def publisher(self) -> Callable[[events.RuntimeEvent], object]:
        return self._publisher

    def bind_event_publisher(
        self,
        publisher: Callable[[events.RuntimeEvent], object],
    ) -> None:
        self._publisher = publisher

    async def wait_for_decision(self, request: ApprovalRequest) -> ApprovalResolution:
        self._validate_request(request)
        if request.tool_run_id in self._pending:
            raise ToolApprovalError("tool approval is already pending")
        if len(self._pending) >= self._max_pending:
            raise ToolApprovalError("tool approval capacity reached")

        future = asyncio.get_running_loop().create_future()
        pending = _PendingApproval(request=request, future=future)
        self._pending[request.tool_run_id] = pending
        self._publisher(
            events.ToolQueued(
                turn_id=request.turn_id,
                conversation_id=request.conversation_id,
                label=request.tool_name,
                tool_run_id=request.tool_run_id,
                tool_name=request.tool_name,
            )
        )
        self._publisher(
            events.ToolWaitingForUser(
                turn_id=request.turn_id,
                conversation_id=request.conversation_id,
                label=request.tool_name,
                tool_run_id=request.tool_run_id,
                tool_name=request.tool_name,
                prompt=f"Approve {request.tool_name}?",
            )
        )

        try:
            return await asyncio.wait_for(
                asyncio.shield(future),
                timeout=self._timeout_seconds,
            )
        except asyncio.TimeoutError:
            if self._pending.pop(request.tool_run_id, None) is pending:
                self._publish_cancelled(request, "approval timeout")
            if not future.done():
                future.cancel()
            return ApprovalResolution("reject")
        except asyncio.CancelledError:
            if self._pending.pop(request.tool_run_id, None) is pending:
                self._publish_cancelled(request, "turn cancelled")
            if not future.done():
                future.cancel()
            raise

    async def approve(self, tool_run_id: str) -> None:
        self._resolve(tool_run_id, ApprovalResolution("approve"))

    async def reject(self, tool_run_id: str, reason: str = "") -> None:
        self._resolve(tool_run_id, ApprovalResolution("reject"))

    async def cancel_turn(self, turn_id: str, reason: str = "turn cancelled") -> None:
        matches = [
            pending
            for pending in self._pending.values()
            if pending.request.turn_id == turn_id
        ]
        for pending in matches:
            if self._pending.pop(pending.request.tool_run_id, None) is not pending:
                continue
            self._publish_cancelled(pending.request, reason)
            if not pending.future.done():
                pending.future.set_result(ApprovalResolution("cancel"))

    async def shutdown(self) -> None:
        for pending in tuple(self._pending.values()):
            if self._pending.pop(pending.request.tool_run_id, None) is not pending:
                continue
            self._publish_cancelled(pending.request, "runtime shutdown")
            if not pending.future.done():
                pending.future.set_result(ApprovalResolution("cancel"))

    def _resolve(self, tool_run_id: str, resolution: ApprovalResolution) -> None:
        pending = self._pending.pop(tool_run_id, None)
        if pending is None:
            raise ToolApprovalError("tool approval is not pending")
        self._publisher(
            events.UserResponded(
                turn_id=pending.request.turn_id,
                conversation_id=pending.request.conversation_id,
                label=pending.request.tool_name,
            )
        )
        if resolution.decision == "reject":
            self._publish_cancelled(pending.request, "tool rejected")
        if not pending.future.done():
            pending.future.set_result(resolution)

    def _publish_cancelled(self, request: ApprovalRequest, reason: str) -> None:
        self._publisher(
            events.ToolCancelled(
                turn_id=request.turn_id,
                conversation_id=request.conversation_id,
                label=request.tool_name,
                tool_run_id=request.tool_run_id,
                tool_name=request.tool_name,
                reason=reason,
            )
        )

    @staticmethod
    def _validate_request(request: ApprovalRequest) -> None:
        if not request.turn_id:
            raise ToolApprovalError("tool approval has no active turn")
        if not request.tool_run_id or len(request.tool_run_id) > MAX_TOOL_RUN_ID_LENGTH:
            raise ToolApprovalError("tool approval identifier is invalid")
        if not valid_tool_name(request.tool_name):
            raise ToolApprovalError("tool approval metadata is invalid")
