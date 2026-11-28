"""Per-chat file access control for desktop surfaces.

The port keeps Qt widgets out of the permission authority: surfaces render
the effective mode and submit typed CAS mutations. When the runtime runs in
another process (daemon mode) no port exists and surfaces honestly show no
selector instead of inventing success.
"""

from __future__ import annotations

from typing import Optional, Protocol

FILE_ACCESS_MODE_ITEMS: tuple[tuple[str, str], ...] = (
    ("ask", "Ask"),
    ("sandbox", "Sandbox"),
    ("readonly", "Read-only"),
    ("full", "Full access"),
)


class FileAccessControl(Protocol):
    """Read and mutate the effective per-chat file access mode."""

    def mode_for(self, conversation_id: str) -> str: ...

    def set_mode(self, conversation_id: str, mode: str) -> str:
        """Apply one CAS mutation; return an empty string or an error message."""
        ...

    def available(self) -> bool: ...


class InProcessFileAccessControl:
    """Drive the runtime's FileAccessPolicy directly."""

    def __init__(self, policy) -> None:
        self._policy = policy

    def mode_for(self, conversation_id: str) -> str:
        return self._policy.effective_mode(conversation_id).value

    def set_mode(self, conversation_id: str, mode: str) -> str:
        revision = self._policy.revision(conversation_id)
        try:
            self._policy.set_mode(
                conversation_id, mode, expected_revision=revision, actor="desktop"
            )
        except Exception as error:  # FileAccessConflict and policy validation
            return f"File access change failed: {error}"
        return ""

    def available(self) -> bool:
        return True


def file_access_control_for_host(host) -> Optional[FileAccessControl]:
    """Resolve the port for an in-process runtime host; None when remote."""

    backend = getattr(host, "backend", None)
    policy = getattr(backend, "file_access_policy", None)
    if policy is None:
        return None
    return InProcessFileAccessControl(policy)


__all__ = [
    "FILE_ACCESS_MODE_ITEMS",
    "FileAccessControl",
    "InProcessFileAccessControl",
    "file_access_control_for_host",
]
