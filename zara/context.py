from __future__ import annotations

import mimetypes
import threading
import uuid
from dataclasses import dataclass, field
from enum import Enum
from pathlib import Path
from types import MappingProxyType
from typing import Mapping, Optional, Sequence


class ContextAttachmentError(RuntimeError):
    pass


class ContextAttachmentNotFound(ContextAttachmentError):
    pass


class ContextAttachmentKind(str, Enum):
    FILE = "file"
    AUDIO = "audio"
    VIDEO = "video"
    IMAGE = "image"
    SCREENSHOT = "screenshot"
    PROJECT = "project"
    SYSTEM = "system"


class ContextAttachmentScope(str, Enum):
    TURN = "turn"
    CONVERSATION = "conversation"
    PERSISTENT = "persistent"


class ContextSensitivity(str, Enum):
    PUBLIC = "public"
    PRIVATE = "private"
    SECRET = "secret"


_TEXT_MIME_PREFIXES = ("text/",)
_TEXT_MIME_TYPES = frozenset(
    {
        "application/json",
        "application/ld+json",
        "application/xml",
        "application/yaml",
        "application/x-yaml",
        "application/toml",
        "application/x-toml",
    }
)
_MAX_DISPLAY_NAME = 256
_MAX_SOURCE = 128
_MAX_TEXT_CHARS = 262_144


@dataclass(frozen=True)
class ContextAttachment:
    id: str
    kind: ContextAttachmentKind
    display_name: str
    scope: ContextAttachmentScope
    source: str
    sensitivity: ContextSensitivity = ContextSensitivity.PRIVATE
    media_type: str = "application/octet-stream"
    locator: Optional[str] = None
    text: Optional[str] = None
    metadata: Mapping[str, str] = field(default_factory=dict)

    def __post_init__(self) -> None:
        if not self.id.startswith("ctx-") or len(self.id) > 80:
            raise ValueError("context attachment id must be a bounded ctx-* identifier")
        if not self.display_name or len(self.display_name) > _MAX_DISPLAY_NAME:
            raise ValueError("context display name must contain 1 to 256 characters")
        if not self.source or len(self.source) > _MAX_SOURCE:
            raise ValueError("context source must contain 1 to 128 characters")
        if self.text is not None and len(self.text) > _MAX_TEXT_CHARS:
            raise ValueError("context inline text exceeds the supported character limit")
        object.__setattr__(self, "metadata", MappingProxyType(dict(self.metadata)))


class ContextAttachmentStore:
    def __init__(
        self,
        *,
        max_inline_bytes: int = 256 * 1024,
        max_attachment_bytes: int = 512 * 1024 * 1024,
        max_render_chars: int = 12_000,
    ) -> None:
        if max_inline_bytes < 0:
            raise ValueError("max_inline_bytes must not be negative")
        if max_attachment_bytes <= 0:
            raise ValueError("max_attachment_bytes must be positive")
        if max_render_chars <= 0:
            raise ValueError("max_render_chars must be positive")
        self.max_inline_bytes = int(max_inline_bytes)
        self.max_attachment_bytes = int(max_attachment_bytes)
        self.max_render_chars = int(max_render_chars)
        self._attachments: dict[str, ContextAttachment] = {}
        self._lock = threading.RLock()

    def add(self, attachment: ContextAttachment) -> ContextAttachment:
        if not isinstance(attachment, ContextAttachment):
            raise TypeError("attachment must be a ContextAttachment")
        with self._lock:
            if attachment.id in self._attachments:
                raise ContextAttachmentError("context attachment id is already registered")
            self._attachments[attachment.id] = attachment
        return attachment

    def add_path(
        self,
        path: Path | str,
        *,
        kind: ContextAttachmentKind = ContextAttachmentKind.FILE,
        scope: ContextAttachmentScope = ContextAttachmentScope.TURN,
        source: str = "user:add-context",
        sensitivity: ContextSensitivity = ContextSensitivity.PRIVATE,
    ) -> ContextAttachment:
        resolved = Path(path).expanduser().resolve(strict=True)
        if kind is ContextAttachmentKind.PROJECT:
            if not resolved.is_dir():
                raise ContextAttachmentError("project context must reference a directory")
            size = None
        else:
            if not resolved.is_file():
                raise ContextAttachmentError("context path must reference a regular file")
            size = resolved.stat().st_size
            if size > self.max_attachment_bytes:
                raise ContextAttachmentError("context attachment exceeds the configured size limit")

        media_type = self._media_type(resolved, kind)
        text = None
        if kind is ContextAttachmentKind.FILE and size is not None and size <= self.max_inline_bytes:
            if self._is_text_media_type(media_type):
                try:
                    text = resolved.read_text(encoding="utf-8")
                except UnicodeDecodeError:
                    text = None

        return self.add(
            ContextAttachment(
                id=self._new_id(),
                kind=kind,
                display_name=resolved.name,
                scope=scope,
                source=source,
                sensitivity=sensitivity,
                media_type=media_type,
                locator=str(resolved),
                text=text,
                metadata={"size_bytes": "" if size is None else str(size)},
            )
        )

    def add_text(
        self,
        text: str,
        *,
        kind: ContextAttachmentKind = ContextAttachmentKind.SYSTEM,
        display_name: str,
        scope: ContextAttachmentScope = ContextAttachmentScope.TURN,
        source: str,
        sensitivity: ContextSensitivity = ContextSensitivity.PRIVATE,
        media_type: str = "text/plain",
        metadata: Optional[Mapping[str, str]] = None,
    ) -> ContextAttachment:
        encoded = text.encode("utf-8")
        if len(encoded) > self.max_inline_bytes:
            raise ContextAttachmentError("context inline text exceeds the configured size limit")
        return self.add(
            ContextAttachment(
                id=self._new_id(),
                kind=kind,
                display_name=display_name,
                scope=scope,
                source=source,
                sensitivity=sensitivity,
                media_type=media_type,
                text=text,
                metadata=metadata or {},
            )
        )

    def get(self, attachment_id: str) -> ContextAttachment:
        with self._lock:
            attachment = self._attachments.get(attachment_id)
        if attachment is None:
            raise ContextAttachmentNotFound(
                f"unknown context attachment: {attachment_id}"
            )
        return attachment

    def resolve(self, attachment_ids: Sequence[str]) -> tuple[ContextAttachment, ...]:
        seen: set[str] = set()
        resolved = []
        for attachment_id in attachment_ids:
            if attachment_id in seen:
                continue
            seen.add(attachment_id)
            resolved.append(self.get(attachment_id))
        return tuple(resolved)

    def list(self) -> tuple[ContextAttachment, ...]:
        with self._lock:
            return tuple(self._attachments.values())

    def remove(self, attachment_id: str) -> bool:
        with self._lock:
            return self._attachments.pop(attachment_id, None) is not None

    def expire_turn(self, attachment_ids: Sequence[str]) -> int:
        removed = 0
        for attachment in self.resolve(attachment_ids):
            if attachment.scope is ContextAttachmentScope.TURN:
                removed += int(self.remove(attachment.id))
        return removed

    def render(
        self,
        attachment_ids: Sequence[str],
        *,
        max_chars: Optional[int] = None,
    ) -> str:
        attachments = self.resolve(attachment_ids)
        if not attachments:
            return ""
        blocks = []
        for attachment in attachments:
            header = (
                f"[{attachment.kind.value}] {attachment.display_name} "
                f"(source={attachment.source}, scope={attachment.scope.value})"
            )
            if attachment.sensitivity is ContextSensitivity.SECRET:
                body = "<secret context withheld from model>"
            elif attachment.text is not None:
                body = attachment.text.strip()
            elif attachment.kind is ContextAttachmentKind.PROJECT:
                body = f"project reference: {attachment.locator}"
            else:
                body = f"binary/reference context; media_type={attachment.media_type}"
            blocks.append(header if not body else f"{header}\n{body}")
        rendered = "Attached context:\n" + "\n\n".join(blocks)
        limit = self.max_render_chars if max_chars is None else max(1, int(max_chars))
        return rendered[:limit].rstrip()

    @staticmethod
    def _new_id() -> str:
        return f"ctx-{uuid.uuid4().hex}"

    @staticmethod
    def _is_text_media_type(media_type: str) -> bool:
        return media_type.startswith(_TEXT_MIME_PREFIXES) or media_type in _TEXT_MIME_TYPES

    @staticmethod
    def _media_type(path: Path, kind: ContextAttachmentKind) -> str:
        if kind is ContextAttachmentKind.PROJECT:
            return "application/x-zara-project"
        guessed, _ = mimetypes.guess_type(path.name)
        if guessed:
            return guessed
        defaults = {
            ContextAttachmentKind.AUDIO: "audio/*",
            ContextAttachmentKind.VIDEO: "video/*",
            ContextAttachmentKind.IMAGE: "image/*",
            ContextAttachmentKind.SCREENSHOT: "image/png",
        }
        return defaults.get(kind, "application/octet-stream")


_DEFAULT_CONTEXT_STORE = ContextAttachmentStore()


def get_context_attachment_store() -> ContextAttachmentStore:
    return _DEFAULT_CONTEXT_STORE


__all__ = [
    "ContextAttachment",
    "ContextAttachmentError",
    "ContextAttachmentKind",
    "ContextAttachmentNotFound",
    "ContextAttachmentScope",
    "ContextAttachmentStore",
    "ContextSensitivity",
    "get_context_attachment_store",
]
