from __future__ import annotations

from dataclasses import dataclass
from enum import Enum
import re
from typing import Mapping


SYNC_PROTOCOL = "ZARA-SYNC/1"
SYNC_MESSAGE_TYPES = (
    "sync.hello",
    "sync.workspace",
    "sync.index",
    "sync.index.delta",
    "sync.block.request",
    "sync.block.response",
    "sync.progress",
    "sync.commit",
    "sync.conflict",
    "sync.close",
)

MAX_VECTOR_ENTRIES = 128
MAX_COUNTER = (1 << 63) - 1
MAX_IDENTIFIER_BYTES = 128
MIN_BLOCK_BYTES = 4 * 1024
MAX_BLOCK_BYTES = 4 * 1024 * 1024
MAX_INDEX_SEQUENCE = (1 << 63) - 1

_IDENTIFIER_RE = re.compile(r"^[A-Za-z0-9][A-Za-z0-9._:@+-]{0,127}$")
_SHA256_RE = re.compile(r"^[0-9a-f]{64}$")


class SyncProtocolError(ValueError):
    pass


class VectorRelation(str, Enum):
    EQUAL = "equal"
    DOMINATES = "dominates"
    DOMINATED_BY = "dominated_by"
    CONCURRENT = "concurrent"


def _bounded_identifier(value: str, *, field: str) -> str:
    if not isinstance(value, str):
        raise SyncProtocolError(f"{field} must be text")
    if not value or len(value.encode("utf-8")) > MAX_IDENTIFIER_BYTES:
        raise SyncProtocolError(f"{field} is out of bounds")
    if _IDENTIFIER_RE.fullmatch(value) is None:
        raise SyncProtocolError(f"{field} has invalid characters")
    return value


def _counter(value: int, *, field: str) -> int:
    if isinstance(value, bool) or not isinstance(value, int):
        raise SyncProtocolError(f"{field} must be an integer")
    if value < 0 or value > MAX_COUNTER:
        raise SyncProtocolError(f"{field} is out of bounds")
    return value


@dataclass(frozen=True)
class VersionVector:
    entries: tuple[tuple[str, int], ...]

    def __post_init__(self) -> None:
        if not isinstance(self.entries, tuple):
            raise SyncProtocolError("version vector entries must be a tuple")
        if len(self.entries) > MAX_VECTOR_ENTRIES:
            raise SyncProtocolError("version vector has too many entries")
        previous: str | None = None
        for entry in self.entries:
            if not isinstance(entry, tuple) or len(entry) != 2:
                raise SyncProtocolError("version vector entry is malformed")
            node_id = _bounded_identifier(entry[0], field="node_id")
            _counter(entry[1], field="version counter")
            if previous is not None and node_id <= previous:
                raise SyncProtocolError("version vector entries must be unique and sorted")
            previous = node_id

    @classmethod
    def from_mapping(cls, values: Mapping[str, int]) -> "VersionVector":
        if not isinstance(values, Mapping):
            raise SyncProtocolError("version vector must be a mapping")
        if len(values) > MAX_VECTOR_ENTRIES:
            raise SyncProtocolError("version vector has too many entries")
        normalized: list[tuple[str, int]] = []
        for node_id, counter in values.items():
            normalized.append(
                (
                    _bounded_identifier(node_id, field="node_id"),
                    _counter(counter, field="version counter"),
                )
            )
        normalized.sort(key=lambda item: item[0])
        return cls(tuple(normalized))

    def as_dict(self) -> dict[str, int]:
        return dict(self.entries)

    def increment(self, node_id: str) -> "VersionVector":
        node_id = _bounded_identifier(node_id, field="node_id")
        values = self.as_dict()
        current = values.get(node_id, 0)
        if current >= MAX_COUNTER:
            raise SyncProtocolError("version counter overflow")
        values[node_id] = current + 1
        return VersionVector.from_mapping(values)

    def compare(self, other: "VersionVector") -> VectorRelation:
        if not isinstance(other, VersionVector):
            raise SyncProtocolError("other version must be a VersionVector")
        left = self.as_dict()
        right = other.as_dict()
        left_greater = False
        right_greater = False
        for node_id in set(left) | set(right):
            left_value = left.get(node_id, 0)
            right_value = right.get(node_id, 0)
            if left_value > right_value:
                left_greater = True
            elif right_value > left_value:
                right_greater = True
            if left_greater and right_greater:
                return VectorRelation.CONCURRENT
        if left_greater:
            return VectorRelation.DOMINATES
        if right_greater:
            return VectorRelation.DOMINATED_BY
        return VectorRelation.EQUAL


@dataclass(frozen=True)
class IndexCursor:
    generation: str
    sequence: int

    def __post_init__(self) -> None:
        _bounded_identifier(self.generation, field="index generation")
        sequence = _counter(self.sequence, field="index sequence")
        if sequence > MAX_INDEX_SEQUENCE:
            raise SyncProtocolError("index sequence is out of bounds")

    def require_delta_baseline(self, generation: str, sequence: int) -> None:
        generation = _bounded_identifier(generation, field="index generation")
        sequence = _counter(sequence, field="index sequence")
        if generation != self.generation or sequence != self.sequence:
            raise SyncProtocolError("stale or unknown index delta baseline")


@dataclass(frozen=True)
class BlockRef:
    sha256: str
    offset: int
    size: int

    def __post_init__(self) -> None:
        if not isinstance(self.sha256, str) or _SHA256_RE.fullmatch(self.sha256) is None:
            raise SyncProtocolError("block sha256 must be canonical lowercase hex")
        offset = _counter(self.offset, field="block offset")
        size = _counter(self.size, field="block size")
        if size <= 0 or size > MAX_BLOCK_BYTES:
            raise SyncProtocolError("block size is out of bounds")
        if offset > MAX_COUNTER - size:
            raise SyncProtocolError("block range overflows")


@dataclass(frozen=True)
class ObjectRevision:
    workspace_id: str
    object_id: str
    revision_id: str
    version: VersionVector
    tombstone: bool
    content_size: int
    content_sha256: str | None
    blocks: tuple[BlockRef, ...]
    encrypted: bool = False

    def __post_init__(self) -> None:
        _bounded_identifier(self.workspace_id, field="workspace_id")
        _bounded_identifier(self.object_id, field="object_id")
        _bounded_identifier(self.revision_id, field="revision_id")
        if not isinstance(self.version, VersionVector):
            raise SyncProtocolError("version must be a VersionVector")
        if not isinstance(self.tombstone, bool):
            raise SyncProtocolError("tombstone must be boolean")
        if not isinstance(self.encrypted, bool):
            raise SyncProtocolError("encrypted must be boolean")
        if not isinstance(self.blocks, tuple):
            raise SyncProtocolError("blocks must be a tuple")
        content_size = _counter(self.content_size, field="content size")
        if self.tombstone:
            if content_size != 0 or self.content_sha256 is not None or self.blocks:
                raise SyncProtocolError("tombstones cannot carry content")
            return
        if not isinstance(self.content_sha256, str) or _SHA256_RE.fullmatch(self.content_sha256) is None:
            raise SyncProtocolError("content sha256 must be canonical lowercase hex")
        if content_size > 0 and not self.blocks:
            raise SyncProtocolError("non-empty content requires blocks")
        expected_offset = 0
        for block in self.blocks:
            if not isinstance(block, BlockRef):
                raise SyncProtocolError("blocks must contain BlockRef values")
            if block.offset != expected_offset:
                raise SyncProtocolError("block manifest must be contiguous and ordered")
            expected_offset += block.size
        if expected_offset != content_size:
            raise SyncProtocolError("block manifest does not cover content size")


__all__ = [
    "MAX_BLOCK_BYTES",
    "MAX_COUNTER",
    "MAX_INDEX_SEQUENCE",
    "MAX_VECTOR_ENTRIES",
    "MIN_BLOCK_BYTES",
    "SYNC_MESSAGE_TYPES",
    "SYNC_PROTOCOL",
    "BlockRef",
    "IndexCursor",
    "ObjectRevision",
    "SyncProtocolError",
    "VectorRelation",
    "VersionVector",
]
