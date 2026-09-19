from __future__ import annotations

import pytest

from zara.sync_protocol import (
    MAX_BLOCK_BYTES,
    MAX_COUNTER,
    BlockRef,
    IndexCursor,
    ObjectRevision,
    SyncProtocolError,
    VectorRelation,
    VersionVector,
)


A_HASH = "a" * 64
B_HASH = "b" * 64


def vector(**entries: int) -> VersionVector:
    return VersionVector.from_mapping(entries)


def revision(*, blocks: tuple[BlockRef, ...], size: int) -> ObjectRevision:
    return ObjectRevision(
        workspace_id="notes",
        object_id="org:daily-2026-09-19",
        revision_id="rev-1",
        version=vector(phone=2, desktop=1),
        tombstone=False,
        content_size=size,
        content_sha256=A_HASH,
        blocks=blocks,
    )


def test_version_vectors_distinguish_equal_dominating_and_concurrent_versions():
    base = vector(phone=2, desktop=1)

    assert base.compare(vector(desktop=1, phone=2)) is VectorRelation.EQUAL
    assert base.compare(vector(phone=1, desktop=1)) is VectorRelation.DOMINATES
    assert base.compare(vector(phone=3, desktop=1)) is VectorRelation.DOMINATED_BY
    assert base.compare(vector(phone=1, desktop=2)) is VectorRelation.CONCURRENT


def test_increment_is_immutable_and_overflow_fails_closed():
    original = vector(phone=4)
    advanced = original.increment("phone")

    assert original.as_dict() == {"phone": 4}
    assert advanced.as_dict() == {"phone": 5}

    with pytest.raises(SyncProtocolError, match="overflow"):
        vector(phone=MAX_COUNTER).increment("phone")


def test_direct_version_vector_construction_cannot_bypass_canonical_validation():
    with pytest.raises(SyncProtocolError, match="unique and sorted"):
        VersionVector((('phone', 1), ('desktop', 1)))

    with pytest.raises(SyncProtocolError, match="unique and sorted"):
        VersionVector((('phone', 1), ('phone', 2)))

    with pytest.raises(SyncProtocolError, match="invalid characters"):
        VersionVector((('phone secret', 1),))


def test_index_delta_requires_exact_generation_and_sequence_baseline():
    cursor = IndexCursor(generation="gen-7", sequence=41)

    cursor.require_delta_baseline("gen-7", 41)
    with pytest.raises(SyncProtocolError, match="baseline"):
        cursor.require_delta_baseline("gen-8", 41)
    with pytest.raises(SyncProtocolError, match="baseline"):
        cursor.require_delta_baseline("gen-7", 40)


def test_block_manifest_is_contiguous_ordered_and_bounded():
    blocks = (
        BlockRef(A_HASH, offset=0, size=4096),
        BlockRef(B_HASH, offset=4096, size=1024),
    )
    item = revision(blocks=blocks, size=5120)

    assert item.blocks == blocks

    with pytest.raises(SyncProtocolError, match="contiguous"):
        revision(
            blocks=(
                BlockRef(A_HASH, offset=0, size=4096),
                BlockRef(B_HASH, offset=5000, size=120),
            ),
            size=4216,
        )

    with pytest.raises(SyncProtocolError, match="block size"):
        BlockRef(A_HASH, offset=0, size=MAX_BLOCK_BYTES + 1)

    with pytest.raises(SyncProtocolError, match="canonical lowercase"):
        BlockRef("A" * 64, offset=0, size=4096)


def test_nonempty_revision_requires_exact_manifest_coverage():
    with pytest.raises(SyncProtocolError, match="cover content size"):
        revision(blocks=(BlockRef(A_HASH, offset=0, size=4096),), size=8192)

    with pytest.raises(SyncProtocolError, match="requires blocks"):
        revision(blocks=(), size=1)


def test_tombstone_carries_no_content_but_keeps_version_history():
    deleted = ObjectRevision(
        workspace_id="notes",
        object_id="org:daily-2026-09-19",
        revision_id="rev-delete",
        version=vector(phone=3, desktop=1),
        tombstone=True,
        content_size=0,
        content_sha256=None,
        blocks=(),
    )

    assert deleted.tombstone is True
    assert deleted.version.compare(vector(phone=2, desktop=1)) is VectorRelation.DOMINATES

    with pytest.raises(SyncProtocolError, match="tombstones cannot carry content"):
        ObjectRevision(
            workspace_id="notes",
            object_id="org:daily-2026-09-19",
            revision_id="rev-delete",
            version=vector(phone=3, desktop=1),
            tombstone=True,
            content_size=1,
            content_sha256=A_HASH,
            blocks=(BlockRef(A_HASH, offset=0, size=1),),
        )


def test_encrypted_revision_is_opaque_to_sync_core():
    encrypted = ObjectRevision(
        workspace_id="conversations",
        object_id="chat:abc",
        revision_id="ciphertext-3",
        version=vector(phone=7),
        tombstone=False,
        content_size=4096,
        content_sha256=A_HASH,
        blocks=(BlockRef(B_HASH, offset=0, size=4096),),
        encrypted=True,
    )

    assert encrypted.encrypted is True
    assert encrypted.blocks[0].sha256 == B_HASH
