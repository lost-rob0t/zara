"""Normalize Navidrome rows into conservative Tek9 music graph batches."""

from __future__ import annotations

import hashlib
from dataclasses import dataclass
from typing import Any, Iterable, Mapping


@dataclass(frozen=True)
class Tek9CatalogBatch:
    documents: tuple[dict[str, Any], ...]
    nodes: tuple[dict[str, Any], ...]
    edges: tuple[dict[str, Any], ...]


def normalize_navidrome_rows(
    rows: Iterable[Mapping[str, Any]],
) -> Tek9CatalogBatch:
    documents: list[dict[str, Any]] = []
    node_by_id: dict[str, dict[str, Any]] = {}
    edge_by_id: dict[str, dict[str, Any]] = {}

    for row in rows:
        item = dict(row)
        media_id = _required_text(item, "id")
        library_id = _required_text(item, "library_id")
        path = _required_text(item, "path")
        title = _required_text(item, "title")
        album_name = _required_text(item, "album")
        artist_name = _required_text(item, "artist")

        file_id = _source_id("file", "navidrome", media_id)
        library_node_id = _source_id("library", "navidrome", library_id)

        recording_mbid = _optional_text(item.get("mbz_recording_id"))
        recording_id = (
            _source_id("recording", "musicbrainz", recording_mbid)
            if recording_mbid
            else _source_id("recording", "navidrome-file", media_id)
        )

        artist_source_id = _optional_text(item.get("artist_id"))
        artist_id = (
            _source_id("artist", "navidrome", artist_source_id)
            if artist_source_id
            else _semantic_id("artist", artist_name.casefold())
        )

        album_source_id = _optional_text(item.get("album_id"))
        album_id = (
            _source_id("album", "navidrome", album_source_id)
            if album_source_id
            else _semantic_id(
                "album",
                artist_name.casefold(),
                album_name.casefold(),
            )
        )

        value = {
            "dtype": "music_file",
            "source": "navidrome",
            "source_id": media_id,
            "library_id": library_id,
            "path": path,
            "title": title,
            "album": album_name,
            "artist": artist_name,
            "duration_seconds": _optional_number(item.get("duration")),
            "size_bytes": _optional_int(item.get("size")),
            "updated_at": _optional_text(item.get("updated_at")),
            "created_at": _optional_text(item.get("created_at")),
            "format": _optional_text(item.get("suffix")),
            "year": _optional_int(item.get("year")),
            "bit_rate": _optional_int(item.get("bit_rate")),
            "bit_depth": _optional_int(item.get("bit_depth")),
            "sample_rate": _optional_int(item.get("sample_rate")),
            "channels": _optional_int(item.get("channels")),
            "musicbrainz_recording_id": recording_mbid,
            "musicbrainz_album_id": _optional_text(item.get("mbz_album_id")),
            "musicbrainz_artist_id": _optional_text(item.get("mbz_artist_id")),
            "musicbrainz_album_artist_id": _optional_text(
                item.get("mbz_album_artist_id")
            ),
            "file_node_id": file_id,
            "recording_node_id": recording_id,
            "artist_node_id": artist_id,
            "album_node_id": album_id,
            "library_node_id": library_node_id,
        }
        documents.append({"id": file_id, "value": value})

        _upsert_node(
            node_by_id,
            file_id,
            {
                "dtype": "music_file",
                "source": "navidrome",
                "source_id": media_id,
                "path": path,
                "title": title,
                "format": _optional_text(item.get("suffix")),
            },
        )
        _upsert_node(
            node_by_id,
            recording_id,
            {
                "dtype": "recording",
                "title": title,
                "musicbrainz_recording_id": recording_mbid,
                "identity_strength": "hard"
                if recording_mbid
                else "source-local",
            },
        )
        _upsert_node(
            node_by_id,
            artist_id,
            {
                "dtype": "artist",
                "name": artist_name,
                "source": "navidrome" if artist_source_id else "derived-name",
                "source_id": artist_source_id,
            },
        )
        _upsert_node(
            node_by_id,
            album_id,
            {
                "dtype": "album",
                "title": album_name,
                "year": _optional_int(item.get("year")),
                "source": "navidrome" if album_source_id else "derived-name",
                "source_id": album_source_id,
            },
        )
        _upsert_node(
            node_by_id,
            library_node_id,
            {
                "dtype": "library",
                "source": "navidrome",
                "source_id": library_id,
            },
        )

        for source, predicate, target in (
            (file_id, "represents-recording", recording_id),
            (file_id, "in-library", library_node_id),
            (recording_id, "performed-by", artist_id),
            (recording_id, "part-of-album", album_id),
        ):
            edge = _edge(source, predicate, target)
            edge_by_id[edge["id"]] = edge

    return Tek9CatalogBatch(
        documents=tuple(documents),
        nodes=tuple(node_by_id.values()),
        edges=tuple(edge_by_id.values()),
    )


def _upsert_node(
    target: dict[str, dict[str, Any]],
    node_id: str,
    props: Mapping[str, Any],
) -> None:
    existing = target.get(node_id)
    if existing is None:
        target[node_id] = {"id": node_id, "props": dict(props)}
        return

    merged = dict(existing["props"])
    for key, value in props.items():
        if value is not None and merged.get(key) is None:
            merged[key] = value
    existing["props"] = merged


def _edge(source: str, predicate: str, target: str) -> dict[str, str]:
    identity = _digest(source, predicate, target)
    return {
        "id": f"edge:{identity}",
        "source": source,
        "predicate": predicate,
        "target": target,
    }


def _source_id(kind: str, namespace: str, value: str) -> str:
    return f"{kind}:{namespace}:{_digest(value)}"


def _semantic_id(kind: str, *values: str) -> str:
    return f"{kind}:derived:{_digest(*values)}"


def _digest(*values: str) -> str:
    digest = hashlib.sha256()
    for value in values:
        encoded = value.encode("utf-8")
        digest.update(len(encoded).to_bytes(8, "big"))
        digest.update(encoded)
    return digest.hexdigest()[:32]


def _required_text(row: Mapping[str, Any], key: str) -> str:
    value = row.get(key)
    if not isinstance(value, str) or not value:
        raise ValueError(f"Navidrome row {key} must be a non-empty string")
    return value


def _optional_text(value: Any) -> str | None:
    if value is None:
        return None
    if not isinstance(value, str):
        raise ValueError("Navidrome text field must be a string or None")
    return value or None


def _optional_int(value: Any) -> int | None:
    if value is None:
        return None
    if isinstance(value, bool) or not isinstance(value, int):
        raise ValueError("Navidrome integer field must be an integer or None")
    return value


def _optional_number(value: Any) -> int | float | None:
    if value is None:
        return None
    if isinstance(value, bool) or not isinstance(value, (int, float)):
        raise ValueError("Navidrome numeric field must be numeric or None")
    return value


__all__ = ["Tek9CatalogBatch", "normalize_navidrome_rows"]
