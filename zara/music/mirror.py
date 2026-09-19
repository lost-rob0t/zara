"""Crash-resumable Navidrome -> Tek9 whole-library import."""

from __future__ import annotations

import json
import uuid
from dataclasses import dataclass
from pathlib import Path
from typing import Callable

from zara.music.catalog import normalize_navidrome_rows
from zara.music.navidrome_db import NavidromeLiveReader
from zara.music.tek9_worker import Tek9WorkerError, Tek9WorkerUnavailable


_SOURCE_ID = "navidrome"
_WATERMARK_VERSION = 1


@dataclass(frozen=True)
class MusicImportResult:
    rows: int
    batches: int
    generation: int
    phase: str


class NavidromeTek9Importer:
    """Import one consistent Navidrome snapshot into the canonical Tek9 graph."""

    def __init__(
        self,
        *,
        reader: NavidromeLiveReader,
        tek9,
        state_dir: str | Path,
        snapshot_name_factory: Callable[[], str] | None = None,
    ) -> None:
        self._reader = reader
        self._tek9 = tek9
        self._state_dir = Path(state_dir).expanduser().resolve()
        self._snapshot_name_factory = snapshot_name_factory or (
            lambda: f"navidrome-{uuid.uuid4().hex}.db"
        )

    def import_all(self, *, batch_size: int = 1000) -> MusicImportResult:
        self._state_dir.mkdir(parents=True, exist_ok=True)
        status = self._tek9.status(_SOURCE_ID)
        generation = _status_generation(status)
        checkpoint = _decode_watermark(status.get("watermark"))

        if checkpoint is not None and checkpoint["phase"] == "live":
            return MusicImportResult(
                rows=0,
                batches=0,
                generation=generation,
                phase="live",
            )

        snapshot_path, last_id = self._resume_or_snapshot(checkpoint)
        rows = 0
        batches = 0

        with NavidromeLiveReader(snapshot_path) as snapshot:
            snapshot_schema = snapshot.schema_info().migration_version
            if checkpoint is not None and checkpoint["phase"] == "snapshot":
                expected_schema = checkpoint["schema_version"]
                if snapshot_schema != expected_schema:
                    raise ValueError(
                        "resume snapshot schema does not match its checkpoint"
                    )

            for raw_batch in snapshot.iter_media_batches(
                batch_size=batch_size,
                after_id=last_id,
            ):
                normalized = normalize_navidrome_rows(raw_batch)
                last_id = str(raw_batch[-1]["id"])
                next_generation = generation + 1
                watermark = _encode_watermark(
                    {
                        "v": _WATERMARK_VERSION,
                        "phase": "snapshot",
                        "snapshot": snapshot_path.name,
                        "last_id": last_id,
                        "schema_version": snapshot_schema,
                    }
                )
                self._apply_with_reconcile(
                    generation=next_generation,
                    expected_generation=generation,
                    watermark=watermark,
                    documents=normalized.documents,
                    nodes=normalized.nodes,
                    edges=normalized.edges,
                )
                generation = next_generation
                rows += len(raw_batch)
                batches += 1

        current_schema = self._reader.schema_info().migration_version
        if current_schema != snapshot_schema:
            raise Tek9WorkerError(
                "Navidrome schema changed during import; keep snapshot and rerun"
            )

        live_watermark = _encode_watermark(
            {
                "v": _WATERMARK_VERSION,
                "phase": "live",
                "schema_version": current_schema,
                "data_version": self._reader.data_version(),
            }
        )
        next_generation = generation + 1
        self._apply_with_reconcile(
            generation=next_generation,
            expected_generation=generation,
            watermark=live_watermark,
            documents=(),
            nodes=(),
            edges=(),
        )
        generation = next_generation
        snapshot_path.unlink(missing_ok=True)

        return MusicImportResult(
            rows=rows,
            batches=batches,
            generation=generation,
            phase="live",
        )

    def _resume_or_snapshot(
        self,
        checkpoint: dict[str, object] | None,
    ) -> tuple[Path, str | None]:
        if checkpoint is not None and checkpoint["phase"] == "snapshot":
            name = _safe_snapshot_name(checkpoint["snapshot"])
            candidate = self._state_dir / name
            if candidate.is_file():
                last_id = checkpoint.get("last_id")
                if last_id is not None and not isinstance(last_id, str):
                    raise ValueError("snapshot last_id is invalid")
                return candidate, last_id

        name = _safe_snapshot_name(self._snapshot_name_factory())
        destination = self._state_dir / name
        self._reader.create_snapshot(destination)
        return destination, None

    def _apply_with_reconcile(
        self,
        *,
        generation: int,
        expected_generation: int,
        watermark: str,
        documents,
        nodes,
        edges,
    ) -> dict[str, object]:
        try:
            return self._tek9.apply_batch(
                source_id=_SOURCE_ID,
                generation=generation,
                expected_generation=expected_generation,
                documents=documents,
                nodes=nodes,
                edges=edges,
                graph_name="music",
                watermark=watermark,
            )
        except Tek9WorkerUnavailable:
            status = self._tek9.status(_SOURCE_ID)
            actual = _status_generation(status)
            if actual == generation and status.get("watermark") == watermark:
                return status
            if actual == expected_generation:
                raise
            raise Tek9WorkerError(
                "Tek9 worker outcome is ambiguous after pipe failure: "
                f"expected generation {expected_generation}, observed {actual}"
            )


def _status_generation(status: object) -> int:
    if not isinstance(status, dict):
        raise Tek9WorkerError("Tek9 status response is not an object")
    generation = status.get("generation")
    if isinstance(generation, bool) or not isinstance(generation, int):
        raise Tek9WorkerError("Tek9 status generation is invalid")
    if generation < 0:
        raise Tek9WorkerError("Tek9 status generation is negative")
    return generation


def _decode_watermark(value: object) -> dict[str, object] | None:
    if value is None:
        return None
    if not isinstance(value, str):
        raise ValueError("Tek9 Navidrome watermark must be a JSON string")
    try:
        parsed = json.loads(value)
    except (TypeError, ValueError, json.JSONDecodeError) as error:
        raise ValueError("Tek9 Navidrome watermark is invalid JSON") from error
    if not isinstance(parsed, dict) or parsed.get("v") != _WATERMARK_VERSION:
        raise ValueError("Tek9 Navidrome watermark version is unsupported")
    phase = parsed.get("phase")
    if phase == "snapshot":
        if set(parsed) != {
            "v",
            "phase",
            "snapshot",
            "last_id",
            "schema_version",
        }:
            raise ValueError("snapshot watermark has invalid fields")
        _safe_snapshot_name(parsed["snapshot"])
        if parsed["last_id"] is not None and not isinstance(parsed["last_id"], str):
            raise ValueError("snapshot watermark last_id is invalid")
        if isinstance(parsed["schema_version"], bool) or not isinstance(
            parsed["schema_version"], int
        ):
            raise ValueError("snapshot watermark schema_version is invalid")
        return parsed
    if phase == "live":
        if set(parsed) != {
            "v",
            "phase",
            "schema_version",
            "data_version",
        }:
            raise ValueError("live watermark has invalid fields")
        for key in ("schema_version", "data_version"):
            if isinstance(parsed[key], bool) or not isinstance(parsed[key], int):
                raise ValueError(f"live watermark {key} is invalid")
        return parsed
    raise ValueError("Tek9 Navidrome watermark phase is invalid")


def _encode_watermark(value: dict[str, object]) -> str:
    return json.dumps(
        value,
        ensure_ascii=False,
        allow_nan=False,
        sort_keys=True,
        separators=(",", ":"),
    )


def _safe_snapshot_name(value: object) -> str:
    if not isinstance(value, str) or not value:
        raise ValueError("snapshot name must be a non-empty string")
    path = Path(value)
    if path.name != value or value in {".", ".."}:
        raise ValueError("snapshot name must not escape the state directory")
    if len(value.encode("utf-8")) > 255:
        raise ValueError("snapshot name is too long")
    return value


__all__ = ["MusicImportResult", "NavidromeTek9Importer"]
