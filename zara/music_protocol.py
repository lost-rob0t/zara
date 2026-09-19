"""Strict Zara Music domain validation layered on the ZARA/1 envelope.

The media plane deliberately does not live here. Audio bytes, artwork, and
transcoded streams stay on OpenSubsonic/HTTP; these messages carry bounded
control, query, job, plan, and progress state only.
"""

from __future__ import annotations

import re
from typing import Any, Mapping


MUSIC_CLIENT_MESSAGE_TYPES = frozenset(
    {
        "music.library.query",
        "music.job.submit",
        "music.job.status",
        "music.job.cancel",
        "music.plan.apply",
    }
)

MUSIC_SERVER_MESSAGE_TYPES = frozenset(
    {
        "music.library.page",
        "music.job.accepted",
        "music.job.status.ok",
        "music.job.cancel.accepted",
        "music.job.progress",
        "music.job.completed",
        "music.job.failed",
        "music.plan.ready",
        "music.plan.apply.accepted",
    }
)

MUSIC_MESSAGE_TYPES = MUSIC_CLIENT_MESSAGE_TYPES | MUSIC_SERVER_MESSAGE_TYPES

MUSIC_JOB_OPERATIONS = frozenset(
    {
        "scan",
        "hash",
        "fingerprint",
        "metadata",
        "dedupe",
        "tag",
        "move",
        "quarantine",
    }
)
MUSIC_MUTATION_OPERATIONS = frozenset({"tag", "move", "quarantine"})
MUSIC_JOB_MODES = frozenset({"execute", "plan"})
MUSIC_JOB_STATES = frozenset(
    {"queued", "running", "planning", "ready", "completed", "failed", "cancelled"}
)

MUSIC_SELECTOR_FIELDS = frozenset(
    {
        "id",
        "path",
        "artist",
        "album",
        "title",
        "genre",
        "year",
        "format",
        "size",
        "modified_ns",
        "duration_ms",
        "recording_id",
        "duplicate_class",
        "metadata_state",
    }
)
MUSIC_SELECTOR_OPERATORS = frozenset(
    {"eq", "ne", "lt", "lte", "gt", "gte", "contains", "prefix", "in"}
)
MUSIC_SORT_DIRECTIONS = frozenset({"asc", "desc"})

MUSIC_PAGE_LIMIT = 500
MUSIC_SELECTOR_DEPTH_LIMIT = 8
MUSIC_SELECTOR_CLAUSE_LIMIT = 64
MUSIC_SORT_LIMIT = 4
MUSIC_PAGE_ITEM_KEYS = frozenset(
    {
        "id",
        "path",
        "size",
        "modified_ns",
        "hash",
        "fingerprint",
        "recording_id",
        "artist",
        "album",
        "title",
        "duration_ms",
        "format",
        "duplicate_class",
        "metadata_state",
    }
)

_TOKEN_RE = re.compile(r"^[A-Za-z0-9_.:@/+\-=]+$")
_SAFE_ERROR_CODES = frozenset(
    {
        "cancelled",
        "conflict",
        "deadline_exceeded",
        "invalid_plan",
        "io_error",
        "not_found",
        "stale_generation",
        "unavailable",
    }
)


class MusicProtocolError(ValueError):
    """A Zara Music body violates the bounded domain contract."""


def _token(name: str, value: Any, *, max_bytes: int = 256) -> str:
    if not isinstance(value, str) or not value:
        raise MusicProtocolError(f"{name} must be a non-empty string")
    try:
        encoded = value.encode("ascii")
    except UnicodeEncodeError as error:
        raise MusicProtocolError(f"{name} must be ASCII") from error
    if len(encoded) > max_bytes or _TOKEN_RE.fullmatch(value) is None:
        raise MusicProtocolError(f"{name} is not a bounded token")
    return value


def _text(name: str, value: Any, *, max_bytes: int) -> str:
    if not isinstance(value, str):
        raise MusicProtocolError(f"{name} must be a string")
    if len(value.encode("utf-8")) > max_bytes:
        raise MusicProtocolError(f"{name} exceeds byte limit")
    if any(ord(character) < 0x20 or ord(character) == 0x7F for character in value):
        raise MusicProtocolError(f"{name} contains control characters")
    return value


def _nonnegative(name: str, value: Any) -> int:
    if type(value) is not int or value < 0:
        raise MusicProtocolError(f"{name} must be a non-negative integer")
    return value


def _positive(name: str, value: Any) -> int:
    if type(value) is not int or value <= 0:
        raise MusicProtocolError(f"{name} must be a positive integer")
    return value


def _bool(name: str, value: Any) -> bool:
    if type(value) is not bool:
        raise MusicProtocolError(f"{name} must be boolean")
    return value


def _body(message: Any) -> dict[str, Any]:
    value = getattr(message, "body", None)
    if not isinstance(value, Mapping):
        raise MusicProtocolError(f"{message.type} requires an object body")
    return dict(value)


def _no_media_fields(message: Any) -> None:
    if getattr(message, "payload_count", 0) != 0:
        raise MusicProtocolError(f"{message.type} does not accept payload frames")
    if any(
        getattr(message, name, None) is not None
        for name in ("conversation_id", "turn_id", "stream_id", "content_type")
    ):
        raise MusicProtocolError(f"{message.type} does not accept media/turn correlation")
    if getattr(message, "flags", None):
        raise MusicProtocolError(f"{message.type} does not accept flags")


def _client_request(message: Any) -> None:
    _no_media_fields(message)
    if getattr(message, "session_id", None) is None:
        raise MusicProtocolError(f"{message.type} requires session_id")
    if getattr(message, "reply_to", None) is not None or getattr(message, "seq", None) is not None:
        raise MusicProtocolError(f"{message.type} is a request, not a reply/event")


def _server_reply(message: Any) -> None:
    _no_media_fields(message)
    if getattr(message, "session_id", None) is None or getattr(message, "reply_to", None) is None:
        raise MusicProtocolError(f"{message.type} requires session_id and reply_to")
    if getattr(message, "seq", None) is not None:
        raise MusicProtocolError(f"{message.type} reply does not accept seq")


def _server_event(message: Any) -> None:
    _no_media_fields(message)
    if getattr(message, "session_id", None) is None or getattr(message, "seq", None) is None:
        raise MusicProtocolError(f"{message.type} requires session_id and seq")
    if getattr(message, "reply_to", None) is not None:
        raise MusicProtocolError(f"{message.type} event does not accept reply_to")


def _scalar(name: str, value: Any) -> Any:
    if value is None or type(value) in (bool, int):
        return value
    if isinstance(value, str):
        return _text(name, value, max_bytes=4096)
    raise MusicProtocolError(f"{name} must be a JSON scalar")


def _selector_node(node: Any, *, depth: int = 1) -> int:
    if depth > MUSIC_SELECTOR_DEPTH_LIMIT:
        raise MusicProtocolError("selector nesting exceeds limit")
    if not isinstance(node, Mapping):
        raise MusicProtocolError("selector node must be an object")
    data = dict(node)

    logical = set(data) & {"all", "any", "not"}
    leaf = {"field", "op", "value"}
    if logical:
        if len(logical) != 1 or set(data) != logical:
            raise MusicProtocolError("logical selector node has invalid fields")
        key = next(iter(logical))
        if key == "not":
            return 1 + _selector_node(data[key], depth=depth + 1)
        children = data[key]
        if not isinstance(children, list) or not children:
            raise MusicProtocolError(f"selector {key} requires a non-empty list")
        clauses = 1
        for child in children:
            clauses += _selector_node(child, depth=depth + 1)
            if clauses > MUSIC_SELECTOR_CLAUSE_LIMIT:
                raise MusicProtocolError("selector clause count exceeds limit")
        return clauses

    if set(data) != leaf:
        raise MusicProtocolError("selector leaf must contain field, op, and value")
    if data["field"] not in MUSIC_SELECTOR_FIELDS:
        raise MusicProtocolError("selector field is not allowed")
    if data["op"] not in MUSIC_SELECTOR_OPERATORS:
        raise MusicProtocolError("selector operator is not allowed")
    if data["op"] == "in":
        values = data["value"]
        if not isinstance(values, list) or not values or len(values) > 64:
            raise MusicProtocolError("selector in requires 1..64 scalar values")
        for index, value in enumerate(values):
            _scalar(f"selector value {index}", value)
    else:
        _scalar("selector value", data["value"])
    return 1


def _selector(value: Any) -> None:
    if not isinstance(value, Mapping):
        raise MusicProtocolError("selector must be an object")
    data = dict(value)
    unknown = set(data) - {"snapshot_id", "where", "order"}
    if unknown:
        raise MusicProtocolError("selector has invalid fields")
    if "snapshot_id" in data and data["snapshot_id"] is not None:
        _token("snapshot_id", data["snapshot_id"])
    if "where" in data and data["where"] is not None:
        clauses = _selector_node(data["where"])
        if clauses > MUSIC_SELECTOR_CLAUSE_LIMIT:
            raise MusicProtocolError("selector clause count exceeds limit")
    if "order" in data:
        order = data["order"]
        if not isinstance(order, list) or len(order) > MUSIC_SORT_LIMIT:
            raise MusicProtocolError("selector order exceeds limit")
        for entry in order:
            if not isinstance(entry, Mapping) or set(entry) != {"field", "direction"}:
                raise MusicProtocolError("selector order entry has invalid fields")
            if entry["field"] not in MUSIC_SELECTOR_FIELDS:
                raise MusicProtocolError("selector order field is not allowed")
            if entry["direction"] not in MUSIC_SORT_DIRECTIONS:
                raise MusicProtocolError("selector order direction is invalid")


def _page_item(value: Any) -> None:
    if not isinstance(value, Mapping):
        raise MusicProtocolError("music page item must be an object")
    item = dict(value)
    if "id" not in item or set(item) - MUSIC_PAGE_ITEM_KEYS:
        raise MusicProtocolError("music page item has invalid fields")
    _token("item id", item["id"])
    for key, value in item.items():
        if key in {"size", "modified_ns", "duration_ms"}:
            _nonnegative(key, value)
        elif key == "path":
            _text(key, value, max_bytes=4096)
        elif key != "id" and value is not None:
            _text(key, value, max_bytes=1024)


def _job_status_body(body: Mapping[str, Any]) -> None:
    required = {
        "job_id",
        "operation",
        "state",
        "processed",
        "total",
        "errors",
        "generation",
        "last_seq",
    }
    optional = {"plan_id", "result_ref"}
    if not required <= set(body) or set(body) - required - optional:
        raise MusicProtocolError("music job status body has invalid fields")
    _token("job_id", body["job_id"])
    if body["operation"] not in MUSIC_JOB_OPERATIONS:
        raise MusicProtocolError("music job operation is invalid")
    if body["state"] not in MUSIC_JOB_STATES:
        raise MusicProtocolError("music job state is invalid")
    _nonnegative("processed", body["processed"])
    if body["total"] is not None:
        _nonnegative("total", body["total"])
    _nonnegative("errors", body["errors"])
    _nonnegative("generation", body["generation"])
    _nonnegative("last_seq", body["last_seq"])
    if "plan_id" in body and body["plan_id"] is not None:
        _token("plan_id", body["plan_id"])
    if "result_ref" in body and body["result_ref"] is not None:
        _token("result_ref", body["result_ref"], max_bytes=512)


def validate_music_message(message: Any) -> None:
    """Validate one already-framed ZARA/1 music-domain message."""

    if getattr(message, "type", None) not in MUSIC_MESSAGE_TYPES:
        return

    if message.type == "music.library.query":
        _client_request(message)
        body = _body(message)
        if set(body) not in (
            {"library_id", "selector", "page_size"},
            {"library_id", "selector", "page_size", "cursor"},
        ):
            raise MusicProtocolError("music.library.query body has invalid fields")
        _token("library_id", body["library_id"])
        _selector(body["selector"])
        page_size = _positive("page_size", body["page_size"])
        if page_size > MUSIC_PAGE_LIMIT:
            raise MusicProtocolError("page_size exceeds music page limit")
        if "cursor" in body and body["cursor"] is not None:
            _token("cursor", body["cursor"], max_bytes=512)
        return

    if message.type == "music.library.page":
        _server_reply(message)
        body = _body(message)
        required = {
            "library_id",
            "snapshot_id",
            "generation",
            "items",
            "next_cursor",
            "complete",
        }
        if set(body) != required:
            raise MusicProtocolError("music.library.page body has invalid fields")
        _token("library_id", body["library_id"])
        _token("snapshot_id", body["snapshot_id"])
        _nonnegative("generation", body["generation"])
        if not isinstance(body["items"], list) or len(body["items"]) > MUSIC_PAGE_LIMIT:
            raise MusicProtocolError("music page item count exceeds limit")
        for item in body["items"]:
            _page_item(item)
        if body["next_cursor"] is not None:
            _token("next_cursor", body["next_cursor"], max_bytes=512)
        _bool("complete", body["complete"])
        if body["complete"] and body["next_cursor"] is not None:
            raise MusicProtocolError("complete music page cannot carry next_cursor")
        return

    if message.type == "music.job.submit":
        _client_request(message)
        body = _body(message)
        required = {
            "library_id",
            "operation",
            "mode",
            "selector",
            "idempotency_key",
            "deadline_ns",
        }
        if set(body) != required:
            raise MusicProtocolError("music.job.submit body has invalid fields")
        _token("library_id", body["library_id"])
        if body["operation"] not in MUSIC_JOB_OPERATIONS:
            raise MusicProtocolError("music job operation is invalid")
        if body["mode"] not in MUSIC_JOB_MODES:
            raise MusicProtocolError("music job mode is invalid")
        if body["operation"] in MUSIC_MUTATION_OPERATIONS and body["mode"] != "plan":
            raise MusicProtocolError("music mutation jobs must be submitted in plan mode")
        _selector(body["selector"])
        _token("idempotency_key", body["idempotency_key"])
        if body["deadline_ns"] is not None:
            _positive("deadline_ns", body["deadline_ns"])
        return

    if message.type in {"music.job.status", "music.job.cancel"}:
        _client_request(message)
        body = _body(message)
        if set(body) != {"job_id"}:
            raise MusicProtocolError(f"{message.type} body has invalid fields")
        _token("job_id", body["job_id"])
        return

    if message.type == "music.plan.apply":
        _client_request(message)
        body = _body(message)
        if set(body) != {"plan_id", "expected_generation", "idempotency_key"}:
            raise MusicProtocolError("music.plan.apply body has invalid fields")
        _token("plan_id", body["plan_id"])
        _nonnegative("expected_generation", body["expected_generation"])
        _token("idempotency_key", body["idempotency_key"])
        return

    if message.type == "music.job.accepted":
        _server_reply(message)
        body = _body(message)
        if set(body) != {"job_id", "operation", "state", "generation", "resumable"}:
            raise MusicProtocolError("music.job.accepted body has invalid fields")
        _token("job_id", body["job_id"])
        if body["operation"] not in MUSIC_JOB_OPERATIONS:
            raise MusicProtocolError("music job operation is invalid")
        if body["state"] not in {"queued", "running", "planning"}:
            raise MusicProtocolError("music accepted job state is invalid")
        _nonnegative("generation", body["generation"])
        _bool("resumable", body["resumable"])
        return

    if message.type == "music.job.status.ok":
        _server_reply(message)
        _job_status_body(_body(message))
        return

    if message.type == "music.job.cancel.accepted":
        _server_reply(message)
        body = _body(message)
        if set(body) != {"job_id"}:
            raise MusicProtocolError("music.job.cancel.accepted body has invalid fields")
        _token("job_id", body["job_id"])
        return

    if message.type in {"music.job.progress", "music.job.completed"}:
        _server_event(message)
        body = _body(message)
        _job_status_body(body)
        if body["last_seq"] != message.seq:
            raise MusicProtocolError(f"{message.type} last_seq must match seq")
        expected = "completed" if message.type == "music.job.completed" else {
            "running",
            "planning",
            "ready",
        }
        if isinstance(expected, str):
            valid = body["state"] == expected
        else:
            valid = body["state"] in expected
        if not valid:
            raise MusicProtocolError(f"{message.type} state is invalid")
        return

    if message.type == "music.job.failed":
        _server_event(message)
        body = _body(message)
        if set(body) != {"job_id", "code", "message", "retryable"}:
            raise MusicProtocolError("music.job.failed body has invalid fields")
        _token("job_id", body["job_id"])
        if body["code"] not in _SAFE_ERROR_CODES:
            raise MusicProtocolError("music job failure code is invalid")
        _text("message", body["message"], max_bytes=256)
        _bool("retryable", body["retryable"])
        return

    if message.type == "music.plan.ready":
        _server_event(message)
        body = _body(message)
        if set(body) != {
            "job_id",
            "plan_id",
            "generation",
            "action_count",
            "expires_ns",
        }:
            raise MusicProtocolError("music.plan.ready body has invalid fields")
        _token("job_id", body["job_id"])
        _token("plan_id", body["plan_id"])
        _nonnegative("generation", body["generation"])
        _nonnegative("action_count", body["action_count"])
        _positive("expires_ns", body["expires_ns"])
        return

    if message.type == "music.plan.apply.accepted":
        _server_reply(message)
        body = _body(message)
        if set(body) != {"job_id", "plan_id"}:
            raise MusicProtocolError("music.plan.apply.accepted body has invalid fields")
        _token("job_id", body["job_id"])
        _token("plan_id", body["plan_id"])
        return

    raise MusicProtocolError("unhandled music message type")


__all__ = [
    "MUSIC_CLIENT_MESSAGE_TYPES",
    "MUSIC_JOB_MODES",
    "MUSIC_JOB_OPERATIONS",
    "MUSIC_JOB_STATES",
    "MUSIC_MESSAGE_TYPES",
    "MUSIC_MUTATION_OPERATIONS",
    "MUSIC_PAGE_LIMIT",
    "MUSIC_SELECTOR_CLAUSE_LIMIT",
    "MUSIC_SELECTOR_DEPTH_LIMIT",
    "MUSIC_SELECTOR_FIELDS",
    "MUSIC_SELECTOR_OPERATORS",
    "MUSIC_SERVER_MESSAGE_TYPES",
    "MusicProtocolError",
    "validate_music_message",
]
