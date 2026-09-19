from __future__ import annotations

import importlib.util
import subprocess
import sys
from pathlib import Path

import pytest

from zara.music_protocol import MUSIC_PAGE_LIMIT
from zara.protocol import ProtocolMessage, ProtocolValidationError, decode_message, encode_message


ROOT = Path(__file__).resolve().parents[1]


def message(message_type: str, **overrides) -> ProtocolMessage:
    values = {
        "type": message_type,
        "id": "music-request-1",
        "timestamp_ns": 100,
        "payload_count": 0,
        "session_id": "session-1",
    }
    values.update(overrides)
    return ProtocolMessage(**values)


def selector(**overrides):
    value = {
        "snapshot_id": "snapshot-1",
        "where": {
            "all": [
                {"field": "artist", "op": "eq", "value": "Burial"},
                {"field": "year", "op": "gte", "value": 2000},
            ]
        },
        "order": [{"field": "album", "direction": "asc"}],
    }
    value.update(overrides)
    return value


def test_music_query_round_trips_as_bounded_zara1_control_message():
    original = message(
        "music.library.query",
        body={
            "library_id": "main",
            "selector": selector(),
            "page_size": 100,
            "cursor": "cursor-1",
        },
    )

    decoded = decode_message(encode_message(original))

    assert decoded.message == original
    assert decoded.payloads == ()


@pytest.mark.parametrize("page_size", [0, MUSIC_PAGE_LIMIT + 1])
def test_music_query_rejects_invalid_page_bounds(page_size):
    with pytest.raises(ProtocolValidationError):
        encode_message(
            message(
                "music.library.query",
                body={
                    "library_id": "main",
                    "selector": {},
                    "page_size": page_size,
                },
            )
        )


def test_music_selector_rejects_unknown_field_and_unbounded_tree():
    with pytest.raises(ProtocolValidationError):
        encode_message(
            message(
                "music.library.query",
                body={
                    "library_id": "main",
                    "selector": {
                        "where": {"field": "shell_command", "op": "eq", "value": "rm"}
                    },
                    "page_size": 10,
                },
            )
        )

    node = {"field": "artist", "op": "eq", "value": "Burial"}
    for _ in range(9):
        node = {"not": node}
    with pytest.raises(ProtocolValidationError, match="nesting"):
        encode_message(
            message(
                "music.library.query",
                body={
                    "library_id": "main",
                    "selector": {"where": node},
                    "page_size": 10,
                },
            )
        )


def test_mutating_job_must_plan_before_apply():
    with pytest.raises(ProtocolValidationError, match="plan mode"):
        encode_message(
            message(
                "music.job.submit",
                body={
                    "library_id": "main",
                    "operation": "move",
                    "mode": "execute",
                    "selector": selector(),
                    "idempotency_key": "move-1",
                    "deadline_ns": None,
                },
            )
        )

    planned = message(
        "music.job.submit",
        body={
            "library_id": "main",
            "operation": "move",
            "mode": "plan",
            "selector": selector(),
            "idempotency_key": "move-plan-1",
            "deadline_ns": 200,
        },
    )
    assert decode_message(encode_message(planned)).message == planned


def test_plan_apply_carries_generation_fence_and_idempotency_key():
    apply = message(
        "music.plan.apply",
        body={
            "plan_id": "plan-7",
            "expected_generation": 42,
            "idempotency_key": "apply-7",
        },
    )

    assert decode_message(encode_message(apply)).message == apply

    with pytest.raises(ProtocolValidationError):
        encode_message(
            message(
                "music.plan.apply",
                body={
                    "plan_id": "plan-7",
                    "expected_generation": -1,
                    "idempotency_key": "apply-7",
                },
            )
        )


def test_music_page_is_bounded_and_snapshot_pinned():
    page = message(
        "music.library.page",
        reply_to="music-request-1",
        body={
            "library_id": "main",
            "snapshot_id": "snapshot-1",
            "generation": 7,
            "items": [
                {
                    "id": "file-1",
                    "path": "/music/Burial/Untrue/01 Untitled.flac",
                    "size": 123,
                    "modified_ns": 456,
                    "artist": "Burial",
                    "album": "Untrue",
                    "title": "Untitled",
                }
            ],
            "next_cursor": None,
            "complete": True,
        },
    )

    assert decode_message(encode_message(page)).message == page

    page.body["items"].extend({"id": f"file-{index}"} for index in range(MUSIC_PAGE_LIMIT))
    with pytest.raises(ProtocolValidationError, match="item count"):
        encode_message(page)


def test_incomplete_music_page_requires_continuation_cursor():
    with pytest.raises(ProtocolValidationError, match="next_cursor"):
        encode_message(
            message(
                "music.library.page",
                reply_to="music-request-1",
                body={
                    "library_id": "main",
                    "snapshot_id": "snapshot-1",
                    "generation": 7,
                    "items": [],
                    "next_cursor": None,
                    "complete": False,
                },
            )
        )


def test_job_progress_is_an_event_with_sequence_not_a_reply():
    progress = message(
        "music.job.progress",
        seq=4,
        body={
            "job_id": "job-1",
            "operation": "fingerprint",
            "state": "running",
            "processed": 300,
            "total": 1000,
            "errors": 0,
            "generation": 9,
            "last_seq": 4,
        },
    )

    assert decode_message(encode_message(progress)).message == progress

    with pytest.raises(ProtocolValidationError):
        encode_message(
            message(
                "music.job.progress",
                reply_to="request-1",
                seq=4,
                body=progress.body,
            )
        )


def test_job_progress_and_completion_reject_split_resume_watermarks():
    body = {
        "job_id": "job-1",
        "operation": "fingerprint",
        "state": "running",
        "processed": 300,
        "total": 1000,
        "errors": 0,
        "generation": 9,
        "last_seq": 3,
    }

    with pytest.raises(ProtocolValidationError, match="last_seq"):
        encode_message(message("music.job.progress", seq=4, body=body))

    completed = dict(body, state="completed", processed=1000, last_seq=5)
    with pytest.raises(ProtocolValidationError, match="last_seq"):
        encode_message(message("music.job.completed", seq=6, body=completed))


def test_music_control_messages_never_carry_audio_or_binary_payloads():
    with pytest.raises(ProtocolValidationError, match="payload"):
        encode_message(
            message(
                "music.job.status",
                payload_count=1,
                body={"job_id": "job-1"},
            ),
            payloads=(b"not-audio",),
        )


def test_org_generated_music_protocol_artifacts_are_current():
    subprocess.run(
        [sys.executable, str(ROOT / "scripts" / "tangle-music-protocol.py"), "--check"],
        cwd=ROOT,
        check=True,
    )


def test_org_tangler_rejects_unapproved_target():
    path = ROOT / "scripts" / "tangle-music-protocol.py"
    spec = importlib.util.spec_from_file_location("music_tangler", path)
    assert spec and spec.loader
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)

    with pytest.raises(module.TangleError, match="unapproved"):
        module.extract(
            "#+begin_src prolog :tangle /tmp/escape.pl\n"
            "owned.\n"
            "#+end_src\n"
        )
