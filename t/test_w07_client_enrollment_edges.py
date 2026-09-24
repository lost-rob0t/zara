from __future__ import annotations

import json
import os
from pathlib import Path

import pytest
import zmq

import zara.client_enrollment as enrollment
from zara.client_enrollment import ClientEnrollmentError, ClientEnrollmentStore


def _private_write(path: Path, payload: object) -> None:
    path.write_text(json.dumps(payload), encoding="utf-8")
    os.chmod(path, 0o600)


def _identity_payload(store: ClientEnrollmentStore) -> dict[str, object]:
    store.identity_or_create()
    return json.loads(store.path.read_text(encoding="utf-8"))


def test_client_enrollment_rejects_empty_and_oversized_state_files(tmp_path):
    store = ClientEnrollmentStore(tmp_path / "client-curve.json")
    store.path.write_bytes(b"")
    os.chmod(store.path, 0o600)
    with pytest.raises(ClientEnrollmentError, match="invalid size"):
        store.ready_profile()

    store.path.write_bytes(b"x" * (16 * 1024 + 1))
    os.chmod(store.path, 0o600)
    with pytest.raises(ClientEnrollmentError, match="invalid size"):
        store.ready_profile()


def test_client_enrollment_rejects_malformed_or_non_object_json(tmp_path):
    store = ClientEnrollmentStore(tmp_path / "client-curve.json")
    store.path.write_bytes(b"\xff")
    os.chmod(store.path, 0o600)
    with pytest.raises(ClientEnrollmentError, match="invalid or unsafe"):
        store.ready_profile()

    _private_write(store.path, ["not", "an", "object"])
    with pytest.raises(ClientEnrollmentError, match="root must be an object"):
        store.ready_profile()


def test_client_enrollment_rejects_unknown_fields_and_version(tmp_path):
    store = ClientEnrollmentStore(tmp_path / "client-curve.json")
    payload = _identity_payload(store)
    payload["extra"] = True
    _private_write(store.path, payload)
    with pytest.raises(ClientEnrollmentError, match="fields are invalid"):
        store.ready_profile()

    payload.pop("extra")
    payload["version"] = 2
    _private_write(store.path, payload)
    with pytest.raises(ClientEnrollmentError, match="unsupported client enrollment version"):
        store.ready_profile()


def test_client_enrollment_rejects_partial_paired_profile(tmp_path):
    store = ClientEnrollmentStore(tmp_path / "client-curve.json")
    payload = _identity_payload(store)
    payload["endpoint"] = "tcp://127.0.0.1:5555"
    _private_write(store.path, payload)
    with pytest.raises(ClientEnrollmentError, match="server public key is invalid"):
        store.ready_profile()

    payload["endpoint"] = None
    payload["server_public_key"] = zmq.curve_keypair()[0].decode("ascii")
    _private_write(store.path, payload)
    with pytest.raises(ClientEnrollmentError, match="paired endpoint is invalid"):
        store.ready_profile()


def test_client_enrollment_pairing_validation_is_atomic(tmp_path):
    store = ClientEnrollmentStore(tmp_path / "client-curve.json")
    store.identity_or_create()
    before = store.path.read_bytes()
    server_public, _server_secret = zmq.curve_keypair()

    with pytest.raises(ClientEnrollmentError, match="endpoint must not be empty"):
        store.complete_pairing(
            endpoint="   ",
            server_public_key=server_public.decode("ascii"),
        )
    assert store.path.read_bytes() == before

    with pytest.raises(ClientEnrollmentError, match="paired CURVE credentials are invalid"):
        store.complete_pairing(
            endpoint="tcp://127.0.0.1:5555",
            server_public_key="not-a-curve-key",
        )
    assert store.path.read_bytes() == before


def test_client_enrollment_rejects_mismatched_or_non_ascii_identity(tmp_path):
    store = ClientEnrollmentStore(tmp_path / "client-curve.json")
    payload = _identity_payload(store)
    _other_public, other_secret = zmq.curve_keypair()
    payload["secret_key"] = other_secret.decode("ascii")
    _private_write(store.path, payload)
    with pytest.raises(ClientEnrollmentError, match="identity is invalid"):
        store.ready_profile()

    payload["public_key"] = "é" * 40
    _private_write(store.path, payload)
    with pytest.raises(ClientEnrollmentError, match="identity is invalid"):
        store.ready_profile()


def test_client_enrollment_reset_drops_identity_and_recreates_private_state(tmp_path):
    store = ClientEnrollmentStore(tmp_path / "client-curve.json")
    first_public, _first_secret = store.identity_or_create()
    assert store.ready_profile() is None

    store.reset()
    assert not store.path.exists()
    second_public, _second_secret = store.identity_or_create()

    assert second_public != first_public
    assert (store.path.stat().st_mode & 0o777) == 0o600


def test_client_enrollment_replace_failure_leaves_no_partial_state(tmp_path, monkeypatch):
    store = ClientEnrollmentStore(tmp_path / "client-curve.json")

    def fail_replace(_source, _target):
        raise OSError("replace failed")

    monkeypatch.setattr(enrollment.os, "replace", fail_replace)
    with pytest.raises(OSError, match="replace failed"):
        store.identity_or_create()

    assert not store.path.exists()
    assert list(tmp_path.glob(".client-curve.json.*")) == []


def test_client_enrollment_rejects_oversized_encoded_state_before_disk_write(tmp_path):
    store = ClientEnrollmentStore(tmp_path / "client-curve.json")

    with pytest.raises(ClientEnrollmentError, match="state is too large"):
        store._write_payload({"blob": "x" * (16 * 1024)})

    assert not store.path.exists()
