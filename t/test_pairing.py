from __future__ import annotations

import json
import socket
import threading
import time
from urllib.parse import parse_qs, urlparse

import pytest

from zara.pairing import (
    PairingBroker,
    PairingExpired,
    derive_device_id,
    verification_code,
)


SERVER_KEY = "a" * 40
CLIENT_KEY = "b" * 40


def _send_request(address: tuple[str, int], token: str, public_key: str = CLIENT_KEY):
    connection = socket.create_connection(address, timeout=2.0)
    stream = connection.makefile("rwb", buffering=0)
    request = {
        "version": 1,
        "token": token,
        "public_key": public_key,
    }
    stream.write(json.dumps(request, separators=(",", ":")).encode("utf-8") + b"\n")
    return connection, stream


def _read_json_line(stream) -> dict[str, object]:
    raw = stream.readline(4097)
    assert raw.endswith(b"\n")
    assert len(raw) <= 4096
    payload = json.loads(raw)
    assert isinstance(payload, dict)
    return payload


def test_pairing_uri_contains_only_public_bootstrap_material() -> None:
    expires_at = int(time.time()) + 120
    broker = PairingBroker(
        endpoint="tcp://10.0.0.5:7731",
        server_public_key=SERVER_KEY,
        token="pairing-token",
        expires_at=expires_at,
        bind_host="127.0.0.1",
    )
    try:
        port = broker.address[1]
        uri = broker.pairing_uri(advertise_host="10.0.0.5")
    finally:
        broker.close()

    parsed = urlparse(uri)
    query = parse_qs(parsed.query, strict_parsing=True)
    assert parsed.scheme == "zara"
    assert parsed.netloc == "pair"
    assert parsed.path == "/v1"
    assert query == {
        "broker_host": ["10.0.0.5"],
        "broker_port": [str(port)],
        "endpoint": ["tcp://10.0.0.5:7731"],
        "server_key": [SERVER_KEY],
        "token": ["pairing-token"],
        "expires": [str(expires_at)],
    }
    assert "secret_key" not in uri


def test_valid_claim_requires_explicit_approval_before_live_enrollment() -> None:
    enrolled: list[tuple[str, str]] = []
    broker = PairingBroker(
        endpoint="tcp://127.0.0.1:7731",
        server_public_key=SERVER_KEY,
        token="correct-horse-battery-staple",
        expires_at=int(time.time()) + 30,
        bind_host="127.0.0.1",
        enroll=lambda public_key, device_id: enrolled.append((public_key, device_id)),
    )
    received: list[dict[str, object]] = []

    def client() -> None:
        connection, stream = _send_request(broker.address, broker.token)
        try:
            received.append(_read_json_line(stream))
            received.append(_read_json_line(stream))
        finally:
            stream.close()
            connection.close()

    thread = threading.Thread(target=client, daemon=True)
    thread.start()
    try:
        claim = broker.wait_for_claim()
        assert enrolled == []
        assert claim.public_key == CLIENT_KEY
        assert claim.device_id == derive_device_id(CLIENT_KEY)
        assert claim.verification_code == verification_code(broker.token, CLIENT_KEY)

        broker.approve(claim)
        thread.join(timeout=2.0)
    finally:
        broker.close()

    assert not thread.is_alive()
    assert enrolled == [(CLIENT_KEY, derive_device_id(CLIENT_KEY))]
    assert received[0] == {
        "status": "pending",
        "verification_code": verification_code(broker.token, CLIENT_KEY),
        "device_id": derive_device_id(CLIENT_KEY),
    }
    assert received[1] == {
        "status": "approved",
        "endpoint": "tcp://127.0.0.1:7731",
        "server_key": SERVER_KEY,
        "device_id": derive_device_id(CLIENT_KEY),
    }


def test_wrong_token_is_rejected_without_consuming_pairing_session() -> None:
    broker = PairingBroker(
        endpoint="tcp://127.0.0.1:7731",
        server_public_key=SERVER_KEY,
        token="right-token",
        expires_at=int(time.time()) + 30,
        bind_host="127.0.0.1",
    )
    wrong_connection, wrong_stream = _send_request(broker.address, "wrong-token")
    try:
        response = _read_json_line(wrong_stream)
        assert response == {"status": "rejected", "code": "invalid_token"}
    finally:
        wrong_stream.close()
        wrong_connection.close()

    good_connection, good_stream = _send_request(broker.address, broker.token)
    try:
        claim = broker.wait_for_claim()
        broker.reject(claim, code="operator_rejected")
        assert _read_json_line(good_stream) == {
            "status": "pending",
            "verification_code": verification_code(broker.token, CLIENT_KEY),
            "device_id": derive_device_id(CLIENT_KEY),
        }
        assert _read_json_line(good_stream) == {
            "status": "rejected",
            "code": "operator_rejected",
        }
    finally:
        good_stream.close()
        good_connection.close()
        broker.close()


def test_expired_pairing_cannot_be_claimed() -> None:
    broker = PairingBroker(
        endpoint="tcp://127.0.0.1:7731",
        server_public_key=SERVER_KEY,
        token="expired-token",
        expires_at=int(time.time()) - 1,
        bind_host="127.0.0.1",
    )
    try:
        with pytest.raises(PairingExpired, match="expired"):
            broker.wait_for_claim()
    finally:
        broker.close()


def test_device_id_and_confirmation_code_are_stable_and_key_bound() -> None:
    assert derive_device_id(CLIENT_KEY).startswith("android-")
    assert derive_device_id(CLIENT_KEY) == derive_device_id(CLIENT_KEY)
    assert derive_device_id(CLIENT_KEY) != derive_device_id("c" * 40)

    code = verification_code("pairing-token", CLIENT_KEY)
    assert len(code) == 6
    assert code.isdigit()
    assert code == verification_code("pairing-token", CLIENT_KEY)
    assert code != verification_code("pairing-token", "c" * 40)
