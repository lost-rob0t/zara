from __future__ import annotations

import json
import os
import random
import socket
import string
from pathlib import Path

import pytest
from zmq.utils import z85

from zara.principals import PrincipalContext
from zara.security import Capability, KeyNotActive, SecurityError, SecurityRegistry
from zara.security_admin import (
    SecurityAdminError,
    SecurityAdminServer,
    _recv_message,
)
from zara.security_state import PersistentSecurityState, SecurityStateError


_FUZZ_SEED_COUNT = int(os.environ.get("ZARA_SECURITY_FUZZ_SEEDS", "16"))
if not 1 <= _FUZZ_SEED_COUNT <= 4096:
    raise RuntimeError("ZARA_SECURITY_FUZZ_SEEDS must be between 1 and 4096")
FUZZ_SEEDS = tuple(range(_FUZZ_SEED_COUNT))


def _jsonish(rng: random.Random, depth: int = 0):
    if depth >= 3:
        return rng.choice(
            [
                None,
                True,
                False,
                rng.randint(-(2**31), 2**31),
                "".join(rng.choice(string.printable) for _ in range(rng.randrange(0, 32))),
            ]
        )
    choice = rng.randrange(6)
    if choice == 0:
        return None
    if choice == 1:
        return rng.randint(-(2**31), 2**31)
    if choice == 2:
        return "".join(rng.choice(string.printable) for _ in range(rng.randrange(0, 48)))
    if choice == 3:
        return [_jsonish(rng, depth + 1) for _ in range(rng.randrange(0, 6))]
    if choice == 4:
        return {
            "".join(rng.choice(string.ascii_letters) for _ in range(rng.randrange(1, 12))): _jsonish(
                rng, depth + 1
            )
            for _ in range(rng.randrange(0, 6))
        }
    return bool(rng.getrandbits(1))


def _valid_curve_key(rng: random.Random) -> bytes:
    raw = bytes(rng.getrandbits(8) for _ in range(32))
    return z85.encode(raw)


def _invalid_curve_key(public_key: bytes, rng: random.Random) -> str | bytes:
    key = public_key.decode("ascii")
    mutation = rng.randrange(6)
    if mutation == 0:
        return key[:-1]
    if mutation == 1:
        return key + "0"
    if mutation == 2:
        index = rng.randrange(len(key))
        return key[:index] + "\x00" + key[index + 1 :]
    if mutation == 3:
        index = rng.randrange(len(key))
        return key[:index] + "☃" + key[index + 1 :]
    if mutation == 4:
        return b"\xff" + public_key[1:]
    return " " + key


def _invalid_json_curve_key(public_key: bytes, rng: random.Random) -> str:
    """Return only JSON-native invalid key forms for persisted-record fuzzing."""
    malformed = _invalid_curve_key(public_key, rng)
    if isinstance(malformed, bytes):
        return "☃" + public_key[1:].decode("ascii")
    return malformed


def _invalid_device_id(rng: random.Random) -> str:
    mutation = rng.randrange(6)
    if mutation == 0:
        return ""
    if mutation == 1:
        return " " * rng.randrange(1, 8)
    if mutation == 2:
        return " leading"
    if mutation == 3:
        return "trailing "
    if mutation == 4:
        control = chr(rng.choice([0, 1, 9, 10, 13, 31, 127]))
        return f"device{control}id"
    if rng.getrandbits(1):
        return "x" * rng.randrange(129, 512)
    return "🙂" * rng.randrange(33, 96)


def _recv_frame(payload: bytes, *, limit: int = 512):
    receiver, sender = socket.socketpair()
    try:
        sender.sendall(payload)
        sender.shutdown(socket.SHUT_WR)
        return _recv_message(receiver, limit=limit)
    finally:
        receiver.close()
        sender.close()


@pytest.mark.parametrize("seed", FUZZ_SEEDS)
def test_curve_public_key_mutation_fuzz_fails_closed(seed: int):
    rng = random.Random(seed)
    registry = SecurityRegistry()
    principal = PrincipalContext.local_owner()

    for index in range(32):
        public_key = _valid_curve_key(rng)
        malformed = _invalid_curve_key(public_key, rng)
        with pytest.raises(ValueError):
            registry.enroll(
                malformed,
                principal=principal,
                device_id=f"fuzz-device-{seed}-{index}",
                capabilities={Capability.SESSION_BASIC},
            )
        with pytest.raises(KeyNotActive):
            registry.resolve_public_key(public_key)


@pytest.mark.parametrize("seed", FUZZ_SEEDS)
def test_device_id_mutation_fuzz_is_bounded_and_side_effect_free(seed: int):
    rng = random.Random(seed ^ 0x5A5A)
    registry = SecurityRegistry()
    principal = PrincipalContext.local_owner()

    for _ in range(32):
        public_key = _valid_curve_key(rng)
        malformed = _invalid_device_id(rng)
        with pytest.raises(ValueError):
            registry.enroll(
                public_key,
                principal=principal,
                device_id=malformed,
                capabilities={Capability.SESSION_BASIC},
            )
        with pytest.raises(KeyNotActive):
            registry.resolve_public_key(public_key)


@pytest.mark.parametrize("seed", FUZZ_SEEDS)
def test_admin_framing_mutation_fuzz_has_closed_exception_surface(seed: int):
    rng = random.Random(seed ^ 0xA5A5)
    for _ in range(32):
        mutation = rng.randrange(4)
        if mutation == 0:
            payload = bytes([0xFF]) + bytes(rng.getrandbits(8) for _ in range(rng.randrange(0, 64))) + b"\n"
        elif mutation == 1:
            payload = b'{"version":1,"action":"list"}\n' + bytes(
                rng.choice(string.ascii_letters).encode("ascii")[0] for _ in range(rng.randrange(1, 32))
            )
        elif mutation == 2:
            payload = b'{"version":1,"action":"list"}'
        else:
            payload = bytes(rng.getrandbits(8) for _ in range(513))
        with pytest.raises(SecurityAdminError):
            _recv_frame(payload)


@pytest.mark.parametrize("seed", FUZZ_SEEDS)
def test_persistent_security_state_json_fuzz_has_closed_exception_surface(
    tmp_path: Path,
    seed: int,
):
    rng = random.Random(seed)
    state = PersistentSecurityState(tmp_path / f"security-{seed}")
    state.initialize()
    clients_path = state.directory / "clients.json"

    corpus: list[bytes] = [
        b"",
        b"null\n",
        b"[]\n",
        b"{}\n",
        b'{"version":1,"clients":null}\n',
        b'{"version":999,"clients":[]}\n',
        b'{"version":1,"clients":[{"active":true}]}\n',
        b"\xff\xfe\xfd\n",
    ]
    for _ in range(40):
        value = _jsonish(rng)
        corpus.append((json.dumps(value, ensure_ascii=False) + "\n").encode("utf-8"))

    for payload in corpus:
        clients_path.write_bytes(payload)
        os.chmod(clients_path, 0o600)
        try:
            state.load_registry()
        except SecurityStateError:
            continue
        # Random input may occasionally synthesize a valid empty registry. If
        # accepted, it still has to survive the typed validation path.
        clients = state.list_clients()
        assert isinstance(clients, tuple)
        for client in clients:
            assert set(client) == {
                "public_key",
                "principal_id",
                "principal_kind",
                "device_id",
                "capabilities",
                "active",
            }


@pytest.mark.parametrize("seed", FUZZ_SEEDS)
def test_persisted_client_record_mutation_fuzz_fails_closed(tmp_path: Path, seed: int):
    rng = random.Random(seed ^ 0xC0DE)
    state = PersistentSecurityState(tmp_path / f"structured-{seed}")
    state.initialize()
    clients_path = state.directory / "clients.json"

    for index in range(32):
        record: dict[str, object] = {
            "public_key": _valid_curve_key(rng).decode("ascii"),
            "principal_id": "local-owner",
            "principal_kind": "owner",
            "device_id": f"structured-{seed}-{index}",
            "capabilities": [Capability.SESSION_BASIC.value],
            "active": True,
        }
        mutation = rng.randrange(9)
        if mutation == 0:
            record["public_key"] = _invalid_json_curve_key(_valid_curve_key(rng), rng)
        elif mutation == 1:
            record["principal_id"] = rng.choice([None, 7, [], {}])
        elif mutation == 2:
            record["principal_kind"] = rng.choice([None, 7, [], {}])
        elif mutation == 3:
            record["device_id"] = _invalid_device_id(rng)
        elif mutation == 4:
            record["capabilities"] = ["unknown.capability"]
        elif mutation == 5:
            record["capabilities"] = [Capability.SESSION_BASIC.value] * 2
        elif mutation == 6:
            record["active"] = rng.choice([0, 1, "true", None])
        elif mutation == 7:
            record["unknown"] = True
        else:
            record.pop(rng.choice(tuple(record)))

        clients_path.write_text(
            json.dumps({"version": 1, "clients": [record]}, ensure_ascii=False) + "\n",
            encoding="utf-8",
        )
        os.chmod(clients_path, 0o600)
        with pytest.raises(SecurityStateError):
            state.load_registry()


@pytest.mark.parametrize("seed", FUZZ_SEEDS)
def test_security_admin_request_shape_fuzz_cannot_mutate_registry(
    tmp_path: Path,
    seed: int,
):
    rng = random.Random(seed)
    state = PersistentSecurityState(tmp_path / f"security-{seed}")
    state.initialize()
    registry = state.load_registry()
    admin = SecurityAdminServer(
        state,
        capabilities={Capability.SESSION_BASIC, Capability.TURN_SUBMIT},
    )
    admin.bind_registry(registry)

    for _ in range(128):
        request = {
            "version": rng.choice([None, 0, 1, 2, "1"]),
            "action": rng.choice([None, "", "enroll", "revoke", "list", "rotate", "shell"]),
            # The deliberate unknown field guarantees that even if version and
            # action accidentally line up, this is not a valid admin request.
            "fuzz": _jsonish(rng),
        }
        if rng.getrandbits(1):
            request["public_key"] = _jsonish(rng)
        if rng.getrandbits(1):
            request["device_id"] = _jsonish(rng)

        before = state.list_clients()
        with pytest.raises(
            (SecurityAdminError, SecurityStateError, SecurityError, TypeError, ValueError)
        ):
            admin._dispatch(request)
        assert state.list_clients() == before
