from __future__ import annotations

import json
import os
import random
import string
from pathlib import Path

import pytest
from zmq.utils import z85

from zara.principals import PrincipalContext
from zara.security import Capability, KeyNotActive, SecurityError, SecurityRegistry
from zara.security_admin import SecurityAdminError, SecurityAdminServer
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
