from __future__ import annotations

import asyncio
from pathlib import Path

import pytest

from zara.tts import Qwen3TTSClient
from zara.tts.qwen import Qwen3TTSClient as BaseQwen3TTSClient


class ForbiddenSession:
    closed = False

    def get(self, *args, **kwargs):
        raise AssertionError("remote provider inventory must not be read")

    def post(self, *args, **kwargs):
        raise AssertionError("remote provider mutation must not begin")

    def delete(self, *args, **kwargs):
        raise AssertionError("remote provider mutation must not begin")

    async def close(self):
        self.closed = True


def remote_client(endpoint: str) -> Qwen3TTSClient:
    client = Qwen3TTSClient(endpoint)
    client.session = ForbiddenSession()
    return client


@pytest.mark.asyncio
@pytest.mark.parametrize(
    "endpoint",
    [
        "https://qwen.example.com:7860",
        "http://192.0.2.44:7860",
        "http://10.20.30.40:7860",
        "http://[2001:db8::44]:7860",
    ],
)
async def test_remote_qwen_registration_is_rejected_before_provider_effect(
    endpoint: str,
    tmp_path: Path,
):
    audio = tmp_path / "authorized.wav"
    audio.write_bytes(b"RIFFxxxxWAVEfmt data")
    client = remote_client(endpoint)

    with pytest.raises(RuntimeError, match="loopback"):
        await client.register_voice("authorized_voice", str(audio), "fixture")


@pytest.mark.asyncio
@pytest.mark.parametrize(
    "endpoint",
    [
        "https://qwen.example.com:7860",
        "http://192.0.2.44:7860",
        "http://10.20.30.40:7860",
        "http://[2001:db8::44]:7860",
    ],
)
async def test_remote_qwen_delete_is_rejected_before_provider_effect(endpoint: str):
    client = remote_client(endpoint)

    with pytest.raises(RuntimeError, match="loopback"):
        await client.delete_voice("authorized_voice")


@pytest.mark.asyncio
async def test_direct_qwen_client_cannot_bypass_remote_mutation_scope(tmp_path: Path):
    audio = tmp_path / "authorized.wav"
    audio.write_bytes(b"RIFFxxxxWAVEfmt data")
    client = BaseQwen3TTSClient("http://192.0.2.44:7860")
    client.session = ForbiddenSession()

    with pytest.raises(RuntimeError, match="loopback"):
        await client.register_voice("authorized_voice", str(audio), "fixture")


@pytest.mark.parametrize(
    "endpoint",
    [
        "http://localhost:7860",
        "https://LOCALHOST.:7860",
        "http://127.0.0.1:7860",
        "http://127.9.8.7:7860",
        "http://[::1]:7860",
    ],
)
def test_qwen_voice_mutation_scope_accepts_loopback_endpoints(endpoint: str):
    Qwen3TTSClient(endpoint)._require_local_voice_mutation_endpoint()


def test_equivalent_loopback_aliases_share_voice_mutation_lock_identity():
    localhost = Qwen3TTSClient("http://localhost:7860")
    ipv4 = Qwen3TTSClient("http://127.0.0.1:7860")
    ipv6 = Qwen3TTSClient("http://[::1]:7860")

    assert localhost._voice_mutation_lock_path("same_voice") == ipv4._voice_mutation_lock_path("same_voice")
    assert localhost._voice_mutation_lock_path("same_voice") == ipv6._voice_mutation_lock_path("same_voice")


class SharedRegistryClient(Qwen3TTSClient):
    def __init__(self, endpoint: str, registry: set[str]):
        super().__init__(endpoint)
        self.registry = registry
        self.mutations = 0

    async def list_voices(self) -> list[str]:
        return sorted(self.registry)

    async def _register_voice_unlocked(self, body: dict) -> dict:
        self.mutations += 1
        await asyncio.sleep(0.05)
        self.registry.add(str(body["name"]))
        return {"ok": True, "name": body["name"]}


@pytest.mark.asyncio
async def test_loopback_aliases_serialize_one_shared_registry_mutation(tmp_path: Path):
    audio = tmp_path / "authorized.wav"
    audio.write_bytes(b"RIFFxxxxWAVEfmt data")
    registry: set[str] = set()
    localhost = SharedRegistryClient("http://localhost:7860", registry)
    ipv4 = SharedRegistryClient("http://127.0.0.1:7860", registry)

    results = await asyncio.gather(
        localhost.register_voice("same_voice", str(audio), "fixture"),
        ipv4.register_voice("same_voice", str(audio), "fixture"),
        return_exceptions=True,
    )

    successes = [result for result in results if isinstance(result, dict)]
    failures = [result for result in results if isinstance(result, BaseException)]
    assert len(successes) == 1
    assert len(failures) == 1
    assert isinstance(failures[0], RuntimeError)
    assert "already exists" in str(failures[0])
    assert localhost.mutations + ipv4.mutations == 1
    assert registry == {"same_voice"}
