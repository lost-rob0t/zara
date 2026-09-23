from __future__ import annotations

from pathlib import Path

import pytest

from zara.tts import Qwen3TTSClient


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
