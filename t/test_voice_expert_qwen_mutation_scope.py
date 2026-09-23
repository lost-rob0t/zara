from __future__ import annotations

import pytest

from zara.voice_expert import VoiceExpert


class FakeProlog:
    def query_once(self, goal: str):
        raise AssertionError(f"unexpected Prolog query: {goal}")


def expert_for(endpoint: str) -> VoiceExpert:
    return VoiceExpert(
        FakeProlog(),
        {"tts": {"provider": "qwen3", "endpoint": endpoint}},
    )


@pytest.mark.parametrize(
    "endpoint",
    [
        "https://qwen.example.com:7860",
        "http://192.0.2.44:7860",
        "http://10.20.30.40:7860",
        "http://[2001:db8::44]:7860",
    ],
)
def test_remote_qwen_clone_rejected_before_provider_or_media_effects(
    endpoint: str,
    monkeypatch: pytest.MonkeyPatch,
):
    expert = expert_for(endpoint)

    async def forbidden_provider_read():
        raise AssertionError("remote provider inventory must not be read")

    monkeypatch.setattr(expert, "_qwen_list_voices", forbidden_provider_read)
    monkeypatch.setattr(
        expert,
        "_download_youtube_audio",
        lambda *args, **kwargs: (_ for _ in ()).throw(
            AssertionError("media download must not begin")
        ),
    )

    with pytest.raises(RuntimeError, match="loopback"):
        expert.clone_from_youtube(
            "https://www.youtube.com/watch?v=fixture",
            "authorized_voice",
            rights_basis="self",
            attest_not_public_figure=True,
        )


@pytest.mark.parametrize(
    "endpoint",
    [
        "https://qwen.example.com:7860",
        "http://192.0.2.44:7860",
        "http://10.20.30.40:7860",
        "http://[2001:db8::44]:7860",
    ],
)
def test_remote_qwen_delete_rejected_before_provider_effect(
    endpoint: str,
    monkeypatch: pytest.MonkeyPatch,
):
    expert = expert_for(endpoint)

    async def forbidden_provider_read():
        raise AssertionError("remote provider inventory must not be read")

    monkeypatch.setattr(expert, "_qwen_list_voices", forbidden_provider_read)

    with pytest.raises(RuntimeError, match="loopback"):
        expert.delete_voice("authorized_voice")


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
def test_qwen_voice_mutation_scope_accepts_only_loopback_endpoints(endpoint: str):
    expert_for(endpoint)._require_local_qwen_mutation_endpoint()
