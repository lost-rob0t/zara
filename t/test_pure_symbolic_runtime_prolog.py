from __future__ import annotations

import pytest

from zara.runtime.pure_symbolic_backend import PureSymbolicRuntimeBackend


@pytest.mark.asyncio
async def test_real_prolog_symbolic_backend_greeting_and_parse_miss_stay_zero_call() -> None:
    backend = PureSymbolicRuntimeBackend()
    await backend.start()
    try:
        greeting = await backend.submit_turn(
            "hello",
            turn_id="symbolic-real-1",
            conversation_id="conversation-real",
        )
        unsupported = await backend.submit_turn(
            "quux flarble snorple",
            turn_id="symbolic-real-2",
            conversation_id="conversation-real",
        )
    finally:
        await backend.stop()

    assert greeting.response == "Hey — what can I help with?"
    assert greeting.metadata["response_act"] == "greeting"
    assert unsupported.response == "I don’t know how to handle that symbolically yet."
    assert unsupported.metadata["response_act"] == "unsupported"

    for result in (greeting, unsupported):
        assert result.metadata["providers_enabled"] is False
        assert result.metadata["max_provider_calls"] == 0
        assert result.metadata["max_model_calls"] == 0
        assert result.metadata["provider_calls"] == 0
        assert result.metadata["model_calls"] == 0
        assert result.metadata["renderer"] == "symbolic-dcg/v1"
