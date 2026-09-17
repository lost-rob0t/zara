from __future__ import annotations

import sys
from types import SimpleNamespace

from zara.agent import AgentManager


def test_agent_llama_cpp_uses_openai_compatible_local_base(monkeypatch):
    captured = {}

    def fake_chat_openai(**kwargs):
        captured.update(kwargs)
        return object()

    monkeypatch.setitem(
        sys.modules,
        "langchain_openai",
        SimpleNamespace(ChatOpenAI=fake_chat_openai),
    )

    manager = object.__new__(AgentManager)
    client = manager._create_llm_client(
        {
            "provider": "llama_cpp",
            "model": None,
            "endpoint": None,
        }
    )

    assert client is not None
    assert captured["model"] == "local"
    assert captured["openai_api_base"] == "http://127.0.0.1:11435/v1"
    assert captured["api_key"] == "local"


def test_agent_llama_cpp_normalizes_chat_completions_endpoint(monkeypatch):
    captured = {}

    monkeypatch.setitem(
        sys.modules,
        "langchain_openai",
        SimpleNamespace(ChatOpenAI=lambda **kwargs: captured.update(kwargs) or object()),
    )

    manager = object.__new__(AgentManager)
    manager._create_llm_client(
        {
            "provider": "llama_cpp",
            "model": "local-model",
            "endpoint": "http://localhost:9000/v1/chat/completions",
        }
    )

    assert captured["openai_api_base"] == "http://localhost:9000/v1"
    assert captured["model"] == "local-model"
