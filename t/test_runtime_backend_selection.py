from __future__ import annotations

import pytest

from zara.runtime.backend import LangGraphRuntimeBackend, create_runtime_backend
from zara.runtime.prolog_rlm import PrologRlmRuntimeBackend


class FakeConfig:
    def __init__(
        self,
        runtime_backend: str = "zara-python",
        agent_backend: str = "langgraph",
    ) -> None:
        self.runtime_backend = runtime_backend
        self.agent_backend = agent_backend

    def get(self, section: str, key: str, default=None):
        if section == "runtime" and key == "backend":
            return self.runtime_backend
        if section == "agent" and key == "backend":
            return self.agent_backend
        return default

    def get_runtime_config(self):
        return {
            "backend": self.runtime_backend,
            "prolog_rlm_endpoint": "http://127.0.0.1:18765",
            "discovery_timeout": 0.1,
            "request_timeout": 1.0,
        }


def test_runtime_backend_defaults_to_zara_python_langgraph() -> None:
    backend = create_runtime_backend(FakeConfig())
    assert isinstance(backend, LangGraphRuntimeBackend)


def test_prolog_rlm_is_a_first_class_optional_runtime_backend() -> None:
    backend = create_runtime_backend(FakeConfig("prolog-rlm"))
    assert isinstance(backend, PrologRlmRuntimeBackend)


def test_legacy_prolog_rlm_spelling_normalizes_to_runtime_id() -> None:
    backend = create_runtime_backend(FakeConfig("prolog_rlm"))
    assert isinstance(backend, PrologRlmRuntimeBackend)


def test_unknown_runtime_backend_fails_closed() -> None:
    with pytest.raises(ValueError, match="Unsupported runtime backend"):
        create_runtime_backend(FakeConfig("invented-runtime"))


def test_python_agent_backend_validation_stays_inside_python_runtime() -> None:
    with pytest.raises(ValueError, match="Unsupported Python agent backend"):
        create_runtime_backend(FakeConfig("zara-python", "invented-loop"))


def test_prolog_runtime_does_not_validate_or_construct_python_agent_loop() -> None:
    backend = create_runtime_backend(FakeConfig("prolog-rlm", "invented-loop"))
    assert isinstance(backend, PrologRlmRuntimeBackend)
