from __future__ import annotations

import pytest

from zara.runtime.backend import LangGraphRuntimeBackend, create_runtime_backend
from zara.runtime.prolog_rlm_backend import PrologRLMRuntimeBackend


class FakeConfig:
    def __init__(self, backend: str = "langgraph") -> None:
        self.backend = backend

    def get(self, section: str, key: str, default=None):
        if section == "agent" and key == "backend":
            return self.backend
        return default

    def get_section(self, section: str):
        if section == "prolog_rlm":
            return {}
        return {}


def test_runtime_backend_defaults_to_langgraph() -> None:
    backend = create_runtime_backend(FakeConfig())
    assert isinstance(backend, LangGraphRuntimeBackend)


def test_prolog_rlm_backend_is_explicit_opt_in() -> None:
    backend = create_runtime_backend(FakeConfig("prolog_rlm"))
    assert isinstance(backend, PrologRLMRuntimeBackend)


def test_unknown_runtime_backend_fails_closed() -> None:
    with pytest.raises(ValueError, match="Unsupported agent backend"):
        create_runtime_backend(FakeConfig("unknown"))
