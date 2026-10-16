from __future__ import annotations

import pytest

from zara.runtime.backend import LangGraphRuntimeBackend, create_runtime_backend
from zara.runtime.prolog_rlm import PrologRLMRuntimeBackend


class FakeConfig:
    def __init__(self, backend: str = "langgraph") -> None:
        self.backend = backend

    def get(self, section: str, key: str, default=None):
        if section == "agent" and key == "backend":
            return self.backend
        return default


def test_runtime_backend_defaults_to_langgraph() -> None:
    backend = create_runtime_backend(FakeConfig())
    assert isinstance(backend, LangGraphRuntimeBackend)


def test_prolog_rlm_backend_selects_real_sidecar() -> None:
    assert isinstance(create_runtime_backend(FakeConfig("prolog_rlm")), PrologRLMRuntimeBackend)


def test_unknown_backend_fails_closed() -> None:
    with pytest.raises(ValueError, match="Unsupported agent backend"):
        create_runtime_backend(FakeConfig("unknown"))
