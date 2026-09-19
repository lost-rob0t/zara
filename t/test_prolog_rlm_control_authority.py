from __future__ import annotations

import asyncio

import pytest

from zara.runtime.discovery import runtime_descriptor_from_wire
from zara.runtime.prolog_rlm import PrologRlmRuntimeBackend, PrologRlmRuntimeError


def _descriptor(*, provider_control: str = "runtime", model_control: str = "runtime"):
    return runtime_descriptor_from_wire(
        {
            "id": "prolog-rlm",
            "display_name": "Prolog-RLM",
            "protocol": "ZARA-RUNTIME/1",
            "runtime_version": "0.1.0-dev",
            "implementation_version": "0.1.0-dev",
            "installed": True,
            "available": True,
            "health": "ready",
            "locality": "local_sidecar",
            "transport": "loopback_http",
            "capabilities": ["direct", "rlm", "cancel"],
            "profiles": [],
            "provider_control": provider_control,
            "model_control": model_control,
            "supports_streaming": False,
            "supports_cancel": True,
            "supports_context_handles": False,
            "supports_host_tools": False,
        }
    )


class _FakeClient:
    def __init__(self, runtime) -> None:
        self.runtime = runtime

    def discover(self):
        return (self.runtime,)


@pytest.mark.parametrize(
    ("provider_control", "model_control"),
    [
        ("zara", "runtime"),
        ("runtime", "zara"),
        ("mixed", "runtime"),
        ("runtime", "mixed"),
    ],
)
def test_prolog_rlm_requires_runtime_owned_provider_and_model_control(
    provider_control: str,
    model_control: str,
) -> None:
    backend = PrologRlmRuntimeBackend(
        client=_FakeClient(
            _descriptor(
                provider_control=provider_control,
                model_control=model_control,
            )
        )
    )

    with pytest.raises(PrologRlmRuntimeError) as raised:
        asyncio.run(backend.start())

    assert raised.value.kind == "capability_denied"
    assert str(raised.value) == (
        "capability_denied: Prolog-RLM must own provider and model control"
    )
