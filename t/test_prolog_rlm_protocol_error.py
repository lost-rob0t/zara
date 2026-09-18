from __future__ import annotations

import pytest

from zara.runtime.discovery import PROLOG_RLM_RUNTIME_ID
from zara.runtime.prolog_rlm import PrologRlmRuntimeBackend, PrologRlmRuntimeError
from zara.runtime.registry import (
    ControlOwner,
    RuntimeDescriptor,
    RuntimeHealth,
    RuntimeLocality,
    RuntimeTransport,
)


class IncompatibleClient:
    def discover(self):
        return (
            RuntimeDescriptor(
                id=PROLOG_RLM_RUNTIME_ID,
                display_name="Prolog-RLM",
                protocol="ZARA-RUNTIME/99",
                runtime_version="test",
                implementation_version="test",
                installed=True,
                available=True,
                health=RuntimeHealth.READY,
                locality=RuntimeLocality.LOCAL_SIDECAR,
                transport=RuntimeTransport.LOOPBACK_HTTP,
                capabilities=("rlm",),
                profiles=(),
                provider_control=ControlOwner.RUNTIME,
                model_control=ControlOwner.RUNTIME,
                supports_streaming=False,
                supports_cancel=True,
                supports_context_handles=False,
                supports_host_tools=False,
                provenance="prolog-rlm:test",
            ),
        )


@pytest.mark.asyncio
async def test_incompatible_discovered_runtime_reports_typed_protocol_error():
    backend = PrologRlmRuntimeBackend(client=IncompatibleClient())

    with pytest.raises(PrologRlmRuntimeError) as captured:
        await backend.start()

    assert captured.value.kind == "incompatible_protocol"
    assert str(captured.value) == (
        "incompatible_protocol: Prolog-RLM runtime protocol is incompatible"
    )
