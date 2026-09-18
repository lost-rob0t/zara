from __future__ import annotations

import json

import pytest

from zara.runtime import discovery


def descriptor(**overrides):
    value = {
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
        "provider_control": "runtime",
        "model_control": "runtime",
        "supports_streaming": False,
        "supports_cancel": True,
        "supports_context_handles": False,
        "supports_host_tools": False,
    }
    value.update(overrides)
    return value


class FakeResponse:
    def __init__(self, payload):
        self.payload = json.dumps(payload).encode()

    def __enter__(self):
        return self

    def __exit__(self, *_args):
        return False

    def read(self, limit):
        return self.payload[:limit]


class FakeOpener:
    def __init__(self, payload):
        self.payload = payload
        self.requests = []

    def open(self, request, timeout):
        self.requests.append((request, timeout))
        return FakeResponse(self.payload)


def test_loopback_endpoint_rejects_remote_and_credentials() -> None:
    for endpoint in (
        "http://192.168.1.3:18765",
        "https://127.0.0.1:18765",
        "http://user:secret@127.0.0.1:18765",
        "http://127.0.0.1:18765?token=secret",
    ):
        with pytest.raises(discovery.RuntimeDiscoveryError):
            discovery.normalize_loopback_endpoint(endpoint)


def test_sidecar_discovery_accepts_exact_runtime_contract() -> None:
    opener = FakeOpener({"runtimes": [descriptor()]})
    client = discovery.PrologRlmSidecarClient(
        "http://127.0.0.1:18765",
        timeout=0.1,
        opener=opener,
    )

    runtimes = client.discover()

    assert [runtime.id for runtime in runtimes] == ["prolog-rlm"]
    assert runtimes[0].provider_control == "runtime"
    assert runtimes[0].model_control == "runtime"
    assert runtimes[0].supports_streaming is False
    request, timeout = opener.requests[0]
    assert request.full_url == "http://127.0.0.1:18765/zara-runtime/v1/discover"
    assert timeout == 0.1


def test_incompatible_runtime_protocol_fails_closed() -> None:
    opener = FakeOpener(
        {"runtimes": [descriptor(protocol="ZARA-RUNTIME/99")]}
    )
    client = discovery.PrologRlmSidecarClient(opener=opener)

    with pytest.raises(discovery.RuntimeDiscoveryError, match="incompatible"):
        client.discover()


def test_duplicate_runtime_identity_fails_closed() -> None:
    opener = FakeOpener({"runtimes": [descriptor(), descriptor()]})
    client = discovery.PrologRlmSidecarClient(opener=opener)

    with pytest.raises(discovery.RuntimeDiscoveryError, match="duplicate"):
        client.discover()


def test_optional_runtime_is_not_listed_when_probe_fails(monkeypatch) -> None:
    class FailingClient:
        def __init__(self, *_args, **_kwargs):
            pass

        def discover(self):
            raise discovery.RuntimeDiscoveryError("offline")

    monkeypatch.setattr(discovery, "PrologRlmSidecarClient", FailingClient)

    runtimes = discovery.discover_installed_runtimes()

    assert [runtime.id for runtime in runtimes] == ["zara-python"]


def test_optional_runtime_is_listed_only_after_compatible_live_discovery(monkeypatch) -> None:
    runtime = discovery.runtime_descriptor_from_wire(descriptor())

    class ReadyClient:
        def __init__(self, *_args, **_kwargs):
            pass

        def discover(self):
            return (runtime,)

    monkeypatch.setattr(discovery, "PrologRlmSidecarClient", ReadyClient)

    runtimes = discovery.discover_installed_runtimes()

    assert [item.id for item in runtimes] == ["zara-python", "prolog-rlm"]
