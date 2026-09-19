"""Fail-closed redirect authority for optional runtime discovery."""

from __future__ import annotations

import urllib.request

import pytest

from zara.runtime.discovery import (
    PrologRlmSidecarClient,
    RuntimeDiscoveryError,
    _RejectRedirectHandler,
)


def test_default_sidecar_transport_rejects_redirects() -> None:
    client = PrologRlmSidecarClient("http://127.0.0.1:18765")
    handler = next(
        (
            item
            for item in client._opener.handlers
            if isinstance(item, _RejectRedirectHandler)
        ),
        None,
    )

    assert handler is not None

    request = urllib.request.Request(
        "http://127.0.0.1:18765/zara-runtime/v1/discover"
    )
    with pytest.raises(RuntimeDiscoveryError, match="redirect"):
        handler.redirect_request(
            request,
            None,
            302,
            "Found",
            {},
            "http://192.0.2.10/runtime",
        )


def test_explicit_custom_opener_remains_injectable_for_deterministic_tests() -> None:
    class FakeOpener:
        pass

    opener = FakeOpener()
    client = PrologRlmSidecarClient(
        "http://127.0.0.1:18765",
        opener=opener,
    )

    assert client._opener is opener
