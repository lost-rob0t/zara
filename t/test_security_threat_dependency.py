from __future__ import annotations

import pytest

import zara.security_transport as security_transport


def test_secure_runtime_rejects_libzmq_build_without_curve_support(monkeypatch):
    monkeypatch.setattr(security_transport.zmq, "zmq_version_info", lambda: (4, 3, 5))
    monkeypatch.setattr(security_transport.zmq, "has", lambda capability: False if capability == "curve" else True)
    with pytest.raises(RuntimeError, match="CURVE support"):
        security_transport.require_secure_curve_runtime()
