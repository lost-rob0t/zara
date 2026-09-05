from __future__ import annotations

from pathlib import Path

import pytest

from zara.plugins import PluginCapabilityUnavailable, PluginMetadata, RuntimeStatus
from zara.plugins.manager import PluginManager, PluginState, _PluginRecord


def _manager():
    return PluginManager(
        (),
        configuration_provider=lambda _name: {},
        status_provider=lambda: RuntimeStatus("running", True, 1),
        dispatcher=lambda _command: None,
        subscriber=lambda **_kwargs: None,
        tool_registrar=lambda _tools: None,
        tool_unregistrar=lambda _names: None,
        publisher=lambda _event: None,
        capability_invoker=lambda *_args: pytest.fail(
            "transferred capability reached canonical invocation"
        ),
    )


def test_capability_handle_is_bound_to_exact_requesting_plugin():
    manager = _manager()
    provider = _PluginRecord(
        path=Path("provider.py"),
        metadata=PluginMetadata(name="provider", version="1"),
        instance=object(),
        state=PluginState.RUNNING,
        capability_tokens={"shared-tool": "generation-token"},
    )
    manager._records.append(provider)

    capability = manager._resolve_capability("requester-a", "shared-tool")

    with pytest.raises(PluginCapabilityUnavailable, match="stale"):
        manager._invoke_capability("requester-b", capability, {}, 1.0)


def test_capability_handle_remains_valid_for_original_requester():
    invoked = []
    manager = _manager()
    manager._capability_invoker = lambda requester, name, arguments, timeout: invoked.append(
        (requester, name, dict(arguments), timeout)
    ) or "sentinel"
    provider = _PluginRecord(
        path=Path("provider.py"),
        metadata=PluginMetadata(name="provider", version="1"),
        instance=object(),
        state=PluginState.RUNNING,
        capability_tokens={"shared-tool": "generation-token"},
    )
    manager._records.append(provider)

    capability = manager._resolve_capability("requester-a", "shared-tool")
    result = manager._invoke_capability("requester-a", capability, {"value": 1}, 2.0)

    assert result == "sentinel"
    assert invoked == [("requester-a", "shared-tool", {"value": 1}, 2.0)]
