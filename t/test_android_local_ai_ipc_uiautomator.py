from __future__ import annotations

import importlib.util
from pathlib import Path
import subprocess

import pytest


ROOT = Path(__file__).resolve().parents[1]
IPC_ACCEPTANCE = ROOT / "android" / "integration" / "device_local_ai_ipc_acceptance.py"


def _load_ipc_acceptance_module():
    spec = importlib.util.spec_from_file_location(
        "zara_local_ai_ipc_uiautomator_test",
        IPC_ACCEPTANCE,
    )
    assert spec is not None
    assert spec.loader is not None
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)
    return module


def test_nodes_retries_transient_missing_uiautomator_hierarchy(
    monkeypatch: pytest.MonkeyPatch,
) -> None:
    module = _load_ipc_acceptance_module()
    device = module.Device("emulator-5554")
    hierarchy = '<hierarchy><node text="Local AI service" bounds="[1,2][3,4]" /></hierarchy>'
    cat_attempts = 0
    sleeps: list[float] = []

    def fake_adb(*arguments: str) -> str:
        nonlocal cat_attempts
        if arguments[:3] == ("shell", "uiautomator", "dump"):
            return "UI hierarchy dumped successfully"
        if arguments[:2] == ("shell", "cat"):
            cat_attempts += 1
            if cat_attempts == 1:
                raise subprocess.CalledProcessError(1, ["adb", *arguments])
            return hierarchy
        return ""

    monkeypatch.setattr(device, "adb", fake_adb)
    monkeypatch.setattr(module.time, "sleep", sleeps.append)

    nodes = list(device.nodes())

    assert [node.get("text") for node in nodes] == ["Local AI service"]
    assert cat_attempts == 2
    assert sleeps == [module.UI_DUMP_RETRY_DELAY_SECONDS]


def test_nodes_fail_closed_after_missing_hierarchy_retry_budget(
    monkeypatch: pytest.MonkeyPatch,
) -> None:
    module = _load_ipc_acceptance_module()
    device = module.Device("emulator-5554")
    cat_attempts = 0

    def fake_adb(*arguments: str) -> str:
        nonlocal cat_attempts
        if arguments[:3] == ("shell", "uiautomator", "dump"):
            return "UI hierarchy dump reported success"
        if arguments[:2] == ("shell", "cat"):
            cat_attempts += 1
            raise subprocess.CalledProcessError(1, ["adb", *arguments])
        return ""

    monkeypatch.setattr(device, "adb", fake_adb)
    monkeypatch.setattr(module.time, "sleep", lambda _seconds: None)

    with pytest.raises(
        AssertionError,
        match=r"UIAutomator did not create /data/local/tmp/zara-local-ai-ipc\.xml: UI hierarchy dump reported success",
    ):
        list(device.nodes())

    assert cat_attempts == module.UI_DUMP_ATTEMPTS
