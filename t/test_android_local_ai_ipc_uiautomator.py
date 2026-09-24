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


def test_tap_recovers_once_from_unrelated_pixel_launcher_anr(
    monkeypatch: pytest.MonkeyPatch,
) -> None:
    module = _load_ipc_acceptance_module()
    device = module.Device("emulator-5554")
    launcher_dialog = list(
        module.ET.fromstring(
            '<hierarchy>'
            '<node text="Pixel Launcher isn\'t responding" resource-id="android:id/alertTitle" '
            'package="android" bounds="[100,100][900,200]" />'
            '<node text="Wait" resource-id="android:id/aerr_wait" package="android" '
            'clickable="true" enabled="true" bounds="[70,1296][1010,1422]" />'
            '</hierarchy>'
        ).iter("node")
    )
    app_hierarchy = list(
        module.ET.fromstring(
            '<hierarchy><node text="Start server" package="ai.zara.llmserve.adversary" '
            'clickable="true" enabled="true" bounds="[200,240][600,360]" /></hierarchy>'
        ).iter("node")
    )
    snapshots = iter((launcher_dialog, app_hierarchy))
    taps: list[tuple[str, ...]] = []
    sleeps: list[float] = []

    monkeypatch.setattr(device, "nodes", lambda: iter(next(snapshots)))

    def fake_adb(*arguments: str) -> str:
        if arguments[:3] == ("shell", "input", "tap"):
            taps.append(arguments)
        return ""

    monkeypatch.setattr(device, "adb", fake_adb)
    monkeypatch.setattr(module.time, "sleep", sleeps.append)

    device.tap("Start server")

    assert taps == [
        ("shell", "input", "tap", "540", "1359"),
        ("shell", "input", "tap", "400", "300"),
    ]
    assert sleeps == [module.SYSTEM_DIALOG_RETRY_DELAY_SECONDS]


def test_tap_does_not_hide_app_under_test_anr(
    monkeypatch: pytest.MonkeyPatch,
) -> None:
    module = _load_ipc_acceptance_module()
    device = module.Device("emulator-5554")
    app_anr = list(
        module.ET.fromstring(
            '<hierarchy>'
            '<node text="LLM Serve isn\'t responding" resource-id="android:id/alertTitle" '
            'package="android" bounds="[100,100][900,200]" />'
            '<node text="Wait" resource-id="android:id/aerr_wait" package="android" '
            'clickable="true" enabled="true" bounds="[70,1296][1010,1422]" />'
            '</hierarchy>'
        ).iter("node")
    )
    taps: list[tuple[str, ...]] = []

    monkeypatch.setattr(device, "nodes", lambda: iter(app_anr))

    def fake_adb(*arguments: str) -> str:
        if arguments[:3] == ("shell", "input", "tap"):
            taps.append(arguments)
        return ""

    monkeypatch.setattr(device, "adb", fake_adb)

    with pytest.raises(AssertionError, match="Missing control: Start server"):
        device.tap("Start server")

    assert taps == []
