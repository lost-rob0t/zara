from __future__ import annotations

import importlib.util
from pathlib import Path
import subprocess

import pytest


ROOT = Path(__file__).resolve().parents[1]
DEVICE_ACCEPTANCE = ROOT / "android" / "integration" / "device_acceptance.py"


def _load_device_acceptance_module():
    spec = importlib.util.spec_from_file_location(
        "zara_device_acceptance_uiautomator_test",
        DEVICE_ACCEPTANCE,
    )
    assert spec is not None
    assert spec.loader is not None
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)
    return module


def test_nodes_uses_fresh_shell_owned_uiautomator_dump(
    monkeypatch: pytest.MonkeyPatch,
    tmp_path: Path,
) -> None:
    module = _load_device_acceptance_module()
    device = module.Device("emulator-5554", tmp_path)
    calls: list[tuple[str, ...]] = []
    hierarchy = '<hierarchy><node text="Continue" bounds="[1,2][3,4]" /></hierarchy>'

    def fake_adb(*arguments: str, **_kwargs):
        calls.append(arguments)
        if arguments[:3] == ("shell", "uiautomator", "dump"):
            return f"UI hierchary dumped to: {module.UI_DUMP_PATH}\n"
        if arguments[:2] == ("shell", "cat"):
            return hierarchy
        return ""

    monkeypatch.setattr(device, "adb", fake_adb)

    nodes = list(device.nodes())

    assert [node.get("text") for node in nodes] == ["Continue"]
    assert calls == [
        ("shell", "rm", "-f", module.UI_DUMP_PATH),
        ("shell", "uiautomator", "dump", module.UI_DUMP_PATH),
        ("shell", "cat", module.UI_DUMP_PATH),
    ]
    assert module.UI_DUMP_PATH.startswith("/data/local/tmp/")
    assert all("/sdcard/" not in argument for call in calls for argument in call)


def test_nodes_retries_transient_missing_uiautomator_hierarchy(
    monkeypatch: pytest.MonkeyPatch,
    tmp_path: Path,
) -> None:
    module = _load_device_acceptance_module()
    device = module.Device("emulator-5554", tmp_path)
    hierarchy = '<hierarchy><node text="Chat" bounds="[1,2][3,4]" /></hierarchy>'
    cat_attempts = 0
    sleeps: list[float] = []

    def fake_adb(*arguments: str, **_kwargs):
        nonlocal cat_attempts
        if arguments[:3] == ("shell", "uiautomator", "dump"):
            return ""
        if arguments[:2] == ("shell", "cat"):
            cat_attempts += 1
            if cat_attempts == 1:
                raise subprocess.CalledProcessError(1, ["adb", *arguments])
            return hierarchy
        return ""

    monkeypatch.setattr(device, "adb", fake_adb)
    monkeypatch.setattr(module.time, "sleep", sleeps.append)

    nodes = list(device.nodes())

    assert [node.get("text") for node in nodes] == ["Chat"]
    assert cat_attempts == 2
    assert sleeps == [module.UI_DUMP_RETRY_DELAY_SECONDS]


def test_nodes_reports_successful_dump_that_created_no_hierarchy(
    monkeypatch: pytest.MonkeyPatch,
    tmp_path: Path,
) -> None:
    module = _load_device_acceptance_module()
    device = module.Device("emulator-5554", tmp_path)
    cat_attempts = 0

    def fake_adb(*arguments: str, **_kwargs):
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
        match=r"UIAutomator did not create /data/local/tmp/zara-acceptance\.xml: UI hierarchy dump reported success",
    ):
        list(device.nodes())

    assert cat_attempts == module.UI_DUMP_ATTEMPTS


def test_await_label_dismisses_release_notes_that_appear_after_launch(
    monkeypatch: pytest.MonkeyPatch,
    tmp_path: Path,
) -> None:
    module = _load_device_acceptance_module()
    device = module.Device("emulator-5554", tmp_path)
    state = {"release_notes": True, "dismissals": 0}
    clock = {"value": 0.0}

    def fake_monotonic() -> float:
        clock["value"] += 0.01
        return clock["value"]

    def fake_find(label: str):
        if label == "Chat" and not state["release_notes"]:
            return object()
        return None

    def fake_dismiss_release_notes() -> bool:
        if not state["release_notes"]:
            return False
        state["release_notes"] = False
        state["dismissals"] += 1
        return True

    monkeypatch.setattr(module.time, "monotonic", fake_monotonic)
    monkeypatch.setattr(module.time, "sleep", lambda _seconds: None)
    monkeypatch.setattr(device, "find", fake_find)
    monkeypatch.setattr(device, "dismiss_release_notes", fake_dismiss_release_notes)
    monkeypatch.setattr(device, "dismiss_pixel_launcher_anr", lambda: False)

    device.await_label("Chat", timeout=0.1)

    assert state["dismissals"] == 1


def test_reveal_contains_scrolls_to_completed_response_outside_viewport(monkeypatch, tmp_path):
    module = _load_device_acceptance_module()
    device = module.Device("emulator-5554", tmp_path)
    visible = False
    swipes = []
    monkeypatch.setattr(device, "size", lambda: (1080, 2400))
    monkeypatch.setattr(device, "find_contains", lambda _: object() if visible else None)

    def fake_adb(*args, **kwargs):
        nonlocal visible
        swipes.append(args)
        visible = True
        return ""

    monkeypatch.setattr(device, "adb", fake_adb)
    device.reveal_contains("zara_error_recovery_1523")
    assert len(swipes) == 1
    assert swipes[0][:3] == ("shell", "input", "swipe")


def test_reveal_contains_fails_closed_when_response_was_not_rendered(monkeypatch, tmp_path):
    module = _load_device_acceptance_module()
    device = module.Device("emulator-5554", tmp_path)
    swipes = []
    monkeypatch.setattr(device, "size", lambda: (1080, 2400))
    monkeypatch.setattr(device, "find_contains", lambda _: None)
    monkeypatch.setattr(device, "adb", lambda *args, **kwargs: swipes.append(args))
    with pytest.raises(AssertionError, match="zara_error_recovery_1523"):
        device.reveal_contains("zara_error_recovery_1523")
    assert len(swipes) == 12
