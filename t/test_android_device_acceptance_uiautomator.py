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


def test_nodes_reports_successful_dump_that_created_no_hierarchy(
    monkeypatch: pytest.MonkeyPatch,
    tmp_path: Path,
) -> None:
    module = _load_device_acceptance_module()
    device = module.Device("emulator-5554", tmp_path)

    def fake_adb(*arguments: str, **_kwargs):
        if arguments[:3] == ("shell", "uiautomator", "dump"):
            return "UI hierarchy dump reported success"
        if arguments[:2] == ("shell", "cat"):
            raise subprocess.CalledProcessError(1, ["adb", *arguments])
        return ""

    monkeypatch.setattr(device, "adb", fake_adb)

    with pytest.raises(
        AssertionError,
        match=r"UIAutomator did not create /data/local/tmp/zara-acceptance\.xml: UI hierarchy dump reported success",
    ):
        list(device.nodes())
