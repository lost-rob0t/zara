from __future__ import annotations

import importlib.util
from pathlib import Path

import pytest


ROOT = Path(__file__).resolve().parents[1]
DEVICE_ACCEPTANCE = ROOT / "android" / "integration" / "device_acceptance.py"


def _load_module():
    spec = importlib.util.spec_from_file_location(
        "zara_device_acceptance_anr_owner_test", DEVICE_ACCEPTANCE
    )
    assert spec is not None
    assert spec.loader is not None
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)
    return module


def _snapshot(device, module, xml: str):
    root = module.ET.fromstring(xml)
    return iter(device.annotate_hierarchy(root))


def test_anr_action_from_other_dialog_owner_is_rejected(
    monkeypatch: pytest.MonkeyPatch,
    tmp_path: Path,
) -> None:
    module = _load_module()
    device = module.Device("emulator-5554", tmp_path)
    hierarchy = """
    <hierarchy>
      <node package="android" bounds="[0,0][500,500]">
        <node resource-id="dialog-a" bounds="[0,0][200,200]">
          <node text="Pixel Launcher isn't responding"
                package="com.google.android.apps.nexuslauncher"
                bounds="[20,20][180,80]" />
        </node>
        <node resource-id="dialog-b" bounds="[200,0][500,300]">
          <node text="com.google.android.googlesdksetup isn't responding"
                package="com.google.android.googlesdksetup"
                bounds="[220,20][480,80]" />
          <node text="Close app" package="android"
                resource-id="android:id/aerr_close"
                bounds="[220,100][320,160]" />
        </node>
      </node>
    </hierarchy>
    """
    adb_calls: list[tuple[str, ...]] = []

    monkeypatch.setattr(device, "nodes", lambda: _snapshot(device, module, hierarchy))
    monkeypatch.setattr(
        device,
        "adb",
        lambda *arguments, **_kwargs: adb_calls.append(arguments) or "",
    )

    with pytest.raises(AssertionError, match="did not expose a dismissal action"):
        device.dismiss_pixel_launcher_anr()

    assert adb_calls == []
    assert device.system_anr_sanitation == [
        {
            "package": "com.google.android.apps.nexuslauncher",
            "dialog": "Pixel Launcher isn't responding",
            "action": None,
            "cleared": False,
        }
    ]


def test_selected_anr_uses_only_its_structurally_bound_action(
    monkeypatch: pytest.MonkeyPatch,
    tmp_path: Path,
) -> None:
    module = _load_module()
    device = module.Device("emulator-5554", tmp_path)
    visible = {"selected": True}
    adb_calls: list[tuple[str, ...]] = []

    def nodes():
        selected = """
        <node resource-id="dialog-a" bounds="[0,0][200,200]">
          <node text="Pixel Launcher isn't responding"
                package="com.google.android.apps.nexuslauncher"
                bounds="[20,20][180,80]" />
          <node text="Close app" package="android"
                resource-id="android:id/aerr_close"
                bounds="[20,100][120,160]" />
        </node>
        """ if visible["selected"] else ""
        hierarchy = f"""
        <hierarchy>
          <node package="android" bounds="[0,0][500,500]">
            {selected}
            <node resource-id="dialog-b" bounds="[200,0][500,300]">
              <node text="com.google.android.googlesdksetup isn't responding"
                    package="com.google.android.googlesdksetup"
                    bounds="[220,20][480,80]" />
              <node text="Close app" package="android"
                    resource-id="android:id/aerr_close"
                    bounds="[320,100][420,160]" />
            </node>
          </node>
        </hierarchy>
        """
        return _snapshot(device, module, hierarchy)

    def adb(*arguments: str, **_kwargs):
        adb_calls.append(arguments)
        if arguments == ("shell", "input", "tap", "70", "130"):
            visible["selected"] = False
        return ""

    monkeypatch.setattr(device, "nodes", nodes)
    monkeypatch.setattr(device, "adb", adb)
    monkeypatch.setattr(module.time, "sleep", lambda _seconds: None)

    assert device.dismiss_pixel_launcher_anr() is True
    assert adb_calls == [("shell", "input", "tap", "70", "130")]
    assert device.system_anr_sanitation == [
        {
            "package": "com.google.android.apps.nexuslauncher",
            "dialog": "Pixel Launcher isn't responding",
            "action": "Close app",
            "cleared": True,
        }
    ]


def test_single_anr_does_not_claim_unrelated_system_action(
    monkeypatch: pytest.MonkeyPatch,
    tmp_path: Path,
) -> None:
    module = _load_module()
    device = module.Device("emulator-5554", tmp_path)
    hierarchy = """
    <hierarchy>
      <node package="android" bounds="[0,0][500,500]">
        <node resource-id="dialog-a" bounds="[0,0][220,220]">
          <node text="Pixel Launcher isn't responding"
                package="com.google.android.apps.nexuslauncher"
                bounds="[20,20][200,80]" />
        </node>
        <node resource-id="background-controls" bounds="[250,0][500,220]">
          <node text="Close app" package="android"
                resource-id="android:id/aerr_close"
                bounds="[280,100][400,160]" />
        </node>
      </node>
    </hierarchy>
    """
    adb_calls: list[tuple[str, ...]] = []

    monkeypatch.setattr(device, "nodes", lambda: _snapshot(device, module, hierarchy))
    monkeypatch.setattr(
        device,
        "adb",
        lambda *arguments, **_kwargs: adb_calls.append(arguments) or "",
    )

    with pytest.raises(AssertionError, match="did not expose a dismissal action"):
        device.dismiss_pixel_launcher_anr()

    assert adb_calls == []
    assert device.system_anr_sanitation == [
        {
            "package": "com.google.android.apps.nexuslauncher",
            "dialog": "Pixel Launcher isn't responding",
            "action": None,
            "cleared": False,
        }
    ]


@pytest.mark.parametrize("unexpected_first", [False, True])
def test_mixed_anr_snapshot_fails_closed_before_any_system_tap(
    monkeypatch: pytest.MonkeyPatch,
    tmp_path: Path,
    unexpected_first: bool,
) -> None:
    module = _load_module()
    device = module.Device("emulator-5554", tmp_path)
    pixel_dialog = """
      <node resource-id="pixel-anr" bounds="[0,0][240,220]">
        <node text="Pixel Launcher isn't responding"
              package="com.google.android.apps.nexuslauncher"
              bounds="[20,20][220,80]" />
        <node text="Close app" package="android"
              resource-id="android:id/aerr_close"
              bounds="[20,100][120,160]" />
      </node>
    """
    unexpected_dialog = """
      <node resource-id="zara-anr" bounds="[250,0][500,220]">
        <node text="ai.zara.app isn't responding"
              package="ai.zara.app"
              bounds="[270,20][480,80]" />
      </node>
    """
    dialogs = (
        unexpected_dialog + pixel_dialog
        if unexpected_first
        else pixel_dialog + unexpected_dialog
    )
    hierarchy = f"""
    <hierarchy>
      <node package="android" bounds="[0,0][500,500]">
        {dialogs}
      </node>
    </hierarchy>
    """
    adb_calls: list[tuple[str, ...]] = []

    monkeypatch.setattr(device, "nodes", lambda: _snapshot(device, module, hierarchy))
    monkeypatch.setattr(
        device,
        "adb",
        lambda *arguments, **_kwargs: adb_calls.append(arguments) or "",
    )
    monkeypatch.setattr(module.time, "sleep", lambda _seconds: None)

    with pytest.raises(AssertionError, match="Unexpected ANR dialog blocks acceptance"):
        device.dismiss_pixel_launcher_anr()

    assert adb_calls == []
    assert device.system_anr_sanitation == [
        {
            "package": "ai.zara.app",
            "dialog": "ai.zara.app isn't responding",
            "action": None,
            "cleared": False,
        }
    ]
