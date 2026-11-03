from __future__ import annotations

import importlib.util
from pathlib import Path

import pytest


ROOT = Path(__file__).resolve().parents[1]
DEVICE_ACCEPTANCE = ROOT / "android" / "integration" / "device_acceptance.py"


def _load_module():
    spec = importlib.util.spec_from_file_location(
        "zara_device_acceptance_anr_owner_boundary_test", DEVICE_ACCEPTANCE
    )
    assert spec is not None
    assert spec.loader is not None
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)
    return module


def _assert_unprovable_owner(
    monkeypatch: pytest.MonkeyPatch,
    tmp_path: Path,
    hierarchy: str,
) -> None:
    module = _load_module()
    device = module.Device("emulator-5554", tmp_path)
    root = module.ET.fromstring(hierarchy)
    nodes = device.annotate_hierarchy(root)
    adb_calls: list[tuple[str, ...]] = []

    monkeypatch.setattr(device, "nodes", lambda: iter(nodes))
    monkeypatch.setattr(
        device,
        "adb",
        lambda *arguments, **_kwargs: adb_calls.append(arguments) or "",
    )
    monkeypatch.setattr(module.time, "sleep", lambda _seconds: None)

    with pytest.raises(
        AssertionError,
        match="Known system ANR did not expose a dismissal action",
    ):
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


def test_anr_direct_child_of_hierarchy_cannot_claim_root_sibling_action(
    monkeypatch: pytest.MonkeyPatch,
    tmp_path: Path,
) -> None:
    _assert_unprovable_owner(
        monkeypatch,
        tmp_path,
        """
        <hierarchy>
          <node text="Pixel Launcher isn't responding"
                package="com.google.android.apps.nexuslauncher"
                bounds="[10,10][190,90]" />
          <node text="Close app"
                package="android"
                resource-id="android:id/aerr_close"
                bounds="[20,100][180,160]" />
        </hierarchy>
        """,
    )


def test_anr_and_action_under_generic_root_are_not_a_proven_dialog_owner(
    monkeypatch: pytest.MonkeyPatch,
    tmp_path: Path,
) -> None:
    _assert_unprovable_owner(
        monkeypatch,
        tmp_path,
        """
        <hierarchy>
          <node package="android" class="android.widget.FrameLayout"
                bounds="[0,0][1080,2400]">
            <node text="Pixel Launcher isn't responding"
                  package="com.google.android.apps.nexuslauncher"
                  bounds="[10,10][190,90]" />
            <node text="Close app"
                  package="android"
                  resource-id="android:id/aerr_close"
                  bounds="[20,100][180,160]" />
          </node>
        </hierarchy>
        """,
    )

def test_nested_generic_containers_are_not_a_dialog_owner_boundary(
    monkeypatch: pytest.MonkeyPatch,
    tmp_path: Path,
) -> None:
    _assert_unprovable_owner(
        monkeypatch,
        tmp_path,
        """
        <hierarchy>
          <node package="android" class="android.widget.FrameLayout"
                bounds="[0,0][1080,2400]">
            <node package="android" class="android.widget.FrameLayout"
                  bounds="[0,0][1080,2400]">
              <node text="Pixel Launcher isn't responding"
                    package="com.google.android.apps.nexuslauncher"
                    bounds="[10,10][190,90]" />
              <node text="Close app"
                    package="android"
                    resource-id="android:id/aerr_close"
                    bounds="[20,100][180,160]" />
            </node>
          </node>
        </hierarchy>
        """,
    )
