from __future__ import annotations

import importlib.util
from pathlib import Path

import pytest


ROOT = Path(__file__).resolve().parents[1]
DEVICE_ACCEPTANCE = ROOT / "android" / "integration" / "device_acceptance.py"


def _load_module():
    spec = importlib.util.spec_from_file_location(
        "zara_device_acceptance_pixel_anr_test", DEVICE_ACCEPTANCE
    )
    assert spec is not None
    assert spec.loader is not None
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)
    return module


def test_pixel_launcher_anr_closes_hung_launcher_instead_of_waiting(
    monkeypatch: pytest.MonkeyPatch,
    tmp_path: Path,
) -> None:
    module = _load_module()
    device = module.Device("emulator-5554", tmp_path)
    launcher_anr = module.ET.fromstring(
        '<node text="Pixel Launcher isn\'t responding" '
        'package="com.google.android.apps.nexuslauncher" bounds="[10,10][90,90]" />'
    )
    close_app = module.ET.fromstring(
        '<node text="Close app" package="android" bounds="[20,30][80,70]" />'
    )
    wait = module.ET.fromstring(
        '<node text="Wait" package="android" bounds="[20,80][80,120]" />'
    )
    visible = {"dialog": True}
    adb_calls: list[tuple[str, ...]] = []

    def find_contains(fragment: str):
        if fragment == "Pixel Launcher isn't responding" and visible["dialog"]:
            return launcher_anr
        if fragment == "isn't responding" and visible["dialog"]:
            return launcher_anr
        return None

    def adb(*arguments: str, **_kwargs):
        adb_calls.append(arguments)
        if arguments[:4] == ("shell", "input", "tap", "50"):
            visible["dialog"] = False
        return ""

    monkeypatch.setattr(device, "find_contains", find_contains)
    monkeypatch.setattr(
        device,
        "find",
        lambda label: close_app if label == "Close app" else wait if label == "Wait" else None,
    )
    monkeypatch.setattr(device, "adb", adb)
    monkeypatch.setattr(module.time, "sleep", lambda _seconds: None)

    assert device.dismiss_pixel_launcher_anr() is True
    assert adb_calls == [("shell", "input", "tap", "50", "50")]
    assert device.system_anr_sanitation == [
        {
            "package": "com.google.android.apps.nexuslauncher",
            "dialog": "Pixel Launcher isn't responding",
            "action": "Close app",
            "cleared": True,
        }
    ]


def test_google_sdk_setup_anr_is_closed_and_recorded(
    monkeypatch: pytest.MonkeyPatch,
    tmp_path: Path,
) -> None:
    module = _load_module()
    device = module.Device("emulator-5554", tmp_path)
    dialog_text = "com.google.android.googlesdksetup isn't responding"
    setup_anr = module.ET.fromstring(
        f'<node text="{dialog_text}" package="com.google.android.googlesdksetup" '
        'bounds="[10,10][90,90]" />'
    )
    close_app = module.ET.fromstring(
        '<node text="Close app" package="android" bounds="[20,30][80,70]" />'
    )
    visible = {"dialog": True}
    adb_calls: list[tuple[str, ...]] = []

    def find_contains(fragment: str):
        if fragment in (dialog_text, "isn't responding") and visible["dialog"]:
            return setup_anr
        return None

    def adb(*arguments: str, **_kwargs):
        adb_calls.append(arguments)
        if arguments[:3] == ("shell", "input", "tap"):
            visible["dialog"] = False
        return ""

    monkeypatch.setattr(device, "find_contains", find_contains)
    monkeypatch.setattr(
        device,
        "find",
        lambda label: close_app if label == "Close app" else None,
    )
    monkeypatch.setattr(device, "adb", adb)
    monkeypatch.setattr(module.time, "sleep", lambda _seconds: None)

    assert device.dismiss_pixel_launcher_anr() is True
    assert adb_calls == [("shell", "input", "tap", "50", "50")]
    assert device.system_anr_sanitation == [
        {
            "package": "com.google.android.googlesdksetup",
            "dialog": dialog_text,
            "action": "Close app",
            "cleared": True,
        }
    ]


def test_known_anr_text_from_wrong_package_fails_closed(
    monkeypatch: pytest.MonkeyPatch,
    tmp_path: Path,
) -> None:
    module = _load_module()
    device = module.Device("emulator-5554", tmp_path)
    dialog_text = "com.google.android.googlesdksetup isn't responding"
    spoofed_anr = module.ET.fromstring(
        f'<node text="{dialog_text}" package="ai.zara.app" bounds="[10,10][90,90]" />'
    )
    close_app = module.ET.fromstring(
        '<node text="Close app" package="android" bounds="[20,30][80,70]" />'
    )
    adb_calls: list[tuple[str, ...]] = []

    monkeypatch.setattr(
        device,
        "find_contains",
        lambda fragment: spoofed_anr
        if fragment in (dialog_text, "isn't responding")
        else None,
    )
    monkeypatch.setattr(
        device,
        "find",
        lambda label: close_app if label == "Close app" else None,
    )
    monkeypatch.setattr(
        device,
        "adb",
        lambda *arguments, **_kwargs: adb_calls.append(arguments) or "",
    )

    with pytest.raises(AssertionError, match="Unexpected ANR dialog blocks acceptance"):
        device.dismiss_pixel_launcher_anr()

    assert adb_calls == []
    assert device.system_anr_sanitation == [
        {
            "package": "ai.zara.app",
            "dialog": dialog_text,
            "action": None,
            "cleared": False,
        }
    ]


def test_known_anr_tap_must_prove_dialog_disappeared(
    monkeypatch: pytest.MonkeyPatch,
    tmp_path: Path,
) -> None:
    module = _load_module()
    device = module.Device("emulator-5554", tmp_path)
    dialog_text = "Pixel Launcher isn't responding"
    launcher_anr = module.ET.fromstring(
        f'<node text="{dialog_text}" package="com.google.android.apps.nexuslauncher" '
        'bounds="[10,10][90,90]" />'
    )
    close_app = module.ET.fromstring(
        '<node text="Close app" package="android" bounds="[20,30][80,70]" />'
    )
    adb_calls: list[tuple[str, ...]] = []

    monkeypatch.setattr(
        device,
        "find_contains",
        lambda fragment: launcher_anr
        if fragment in (dialog_text, "isn't responding")
        else None,
    )
    monkeypatch.setattr(
        device,
        "find",
        lambda label: close_app if label == "Close app" else None,
    )
    monkeypatch.setattr(
        device,
        "adb",
        lambda *arguments, **_kwargs: adb_calls.append(arguments) or "",
    )
    monkeypatch.setattr(module.time, "sleep", lambda _seconds: None)

    with pytest.raises(AssertionError, match="sanitation limit exceeded"):
        device.dismiss_pixel_launcher_anr()

    assert adb_calls == [
        ("shell", "input", "tap", "50", "50"),
        ("shell", "input", "tap", "50", "50"),
    ]
    assert device.system_anr_sanitation == [
        {
            "package": "com.google.android.apps.nexuslauncher",
            "dialog": dialog_text,
            "action": "Close app",
            "cleared": False,
        },
        {
            "package": "com.google.android.apps.nexuslauncher",
            "dialog": dialog_text,
            "action": "Close app",
            "cleared": False,
        },
    ]


def test_unknown_anr_fails_closed_instead_of_accepting_background_zara(
    monkeypatch: pytest.MonkeyPatch,
    tmp_path: Path,
) -> None:
    module = _load_module()
    device = module.Device("emulator-5554", tmp_path)
    unknown_text = "ai.zara.app isn't responding"
    unknown_anr = module.ET.fromstring(
        f'<node text="{unknown_text}" package="ai.zara.app" bounds="[10,10][90,90]" />'
    )
    adb_calls: list[tuple[str, ...]] = []

    monkeypatch.setattr(
        device,
        "find_contains",
        lambda fragment: unknown_anr if fragment == "isn't responding" else None,
    )
    monkeypatch.setattr(device, "find", lambda _label: None)
    monkeypatch.setattr(
        device,
        "adb",
        lambda *arguments, **_kwargs: adb_calls.append(arguments) or "",
    )

    with pytest.raises(AssertionError, match="Unexpected ANR dialog blocks acceptance"):
        device.dismiss_pixel_launcher_anr()

    assert adb_calls == []
    assert device.system_anr_sanitation == [
        {
            "package": "ai.zara.app",
            "dialog": unknown_text,
            "action": None,
            "cleared": False,
        }
    ]


def test_non_launcher_anr_is_never_dismissed(
    monkeypatch: pytest.MonkeyPatch,
    tmp_path: Path,
) -> None:
    module = _load_module()
    device = module.Device("emulator-5554", tmp_path)
    adb_calls: list[tuple[str, ...]] = []

    monkeypatch.setattr(device, "find_contains", lambda _fragment: None)
    monkeypatch.setattr(device, "find", lambda _label: None)
    monkeypatch.setattr(
        device,
        "adb",
        lambda *arguments, **_kwargs: adb_calls.append(arguments) or "",
    )

    assert device.dismiss_pixel_launcher_anr() is False
    assert adb_calls == []


def test_exact_anr_presence_searches_past_wrong_package_duplicate(
    monkeypatch: pytest.MonkeyPatch,
    tmp_path: Path,
) -> None:
    module = _load_module()
    device = module.Device("emulator-5554", tmp_path)
    dialog_text = "Pixel Launcher isn't responding"
    wrong_package = module.ET.fromstring(
        f'<node text="{dialog_text}" package="ai.zara.app" bounds="[10,10][90,90]" />'
    )
    real_dialog = module.ET.fromstring(
        f'<node text="{dialog_text}" package="com.google.android.apps.nexuslauncher" '
        'bounds="[100,10][190,90]" />'
    )

    monkeypatch.setattr(device, "find_contains", lambda _fragment: wrong_package)
    monkeypatch.setattr(device, "nodes", lambda: iter((wrong_package, real_dialog)))

    assert device.exact_anr_is_present(
        "com.google.android.apps.nexuslauncher",
        dialog_text,
    ) is True


@pytest.mark.parametrize(
    ("label", "resource_id"),
    (
        ("Close app", "android:id/aerr_close"),
        ("Wait", "android:id/aerr_wait"),
    ),
)
def test_anr_action_lookup_skips_wrong_package_before_system_action(
    monkeypatch: pytest.MonkeyPatch,
    tmp_path: Path,
    label: str,
    resource_id: str,
) -> None:
    module = _load_module()
    device = module.Device("emulator-5554", tmp_path)
    wrong_action = module.ET.fromstring(
        f'<node text="{label}" package="ai.zara.app" resource-id="fake:{label}" '
        'bounds="[10,10][90,90]" />'
    )
    system_action = module.ET.fromstring(
        f'<node text="{label}" package="android" resource-id="{resource_id}" '
        'bounds="[100,10][190,90]" />'
    )

    monkeypatch.setattr(device, "nodes", lambda: iter((wrong_action, system_action)))

    assert device.find(label) is system_action
