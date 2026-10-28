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


def test_await_absent_waits_for_navigation_drawer_to_close(
    monkeypatch: pytest.MonkeyPatch,
    tmp_path: Path,
) -> None:
    module = _load_device_acceptance_module()
    device = module.Device("emulator-5554", tmp_path)
    drawer = {"open": True}
    clock = {"value": 0.0}
    observations = 0

    def fake_monotonic() -> float:
        clock["value"] += 0.01
        return clock["value"]

    def fake_find(label: str):
        nonlocal observations
        assert label == "Close navigation menu"
        observations += 1
        if observations == 2:
            drawer["open"] = False
        return object() if drawer["open"] else None

    monkeypatch.setattr(module.time, "monotonic", fake_monotonic)
    monkeypatch.setattr(module.time, "sleep", lambda _seconds: None)
    monkeypatch.setattr(device, "find", fake_find)
    monkeypatch.setattr(device, "dismiss_pixel_launcher_anr", lambda: False)

    device.await_absent("Close navigation menu", timeout=0.1)

    assert observations == 2


def test_open_menu_requires_drawer_dismissal_and_destination_content(
    monkeypatch: pytest.MonkeyPatch,
    tmp_path: Path,
) -> None:
    module = _load_device_acceptance_module()
    device = module.Device("emulator-5554", tmp_path)
    calls: list[tuple] = []
    for name in ("tap_tag", "await_drawer_state", "await_tag", "await_absent", "await_navigation", "await_label"):
        monkeypatch.setattr(device, name, lambda *args, _name=name, **kwargs: calls.append((_name, args, kwargs)))

    module.open_menu(device, "Settings")

    assert calls == [
        ("tap_tag", ("zara.navigation.open-menu",), {}),
        ("await_drawer_state", (True,), {}),
        ("await_tag", ("zara.menu.chat",), {}),
        ("await_tag", ("zara.menu.workspace",), {}),
        ("await_tag", ("zara.menu.settings",), {}),
        ("tap_tag", ("zara.menu.settings",), {}),
        ("await_absent", ("Close navigation menu",), {}),
        ("await_navigation", ("settings", "settings"), {"drawer_open": False}),
        ("await_label", ("Runtime & local AI",), {}),
    ]


def test_semantic_target_disambiguates_duplicate_visible_labels(monkeypatch, tmp_path):
    module = _load_device_acceptance_module()
    device = module.Device("emulator-5554", tmp_path)
    duplicate_menu = module.ET.fromstring(
        '<node text="Chat" resource-id="zara.menu.chat" bounds="[1,2][11,12]" />'
    )
    duplicate_tab = module.ET.fromstring(
        '<node text="Chat" resource-id="zara.route.chat" bounds="[21,22][31,32]" />'
    )
    monkeypatch.setattr(device, "nodes", lambda: iter((duplicate_menu, duplicate_tab)))
    taps = []
    monkeypatch.setattr(device, "adb", lambda *args, **kwargs: taps.append(args) or "")

    device.tap_tag("zara.route.chat")

    assert taps[-1][:3] == ("shell", "input", "tap")
    assert taps[-1][-2:] == ("26", "27")


def test_await_drawer_state_fails_if_drawer_remains_open(monkeypatch, tmp_path):
    module = _load_device_acceptance_module()
    device = module.Device("emulator-5554", tmp_path)
    menu = module.ET.fromstring(
        '<node resource-id="zara.navigation.drawer-state" content-desc="Navigation drawer open" />'
    )
    clock = {"value": 0.0}
    monkeypatch.setattr(module.time, "monotonic", lambda: clock.__setitem__("value", clock["value"] + 0.02) or clock["value"])
    monkeypatch.setattr(module.time, "sleep", lambda _seconds: None)
    monkeypatch.setattr(device, "find_tag", lambda _tag: menu)
    monkeypatch.setattr(device, "dismiss_pixel_launcher_anr", lambda: False)

    with pytest.raises(AssertionError, match="did not reach closed state"):
        device.await_drawer_state(False, timeout=0.05)


def test_await_drawer_state_requires_explicit_open_semantics(monkeypatch, tmp_path):
    module = _load_device_acceptance_module()
    device = module.Device("emulator-5554", tmp_path)
    drawer = module.ET.fromstring(
        '<node resource-id="zara.navigation.drawer-state" content-desc="Navigation drawer open" />'
    )
    monkeypatch.setattr(device, "find_tag", lambda tag: drawer if tag == "zara.navigation.drawer-state" else None)
    monkeypatch.setattr(device, "dismiss_pixel_launcher_anr", lambda: False)

    assert device.await_drawer_state(True, timeout=0.05) is None


def test_await_navigation_rejects_wrong_destination(monkeypatch, tmp_path):
    module = _load_device_acceptance_module()
    device = module.Device("emulator-5554", tmp_path)
    menu = module.ET.fromstring(
        '<node resource-id="zara.navigation.drawer-state" content-desc="Navigation drawer closed" />'
    )
    wrong_screen = module.ET.fromstring(
        '<node resource-id="zara.screen.chat" />'
    )
    active_menu = module.ET.fromstring('<node resource-id="zara.menu.active.settings" />')
    clock = {"value": 0.0}
    monkeypatch.setattr(module.time, "monotonic", lambda: clock.__setitem__("value", clock["value"] + 0.02) or clock["value"])
    monkeypatch.setattr(module.time, "sleep", lambda _seconds: None)
    monkeypatch.setattr(
        device,
        "find_tag",
        lambda tag: {
            "zara.navigation.drawer-state": menu,
            "zara.screen.settings": None,
            "zara.screen.chat": wrong_screen,
            "zara.menu.active.settings": active_menu,
        }.get(tag),
    )
    monkeypatch.setattr(device, "find", lambda _label: None)
    monkeypatch.setattr(device, "dismiss_pixel_launcher_anr", lambda: False)

    with pytest.raises(AssertionError, match="Navigation state mismatch"):
        device.await_navigation("settings", "settings", drawer_open=False)


def test_failure_capture_bundles_screenshot_hierarchy_and_logs(tmp_path, monkeypatch):
    module = _load_device_acceptance_module()
    device = module.Device("emulator-5554", tmp_path)
    hierarchy = '<hierarchy><node text="Settings" resource-id="zara.screen.settings" /></hierarchy>'
    screenshot = b"\x89PNG\r\n\x1a\nfixture"

    def fake_adb(*args, **kwargs):
        if args[:3] == ("exec-out", "screencap", "-p"):
            return screenshot
        if args[:3] == ("shell", "uiautomator", "dump"):
            return "dumped"
        if args[:2] == ("shell", "cat"):
            return hierarchy
        if args[:2] == ("shell", "pidof"):
            return "1234"
        if args[:2] == ("shell", "logcat"):
            return "Zara failure fixture"
        return ""

    monkeypatch.setattr(device, "adb", fake_adb)
    try:
        raise AssertionError("navigation failed")
    except AssertionError:
        device.capture("failure")

    evidence = device.screenshots[0]
    assert evidence["scenario_id"] == "failure"
    assert evidence["outcome"] == "captured"
    assert (tmp_path / evidence["screenshot"]["file"]).read_bytes() == screenshot
    assert (tmp_path / evidence["ui_hierarchy"]["file"]).read_text() == hierarchy
    assert "Zara failure fixture" in (tmp_path / evidence["logs"]["file"]).read_text()


def test_production_compose_exposes_semantic_destination_menu_and_route_ids():
    app_ui = Path(
        "android/app/src/main/java/ai/zara/app/ui/ZaraApp.kt"
    ).read_text(encoding="utf-8")
    navigation = Path(
        "android/app/src/main/java/ai/zara/app/ui/ZaraNavigationBars.kt"
    ).read_text(encoding="utf-8")
    settings = Path(
        "android/app/src/main/java/ai/zara/app/ui/SettingsOverview.kt"
    ).read_text(encoding="utf-8")

    assert "testTagsAsResourceId = true" in app_ui
    assert 'testTag("zara.navigation.open-menu")' in app_ui
    assert 'testTag("zara.navigation.drawer-state")' in app_ui
    assert 'testTag("zara.menu.${menu.name.lowercase()}")' in app_ui
    assert 'testTag("zara.menu.active.${selected.name.lowercase()}")' in app_ui
    assert 'testTag("zara.screen.${navigation.route.name.lowercase()}")' in app_ui
    assert 'stateDescription = if (drawerOpen)' in app_ui
    assert 'testTag("zara.route.${route.name.lowercase()}")' in navigation
    assert 'testTag("zara.route.${destination.name.lowercase()}")' in settings


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
