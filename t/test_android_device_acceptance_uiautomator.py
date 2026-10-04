from __future__ import annotations

import importlib.util
from pathlib import Path
import subprocess
import xml.etree.ElementTree as ET

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


def test_capture_rejects_split_rendered_action_ownership(
    monkeypatch: pytest.MonkeyPatch,
    tmp_path: Path,
) -> None:
    module = _load_device_acceptance_module()
    device = module.Device("emulator-5554", tmp_path)
    unlabeled_click_owner = ET.fromstring(
        '<node text="" content-desc="" class="android.view.View" '
        'bounds="[205,2034][875,2126]" clickable="true" enabled="true" />'
    )
    labeled_click_owner = ET.fromstring(
        '<node text="Choose APK" content-desc="" class="android.widget.TextView" '
        'bounds="[408,2057][672,2102]" clickable="true" enabled="true" />'
    )

    monkeypatch.setattr(
        device,
        "adb",
        lambda *arguments, **_kwargs: b"\x89PNG\r\n\x1a\nfixture"
        if arguments[:2] == ("exec-out", "screencap")
        else "",
    )
    monkeypatch.setattr(
        device,
        "nodes",
        lambda: iter((unlabeled_click_owner, labeled_click_owner)),
    )

    with pytest.raises(
        AssertionError,
        match=r"Required rendered action has a distinct unlabeled clickable owner: Choose APK",
    ):
        device.capture("settings-plugins", required_actions=("Choose APK",))



def test_tap_scrolls_clipped_action_inside_scroll_view_before_tapping(
    monkeypatch: pytest.MonkeyPatch,
    tmp_path: Path,
) -> None:
    module = _load_device_acceptance_module()
    device = module.Device("emulator-5554", tmp_path)
    scroll_view = ET.fromstring(
        '<node class="android.widget.ScrollView" bounds="[0,487][840,1611]" '
        'clickable="false" enabled="true" />'
    )
    clipped_action = ET.fromstring(
        '<node text="Choose APK" class="android.widget.Button" '
        'bounds="[89,1600][751,1674]" clickable="true" enabled="true" />'
    )
    visible_action = ET.fromstring(
        '<node text="Choose APK" class="android.widget.Button" '
        'bounds="[89,905][751,1067]" clickable="true" enabled="true" />'
    )
    swipes = 0
    taps: list[tuple[str, ...]] = []

    def fake_nodes():
        action = visible_action if swipes else clipped_action
        scroll_view[:] = [action]
        return iter((scroll_view, action))

    def fake_adb(*arguments: str, **_kwargs):
        nonlocal swipes
        if arguments == ("shell", "wm", "size"):
            return "Physical size: 840x1867"
        if arguments[:3] == ("shell", "input", "swipe"):
            swipes += 1
            return ""
        if arguments[:3] == ("shell", "input", "tap"):
            taps.append(arguments[3:])
            return ""
        raise AssertionError(f"unexpected adb call: {arguments!r}")

    monkeypatch.setattr(device, "nodes", fake_nodes)
    monkeypatch.setattr(device, "adb", fake_adb)

    device.tap("Choose APK")

    assert swipes == 1
    assert taps == [("420", "986")]


@pytest.mark.parametrize(
    ("container_class", "initial_bounds", "visible_bounds", "horizontal"),
    [
        ("android.widget.ScrollView", "[89,450][751,500]", "[89,905][751,1067]", False),
        ("android.widget.ScrollView", "[89,1600][751,1674]", "[89,905][751,1067]", False),
        ("android.widget.HorizontalScrollView", "[790,600][890,680]", "[300,600][440,680]", True),
        ("android.widget.ScrollView", "[89,1850][751,1950]", "[89,905][751,1067]", False),
    ],
)
def test_tap_reveals_controls_in_their_own_scroll_viewport(
    monkeypatch, tmp_path, container_class, initial_bounds, visible_bounds, horizontal,
):
    module = _load_device_acceptance_module()
    device = module.Device("emulator-5554", tmp_path)
    swipes = []
    taps = []

    def fake_adb(*arguments, **_kwargs):
        if arguments == ("shell", "wm", "size"):
            return "Physical size: 840x1867"
        if arguments[:3] == ("shell", "input", "swipe"):
            swipes.append(tuple(map(int, arguments[3:7])))
            return ""
        if arguments[:3] == ("shell", "input", "tap"):
            taps.append(tuple(map(int, arguments[3:])))
            return ""
        if arguments[:2] == ("shell", "cat"):
            bounds = visible_bounds if swipes else initial_bounds
            return (
                '<hierarchy><node class="' + container_class + '" '
                'bounds="[0,487][840,1611]" scrollable="true">'
                '<node text="Choose APK" bounds="' + bounds + '" '
                'clickable="true" enabled="true" /></node></hierarchy>'
            )
        if arguments[:3] in (("shell", "rm", "-f"), ("shell", "uiautomator", "dump")):
            return ""
        raise AssertionError(f"unexpected adb call: {arguments!r}")

    monkeypatch.setattr(device, "adb", fake_adb)
    if horizontal:
        device.tap_tab("Choose APK")
    else:
        device.tap("Choose APK")

    assert len(swipes) == 1
    start_x, start_y, end_x, end_y = swipes[0]
    assert 0 < start_x < 840 and 0 < end_x < 840
    assert 487 < start_y < 1611 and 487 < end_y < 1611
    if horizontal:
        assert start_x > end_x
        assert taps == [(370, 640)]
    else:
        assert (start_y < end_y) == (initial_bounds == "[89,450][751,500]")
        assert taps == [(420, 986)]


def test_tap_ignores_unrelated_scroll_view_and_rejects_layout_change(monkeypatch, tmp_path):
    module = _load_device_acceptance_module()
    device = module.Device("emulator-5554", tmp_path)
    unrelated = ET.fromstring(
        '<node class="android.widget.ScrollView" bounds="[0,0][840,500]" />'
    )
    action = ET.fromstring('<node text="Choose APK" bounds="[89,905][751,1067]" />')
    taps = []

    def fake_adb(*arguments, **_kwargs):
        if arguments == ("shell", "wm", "size"):
            return "Physical size: 840x1867"
        if arguments[:3] == ("shell", "input", "tap"):
            taps.append(arguments[3:])
            return ""
        raise AssertionError(f"unexpected adb call: {arguments!r}")

    monkeypatch.setattr(device, "nodes", lambda: iter((unrelated, action)))
    monkeypatch.setattr(device, "adb", fake_adb)
    device.tap("Choose APK")
    assert taps == [("420", "986")]

    action.set("bounds", "[89,1850][751,1950]")
    with pytest.raises(AssertionError, match="visible"):
        device._tap_found("Choose APK")
    assert len(taps) == 1


def test_bounds_preserves_negative_offscreen_coordinates():
    module = _load_device_acceptance_module()
    node = ET.fromstring('<node bounds="[-100,-80][40,20]" />')
    assert module.Device.bounds(node) == (-100, -80, 40, 20)


@pytest.mark.parametrize("visible_after", [None, 12])
def test_reveal_has_bounded_retries_and_checks_the_last_swipe(monkeypatch, tmp_path, visible_after):
    module = _load_device_acceptance_module()
    device = module.Device("emulator-5554", tmp_path)
    calls = []
    action = ET.fromstring('<node text="Choose APK" bounds="[100,100][300,200]" />')

    def fake_nodes():
        visible = visible_after is not None and len(calls) >= visible_after
        return iter((action,) if visible else ())

    def fake_adb(*arguments, **_kwargs):
        if arguments == ("shell", "wm", "size"):
            return "Physical size: 840x1867"
        assert arguments[:3] == ("shell", "input", "swipe")
        calls.append(arguments)
        return ""

    monkeypatch.setattr(device, "nodes", fake_nodes)
    monkeypatch.setattr(device, "adb", fake_adb)
    if visible_after is None:
        with pytest.raises(AssertionError, match="not reachable after scrolling"):
            device.reveal("Choose APK")
    else:
        device.reveal("Choose APK")
    assert len(calls) == 12
    assert int(calls[0][4]) > int(calls[0][6])
    assert int(calls[-1][4]) < int(calls[-1][6])


def test_nested_scroll_viewports_use_their_intersection_and_skip_clipped_duplicates(monkeypatch, tmp_path):
    module = _load_device_acceptance_module()
    device = module.Device("emulator-5554", tmp_path)
    hierarchy = ET.fromstring(
        '<hierarchy><node class="android.widget.ScrollView" bounds="[0,200][840,1000]">'
        '<node class="android.widget.ScrollView" bounds="[0,100][840,1600]">'
        '<node text="Choose APK" bounds="[100,1100][300,1200]" />'
        '</node></node><node text="Choose APK" bounds="[100,600][300,700]" /></hierarchy>'
    )
    monkeypatch.setattr(device, "nodes", lambda: hierarchy.iter("node"))
    node, viewport = device._locate_control("Choose APK", 840, 1867)
    assert device.bounds(node) == (100, 600, 300, 700)
    assert viewport == (0, 0, 840, 1867)
    hierarchy.remove(list(hierarchy)[-1])
    node, viewport = device._locate_control("Choose APK", 840, 1867)
    assert viewport == (0, 200, 840, 1000)
    assert not device._center_visible(node, viewport)


@pytest.mark.parametrize("bounds", ["[0,0][0,0]", "[10,10][5,5]"])
def test_tap_rejects_empty_or_inverted_bounds(monkeypatch, tmp_path, bounds):
    module = _load_device_acceptance_module()
    device = module.Device("emulator-5554", tmp_path)
    action = ET.fromstring('<node text="Choose APK" bounds="' + bounds + '" />')
    monkeypatch.setattr(device, "nodes", lambda: iter((action,)))
    monkeypatch.setattr(device, "size", lambda: (840, 1867))
    with pytest.raises(AssertionError, match="empty bounds"):
        device._tap_found("Choose APK")


def test_missing_tab_scrolls_the_tab_bar_in_both_directions(monkeypatch, tmp_path):
    module = _load_device_acceptance_module()
    device = module.Device("emulator-5554", tmp_path)
    swipes = []
    monkeypatch.setattr(device, "nodes", lambda: iter(()))
    monkeypatch.setattr(device, "size", lambda: (840, 1867))
    monkeypatch.setattr(device, "adb", lambda *arguments: swipes.append(arguments))
    with pytest.raises(AssertionError, match="not reachable after scrolling"):
        device.reveal_horizontal("Plugins")
    assert len(swipes) == 16
    assert all(swipe[4] == swipe[6] == "233" for swipe in swipes)
    assert int(swipes[0][3]) > int(swipes[0][5])
    assert int(swipes[-1][3]) < int(swipes[-1][5])

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
