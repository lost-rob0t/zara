import importlib.util
from pathlib import Path
import xml.etree.ElementTree as ET


WIDGET_ACCEPTANCE = Path("android/integration/widget_device_acceptance.py")
EMULATOR_GATE = Path("scripts/test-android-emulator-install.sh")


def _load_widget_acceptance():
    spec = importlib.util.spec_from_file_location(
        "widget_device_acceptance_test_module",
        WIDGET_ACCEPTANCE,
    )
    assert spec is not None
    assert spec.loader is not None
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)
    return module


def test_emulator_gate_runs_real_launcher_widget_acceptance() -> None:
    gate = EMULATOR_GATE.read_text(encoding="utf-8")

    assert "android/integration/widget_device_acceptance.py" in gate
    assert '--serial "$serial"' in gate
    assert '--source-sha "$source_sha"' in gate
    assert '--output "$evidence_dir/widgets"' in gate


def test_widget_acceptance_captures_exact_sha_same_state_evidence() -> None:
    source = WIDGET_ACCEPTANCE.read_text(encoding="utf-8")

    assert '["git", "rev-parse", "HEAD"]' in source
    assert "WidgetEvidenceActivity" in source
    assert "requestPinAppWidget" not in source
    assert "uiautomator" in source
    assert "screencap" in source
    assert ".ui.json" in source
    assert ".actions.json" in source
    assert "sha256" in source
    assert "source_sha" in source
    assert "visual_verdict" in source


def test_widget_acceptance_exercises_responsive_theme_runtime_and_process_death_paths() -> None:
    source = WIDGET_ACCEPTANCE.read_text(encoding="utf-8")

    for required in (
        "assistant-outrun",
        "assistant-light",
        "assistant-narrow-200pct",
        "runtime-fresh",
        "runtime-stale",
        "runtime-corrupt",
        "route-cold",
        "route-warm",
        "route-process-death",
        "font_scale",
        "2.0",
        "target_width_dp=320",
    ):
        assert required in source

    assert "LOCAL READY" in source
    assert "STALE" in source
    assert "LOCAL UNKNOWN" in source
    assert "Chat" in source
    assert "Runtime" in source


def test_route_action_waits_for_launcher_widget_after_home(tmp_path: Path) -> None:
    module = _load_widget_acceptance()
    device = module.Device("emulator-5554", tmp_path)
    action_node = ET.fromstring(
        '<node text="CHAT" clickable="true" enabled="true" bounds="[0,0][10,10]" />'
    )
    ready = {"value": False}

    device.home = lambda: None

    def wait_for(label: str, timeout: float = module.WAIT_SECONDS):
        if label == "CHAT":
            ready["value"] = True
            return action_node
        assert label == "Chat"
        return ET.Element("node")

    def hierarchy():
        assert ready["value"], "route action queried launcher before widget settled"
        root = ET.Element("hierarchy")
        root.append(action_node)
        return root

    device.wait_for = wait_for
    device.hierarchy = hierarchy
    device.tap_node = lambda node: None
    device._action_assertion = lambda root, label: {
        "label": label,
        "clickable": True,
        "enabled": True,
    }

    device.tap_action_and_assert_route("CHAT", "Chat", "route-cold")

    assert device.route_assertions == [
        {
            "scenario": "route-cold",
            "action": "CHAT",
            "expected_route": "Chat",
            "passed": True,
            "owner": {
                "label": "CHAT",
                "clickable": True,
                "enabled": True,
            },
        }
    ]


def test_terminate_main_process_targets_canonical_package(tmp_path: Path) -> None:
    module = _load_widget_acceptance()
    device = module.Device("emulator-5554", tmp_path)
    calls: list[tuple[str, ...]] = []

    device.main_pid = lambda: 4242
    device.home = lambda: calls.append(("home",))
    device.adb = lambda *arguments, **kwargs: calls.append(arguments) or ""

    device.terminate_main_process(4242)

    assert module.PACKAGE == "ai.zara.app"
    assert calls == [
        ("home",),
        ("shell", "am", "kill", module.PACKAGE),
    ]


def test_cold_and_warm_route_evidence_fences_process_identity() -> None:
    source = WIDGET_ACCEPTANCE.read_text(encoding="utf-8")
    exercise = source.split("def exercise(device: Device, source_sha: str) -> dict:", 1)[1]
    exercise = exercise.split("\ndef main() -> None:", 1)[0]

    assert "cold_pid = device.require_main_pid()" in exercise
    assert "device.terminate_main_process(cold_pid)" in exercise
    assert "device.wait_for_main_pid_absent(cold_pid)" in exercise
    assert "cold_after_pid = device.tap_action_and_assert_route" in exercise
    assert "previous_pid=cold_pid" in exercise
    assert "require_recreated=True" in exercise
    assert "warm_after_pid = device.tap_action_and_assert_route" in exercise
    assert "previous_pid=cold_after_pid" in exercise
    assert "require_recreated=False" in exercise
    assert "process-death" in exercise


def test_widget_acceptance_keeps_talkback_hardware_claim_unproven() -> None:
    source = WIDGET_ACCEPTANCE.read_text(encoding="utf-8")

    assert '"talkback_spoken_traversal": False' in source
    assert '"visual_verdict": "requires-vision-review"' in source
