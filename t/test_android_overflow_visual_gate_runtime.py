import hashlib
import importlib.util
import json
from pathlib import Path
import xml.etree.ElementTree as ET

from PIL import Image, ImageDraw
import pytest


MODULE_PATH = Path("android/integration/device_acceptance.py")
SPEC = importlib.util.spec_from_file_location("device_acceptance_runtime", MODULE_PATH)
assert SPEC is not None and SPEC.loader is not None
DEVICE_ACCEPTANCE = importlib.util.module_from_spec(SPEC)
SPEC.loader.exec_module(DEVICE_ACCEPTANCE)
Device = DEVICE_ACCEPTANCE.Device
SOURCE_SHA = "a" * 40
APK_SHA256 = "b" * 64


def node(*, bounds: tuple[int, int, int, int], label: str) -> ET.Element:
    left, top, right, bottom = bounds
    return ET.Element(
        "node",
        {
            "text": label,
            "content-desc": label,
            "bounds": f"[{left},{top}][{right},{bottom}]",
        },
    )


def synthetic_device(tmp_path: Path, image: Image.Image) -> Device:
    path = tmp_path / "surface.png"
    image.save(path, format="PNG")
    screenshot_bytes = path.read_bytes()
    hierarchy = "<hierarchy><node text='Rename'/></hierarchy>"
    device = Device("synthetic", tmp_path)
    device.source_sha = SOURCE_SHA
    device.apk_sha256 = APK_SHA256
    device.device_api = "35"
    device.current_profile = "default"
    device.current_route = "chat"
    device.runtime_evidence = {
        "mode": None,
        "runtime_id": None,
        "model": None,
        "quantization": None,
        "phase": None,
    }
    trigger = node(bounds=(100, 20, 140, 34), label="Actions for test")
    action = node(bounds=(40, 40, 120, 80), label="Rename")
    device.find_contains = lambda fragment: trigger if fragment == "Actions for " else None
    device.find = lambda label: action if label == "Rename" else None
    device._hierarchy_text = lambda: hierarchy

    def synthetic_adb(*arguments: str, binary: bool = False):
        if arguments == ("exec-out", "screencap", "-p") and binary:
            return screenshot_bytes
        raise AssertionError(f"Unexpected synthetic adb call: {arguments!r} binary={binary}")

    device.adb = synthetic_adb
    return device


def text_like_image() -> Image.Image:
    image = Image.new("RGB", (160, 120), 10)
    draw = ImageDraw.Draw(image)
    draw.rectangle((40, 40, 119, 79), fill=88)
    for x, luma in ((55, 220), (58, 180), (61, 35), (64, 150)):
        draw.rectangle((x, 52, x + 1, 68), fill=luma)
    return image


def test_overflow_visual_gate_rejects_border_only_contrast(tmp_path: Path) -> None:
    """A high-contrast popup frame must not masquerade as readable action text."""
    image = Image.new("RGB", (160, 120), 10)
    draw = ImageDraw.Draw(image)
    draw.rectangle((40, 40, 119, 79), fill=88)
    for start, end, luma in (
        (40, 59, 20),
        (60, 79, 48),
        (80, 99, 132),
        (100, 119, 210),
    ):
        draw.line((start, 40, end, 40), fill=luma, width=1)

    device = synthetic_device(tmp_path, image)

    with pytest.raises(AssertionError, match="visually blank"):
        device.assert_transient_surface_visible(
            trigger_fragment="Actions for ",
            action_labels=("Rename",),
            screenshot_name="border-only",
        )


def test_overflow_visual_gate_accepts_interior_text_like_contrast(tmp_path: Path) -> None:
    """Deterministic interior glyph-like strokes remain acceptable visual evidence."""
    device = synthetic_device(tmp_path, text_like_image())
    device.assert_transient_surface_visible(
        trigger_fragment="Actions for ",
        action_labels=("Rename",),
        screenshot_name="interior-glyphs",
    )

    assert device.visual_checks[-1]["state"] == "interior-glyphs"
    assert device.scenario_evidence[-1]["scenario_id"] == "android.ui.interior-glyphs"


def test_overflow_visual_gate_uses_preopen_trigger_bounds_after_trigger_disappears(
    tmp_path: Path,
) -> None:
    """Opening a popup may hide its trigger; pre-open geometry stays authoritative."""
    device = synthetic_device(tmp_path, text_like_image())
    trigger_bounds = (100, 20, 140, 34)
    device.find_contains = lambda fragment: None

    device.assert_transient_surface_visible(
        trigger_fragment="Actions for ",
        trigger_bounds=trigger_bounds,
        action_labels=("Rename",),
        screenshot_name="trigger-hidden",
    )

    receipt = device.visual_checks[-1]
    assert receipt["trigger_bounds"] == [100, 20, 140, 34]
    assert device.scenario_evidence[-1]["scenario_id"] == "android.ui.trigger-hidden"


def test_overflow_visual_receipt_binds_same_state_screenshot_and_text_twin(
    tmp_path: Path,
) -> None:
    """Review evidence must bind pixels and the capture-fenced scenario text twin."""
    device = synthetic_device(tmp_path, text_like_image())
    screenshot = tmp_path / "bound-evidence.png"

    device.assert_transient_surface_visible(
        trigger_fragment="Actions for ",
        trigger_bounds=(100, 20, 140, 34),
        action_labels=("Rename",),
        screenshot_name="bound-evidence",
    )

    scenario = device.scenario_evidence[-1]
    receipt = device.visual_checks[-1]
    twin = tmp_path / scenario["text_evidence"]["file"]
    persisted = json.loads((tmp_path / "bound-evidence.json").read_text(encoding="utf-8"))
    twin_text = twin.read_text(encoding="utf-8")

    assert receipt["screenshot_sha256"] == hashlib.sha256(screenshot.read_bytes()).hexdigest()
    assert receipt["text_twin_file"] == scenario["text_evidence"]["file"]
    assert receipt["text_twin_sha256"] == scenario["text_evidence"]["sha256"]
    assert receipt["text_twin_sha256"] == hashlib.sha256(twin.read_bytes()).hexdigest()
    assert persisted["text_evidence"] == scenario["text_evidence"]
    assert 'text="Rename"' in twin_text
    assert (
        "ASSERT PASS transient-surface-visible "
        "actions=Rename viewport=160x120"
    ) in twin_text


def test_overflow_visual_receipt_reuses_capture_bound_text_evidence(
    tmp_path: Path,
) -> None:
    """The visual receipt must not recapture UI semantics after the screenshot fence."""
    device = synthetic_device(tmp_path, text_like_image())

    def reject_late_recapture(_state: str) -> dict:
        raise AssertionError("late UI hierarchy recapture escaped the screenshot fence")

    device.capture_text_twin = reject_late_recapture
    device.assert_transient_surface_visible(
        trigger_fragment="Actions for ",
        trigger_bounds=(100, 20, 140, 34),
        action_labels=("Rename",),
        screenshot_name="same-state-bound",
    )

    scenario = device.scenario_evidence[-1]
    receipt = device.visual_checks[-1]
    assert receipt["text_twin_file"] == scenario["text_evidence"]["file"]
    assert receipt["text_twin_sha256"] == scenario["text_evidence"]["sha256"]
