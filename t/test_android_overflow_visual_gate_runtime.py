import hashlib
import importlib.util
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
    device = Device("synthetic", tmp_path)
    trigger = node(bounds=(100, 20, 140, 34), label="Actions for test")
    action = node(bounds=(40, 40, 120, 80), label="Rename")
    device.find_contains = lambda fragment: trigger if fragment == "Actions for " else None
    device.find = lambda label: action if label == "Rename" else None
    device.capture = lambda screenshot_name: path
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


def test_overflow_visual_gate_uses_preopen_trigger_bounds_after_trigger_disappears(
    tmp_path: Path,
) -> None:
    """Opening a popup may hide its trigger; pre-open geometry stays authoritative."""
    device = synthetic_device(tmp_path, text_like_image())
    trigger_bounds = (100, 20, 140, 34)
    device.find_contains = lambda fragment: None
    twin = tmp_path / "overflow.xml"
    twin.write_text("<hierarchy><node text='Rename'/></hierarchy>", encoding="utf-8")
    device.capture_text_twin = lambda state: {
        "file": twin.name,
        "sha256": hashlib.sha256(twin.read_bytes()).hexdigest(),
    }

    device.assert_transient_surface_visible(
        trigger_fragment="Actions for ",
        trigger_bounds=trigger_bounds,
        action_labels=("Rename",),
        screenshot_name="trigger-hidden",
    )

    receipt = device.visual_checks[-1]
    assert receipt["trigger_bounds"] == [100, 20, 140, 34]


def test_overflow_visual_receipt_binds_same_state_screenshot_and_text_twin(
    tmp_path: Path,
) -> None:
    """Review evidence must bind pixels and UI semantics from the popup state."""
    device = synthetic_device(tmp_path, text_like_image())
    screenshot = tmp_path / "surface.png"
    twin = tmp_path / "overflow.xml"
    twin.write_text("<hierarchy><node text='Rename'/></hierarchy>", encoding="utf-8")
    twin_sha = hashlib.sha256(twin.read_bytes()).hexdigest()
    device.capture_text_twin = lambda state: {"file": twin.name, "sha256": twin_sha}

    device.assert_transient_surface_visible(
        trigger_fragment="Actions for ",
        trigger_bounds=(100, 20, 140, 34),
        action_labels=("Rename",),
        screenshot_name="bound-evidence",
    )

    receipt = device.visual_checks[-1]
    assert receipt["screenshot_sha256"] == hashlib.sha256(screenshot.read_bytes()).hexdigest()
    assert receipt["text_twin_file"] == twin.name
    assert receipt["text_twin_sha256"] == twin_sha
