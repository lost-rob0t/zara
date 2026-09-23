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
    image = Image.new("RGB", (160, 120), 10)
    draw = ImageDraw.Draw(image)
    draw.rectangle((40, 40, 119, 79), fill=88)
    for x, luma in ((55, 220), (58, 180), (61, 35), (64, 150)):
        draw.rectangle((x, 52, x + 1, 68), fill=luma)

    device = synthetic_device(tmp_path, image)
    device.assert_transient_surface_visible(
        trigger_fragment="Actions for ",
        action_labels=("Rename",),
        screenshot_name="interior-glyphs",
    )

    assert device.visual_checks[-1]["state"] == "interior-glyphs"
