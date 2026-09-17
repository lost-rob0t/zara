"""Exercise installed native Android surfaces and retain screenshot evidence."""

from __future__ import annotations

import argparse
import hashlib
import json
import os
from pathlib import Path
import re
import subprocess
import time
import xml.etree.ElementTree as ET


class Device:
    def __init__(self, serial: str, output: Path) -> None:
        self.serial = serial
        self.output = output
        self.screenshots: list[dict] = []
        self.profiles: list[dict] = []
        self._size_before_profile: str | None = None
        self._font_scale_before_profile: str | None = None

    def adb(self, *arguments: str, binary: bool = False):
        return subprocess.check_output(
            ["adb", "-s", self.serial, *arguments],
            timeout=30,
            text=not binary,
        )

    def nodes(self):
        self.adb("shell", "uiautomator", "dump", "/sdcard/zara-acceptance.xml")
        return ET.fromstring(self.adb("shell", "cat", "/sdcard/zara-acceptance.xml")).iter("node")

    def find(self, label: str):
        return next(
            (
                node
                for node in self.nodes()
                if label in (node.get("text"), node.get("content-desc"))
            ),
            None,
        )

    def size(self) -> tuple[int, int]:
        value = self.adb("shell", "wm", "size")
        return tuple(map(int, re.findall(r"(\d+)x(\d+)", value)[-1]))

    def density(self) -> int:
        value = self.adb("shell", "wm", "density")
        return int(re.findall(r"(\d+)", value)[-1])

    @staticmethod
    def bounds(node) -> tuple[int, int, int, int]:
        values = [int(value) for value in re.findall(r"\d+", node.attrib["bounds"])]
        if len(values) != 4:
            raise AssertionError(f"Malformed bounds: {node.attrib.get('bounds')}")
        return tuple(values)

    def reveal(self, label: str) -> None:
        width, height = self.size()
        for direction in (1, -1):
            for _ in range(6):
                if self.find(label) is not None:
                    return
                start, end = (height * 3 // 4, height // 3)
                if direction < 0:
                    start, end = end, start
                self.adb(
                    "shell",
                    "input",
                    "swipe",
                    str(width // 3),
                    str(start),
                    str(width // 3),
                    str(end),
                    "250",
                )
        raise AssertionError(f"Control is not reachable after scrolling: {label}")

    def reveal_horizontal(self, label: str) -> None:
        width, height = self.size()
        for direction in (1, -1):
            for _ in range(8):
                if self.find(label) is not None:
                    return
                start, end = (width * 4 // 5, width // 5)
                if direction < 0:
                    start, end = end, start
                self.adb(
                    "shell",
                    "input",
                    "swipe",
                    str(start),
                    str(max(120, height // 8)),
                    str(end),
                    str(max(120, height // 8)),
                    "220",
                )
        raise AssertionError(f"Tab is not reachable after horizontal scrolling: {label}")

    def tap(self, label: str) -> None:
        self.reveal(label)
        self._tap_found(label)

    def tap_tab(self, label: str) -> None:
        self.reveal_horizontal(label)
        self._tap_found(label)

    def _tap_found(self, label: str) -> None:
        node = self.find(label)
        if node is None:
            raise AssertionError(f"Control is not reachable: {label}")
        left, top, right, bottom = self.bounds(node)
        if right <= left or bottom <= top:
            raise AssertionError(f"Control has empty bounds: {label}")
        self.adb(
            "shell",
            "input",
            "tap",
            str((left + right) // 2),
            str((top + bottom) // 2),
        )

    def await_label(self, label: str, timeout: float = 20.0) -> None:
        deadline = time.monotonic() + timeout
        while time.monotonic() < deadline:
            if self.find(label) is not None:
                return
            time.sleep(0.2)
        raise AssertionError(f"Screen did not show {label}")

    def capture(self, name: str) -> None:
        data = self.adb("exec-out", "screencap", "-p", binary=True)
        if not data.startswith(b"\x89PNG\r\n\x1a\n"):
            raise AssertionError("Device did not produce a PNG screenshot")
        path = self.output / f"{name}.png"
        path.write_bytes(data)
        self.screenshots.append(
            {"state": name, "file": path.name, "sha256": hashlib.sha256(data).hexdigest()}
        )

    def start(self) -> None:
        self.adb("shell", "am", "force-stop", "ai.zara.app")
        self.adb("shell", "am", "start", "-W", "-n", "ai.zara.app/.MainActivity")
        self.await_label("Chat")

    def press_back(self) -> None:
        self.adb("shell", "input", "keyevent", "4")
        time.sleep(0.5)

    def recreate(self) -> None:
        # HOME + am kill preserves the task/saved-state path while killing the app process.
        # Restarting the existing Activity exercises process recreation rather than merely
        # rebuilding a composable in the same process.
        self.adb("shell", "input", "keyevent", "3")
        time.sleep(0.5)
        self.adb("shell", "am", "kill", "ai.zara.app")
        time.sleep(0.8)
        self.adb("shell", "am", "start", "-W", "-n", "ai.zara.app/.MainActivity")
        time.sleep(0.8)

    def set_narrow_large_font(self) -> None:
        if self._size_before_profile is not None:
            raise AssertionError("Display profile already overridden")
        self._size_before_profile = self.adb("shell", "wm", "size")
        self._font_scale_before_profile = self.adb(
            "shell", "settings", "get", "system", "font_scale"
        ).strip()
        width, height = self.size()
        density = self.density()
        target_width = max(1, round(320 * density / 160))
        target_height = max(target_width, round(height * target_width / width))
        self.adb("shell", "wm", "size", f"{target_width}x{target_height}")
        self.adb("shell", "settings", "put", "system", "font_scale", "1.30")
        self.profiles.append(
            {
                "name": "narrow-large-font",
                "size_px": f"{target_width}x{target_height}",
                "target_width_dp": 320,
                "font_scale": "1.30",
            }
        )
        time.sleep(1.2)

    def restore_profile(self) -> None:
        if self._size_before_profile is None:
            return
        original = self._size_before_profile
        override = re.search(r"Override size:\s*(\d+x\d+)", original)
        if override:
            self.adb("shell", "wm", "size", override.group(1))
        else:
            self.adb("shell", "wm", "size", "reset")
        original_font = self._font_scale_before_profile
        if original_font and original_font != "null":
            self.adb("shell", "settings", "put", "system", "font_scale", original_font)
        else:
            self.adb("shell", "settings", "put", "system", "font_scale", "1.0")
        self._size_before_profile = None
        self._font_scale_before_profile = None
        time.sleep(1.0)


def open_menu(device: Device, menu: str) -> None:
    device.tap("Open navigation menu")
    for expected in ("Chat", "Workspace", "Settings"):
        device.await_label(expected)
    device.tap(menu)
    device.await_label(menu)
    time.sleep(0.4)


def exercise_three_menu_ui(device: Device) -> None:
    device.start()
    device.capture("empty-shell")

    device.tap("Open navigation menu")
    for menu in ("Chat", "Workspace", "Settings"):
        device.await_label(menu)
    device.capture("drawer-open")
    device.tap("Workspace")
    device.await_label("Logic")
    device.capture("workspace-logic")
    for tab in ("Projects", "Scheduled"):
        device.tap_tab(tab)
        time.sleep(0.4)
        device.capture(f"workspace-{tab.lower()}")

    open_menu(device, "Settings")
    for tab in (
        "Runtime",
        "Connection",
        "Permissions",
        "Appearance",
        "Plugins",
        "Updates",
        "Diagnostics",
        "About",
    ):
        device.tap_tab(tab)
        time.sleep(0.4)
        device.capture(f"settings-{tab.lower()}")

    device.tap_tab("Appearance")
    device.tap("Outrun")
    time.sleep(0.4)
    device.capture("theme-outrun")
    device.tap("Light")
    time.sleep(0.4)
    device.capture("theme-light")
    device.tap("Outrun")

    open_menu(device, "Chat")
    device.set_narrow_large_font()
    device.await_label("Chat")
    device.capture("narrow-large-font")
    device.restore_profile()
    device.await_label("Chat")

    # Focusing but never submitting the composer is enough to prove the IME viewport.
    device.await_label("Ask anything…", timeout=30.0)
    device.tap("Ask anything…")
    time.sleep(0.8)
    device.capture("ime-composer")
    device.press_back()

    open_menu(device, "Settings")
    device.tap_tab("Diagnostics")
    device.await_label("Diagnostics")
    device.recreate()
    device.await_label("Diagnostics")
    device.capture("recreated-settings")

    device.press_back()
    device.await_label("Runtime")
    device.press_back()
    device.await_label("Chat")
    device.capture("back-to-chat")


def main() -> None:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--serial", default=os.environ.get("ANDROID_SERIAL"))
    parser.add_argument(
        "--output", type=Path, default=Path("android/app/build/reports/device")
    )
    args = parser.parse_args()
    if not args.serial:
        parser.error("Select a test emulator explicitly with --serial or ANDROID_SERIAL")
    args.output.mkdir(parents=True, exist_ok=True)
    device = Device(args.serial, args.output)
    result = {
        "source_sha": subprocess.check_output(["git", "rev-parse", "HEAD"], text=True).strip(),
        "serial": args.serial,
        "passed": False,
        "screenshots": device.screenshots,
        "profiles": device.profiles,
    }
    try:
        result["device"] = {
            "api": device.adb("shell", "getprop", "ro.build.version.sdk").strip(),
            "size": device.adb("shell", "wm", "size").strip(),
            "density": device.adb("shell", "wm", "density").strip(),
            "font_scale": device.adb(
                "shell", "settings", "get", "system", "font_scale"
            ).strip(),
        }
        exercise_three_menu_ui(device)
        result["passed"] = True
    except BaseException as error:
        result["failure"] = str(error)
        try:
            device.capture("failure")
        except Exception as capture_error:
            result["capture_failure"] = str(capture_error)
        raise
    finally:
        try:
            device.restore_profile()
        except Exception as restore_error:
            result["profile_restore_failure"] = str(restore_error)
        (args.output / "manifest.json").write_text(json.dumps(result, indent=2) + "\n")


if __name__ == "__main__":
    main()
