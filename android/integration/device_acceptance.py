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
            (node for node in self.nodes()
             if label in (node.get("text"), node.get("content-desc"))),
            None,
        )

    def reveal(self, label: str) -> None:
        size = self.adb("shell", "wm", "size")
        width, height = map(int, re.findall(r"(\d+)x(\d+)", size)[-1])
        for direction in (1, -1):
            for _ in range(5):
                if self.find(label) is not None:
                    return
                start, end = (height * 3 // 4, height // 3)
                if direction < 0:
                    start, end = end, start
                self.adb("shell", "input", "swipe", str(width // 3), str(start), str(width // 3), str(end), "250")
        raise AssertionError(f"Control is not reachable after scrolling: {label}")

    def tap(self, label: str) -> None:
        self.reveal(label)
        node = self.find(label)
        if node is None:
            raise AssertionError(f"Control is not reachable: {label}")
        bounds = [int(value) for value in re.findall(r"\d+", node.attrib["bounds"])]
        left, top, right, bottom = bounds
        if right <= left or bottom <= top:
            raise AssertionError(f"Control has empty bounds: {label}")
        self.adb("shell", "input", "tap", str((left + right) // 2), str((top + bottom) // 2))

    def await_label(self, label: str) -> None:
        deadline = time.monotonic() + 20
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
        self.screenshots.append({"state": name, "file": path.name, "sha256": hashlib.sha256(data).hexdigest()})

    def start(self) -> None:
        self.adb("shell", "am", "start", "-W", "-n", "ai.zara.app/.MainActivity")
        self.await_label("Chat")


def main() -> None:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--serial", default=os.environ.get("ANDROID_SERIAL"))
    parser.add_argument("--output", type=Path, default=Path("android/app/build/reports/device"))
    args = parser.parse_args()
    if not args.serial:
        parser.error("Select a test emulator explicitly with --serial or ANDROID_SERIAL")
    args.output.mkdir(parents=True, exist_ok=True)
    device = Device(args.serial, args.output)
    result = {"source_sha": subprocess.check_output(["git", "rev-parse", "HEAD"], text=True).strip(),
              "serial": args.serial, "passed": False, "screenshots": device.screenshots}
    try:
        result["device"] = {
            "api": device.adb("shell", "getprop", "ro.build.version.sdk").strip(),
            "size": device.adb("shell", "wm", "size").strip(),
            "density": device.adb("shell", "wm", "density").strip(),
            "font_scale": device.adb("shell", "settings", "get", "system", "font_scale").strip(),
        }
        device.start()
        device.capture("empty-shell")
        device.tap("☰")
        device.await_label("Logic")
        device.capture("drawer-open")
        for route in ("Chat", "Logic", "Voice", "Projects", "Remote", "Scheduled", "Plugins", "Themes", "Diagnostics", "Settings", "About"):
            device.reveal(route)
        device.tap("Themes")
        device.await_label("Appearance")
        device.capture("theme-selector")
        for theme in ("StarIntel", "Light", "Terminal", "Midnight", "Outrun", "System"):
            device.tap(theme)
            device.capture(f"theme-{theme.lower()}")
        result["passed"] = True
    except BaseException as error:
        result["failure"] = str(error)
        try:
            device.capture("failure")
        except Exception as capture_error:
            result["capture_failure"] = str(capture_error)
        raise
    finally:
        (args.output / "manifest.json").write_text(json.dumps(result, indent=2) + "\n")


if __name__ == "__main__":
    main()
