"""Capture exact-SHA evidence from real Zara home-screen widgets on an Android launcher."""

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


REPO_ROOT = Path(__file__).resolve().parents[2]
PACKAGE = "ai.zara.app"
EVIDENCE_ACTIVITY = "ai.zara.app/.widget.WidgetEvidenceActivity"
UI_DUMP_PATH = "/data/local/tmp/zara-widget-evidence.xml"
SOURCE_SHA_RE = re.compile(r"[0-9a-f]{40}")
PIN_LABELS = ("Add to Home screen", "Add to home screen", "Add")
PIN_FAILURES = ("PIN_UNSUPPORTED", "PIN_REJECTED", "UNKNOWN_WIDGET_KIND")
WAIT_SECONDS = 20.0


def verified_source_sha(claimed_source_sha: str | None) -> str:
    actual = subprocess.check_output(
        ["git", "rev-parse", "HEAD"],
        cwd=REPO_ROOT,
        text=True,
    ).strip()
    if not SOURCE_SHA_RE.fullmatch(actual):
        raise RuntimeError(f"Repository HEAD is not an immutable source SHA: {actual!r}")
    if claimed_source_sha is None:
        return actual
    if not SOURCE_SHA_RE.fullmatch(claimed_source_sha):
        raise ValueError(f"Claimed source SHA is invalid: {claimed_source_sha!r}")
    if claimed_source_sha != actual:
        raise RuntimeError(
            "Claimed source SHA does not match the checked-out repository: "
            f"claimed={claimed_source_sha} actual={actual}"
        )
    return actual


class Device:
    def __init__(self, serial: str, output: Path) -> None:
        self.serial = serial
        self.output = output
        self.bundles: list[dict] = []
        self.route_assertions: list[dict] = []
        self.profiles: list[dict] = []
        self._size_before_profile: str | None = None
        self._font_scale_before_profile: str | None = None

    def adb(self, *arguments: str, binary: bool = False):
        return subprocess.check_output(
            ["adb", "-s", self.serial, *arguments],
            timeout=30,
            text=not binary,
        )

    def hierarchy(self) -> ET.Element:
        self.adb("shell", "rm", "-f", UI_DUMP_PATH)
        self.adb("shell", "uiautomator", "dump", UI_DUMP_PATH)
        xml = self.adb("shell", "cat", UI_DUMP_PATH)
        return ET.fromstring(xml)

    @staticmethod
    def _node_label(node: ET.Element) -> str:
        return (node.get("text") or node.get("content-desc") or "").strip()

    @staticmethod
    def _bounds(node: ET.Element) -> tuple[int, int, int, int]:
        values = [int(value) for value in re.findall(r"\d+", node.get("bounds") or "")]
        if len(values) != 4:
            raise AssertionError(f"Malformed bounds: {node.get('bounds')!r}")
        return tuple(values)

    def find(self, label: str, root: ET.Element | None = None) -> ET.Element | None:
        tree = root if root is not None else self.hierarchy()
        return next(
            (node for node in tree.iter("node") if self._node_label(node) == label),
            None,
        )

    def find_any(self, labels: tuple[str, ...], root: ET.Element | None = None) -> ET.Element | None:
        tree = root if root is not None else self.hierarchy()
        wanted = set(labels)
        return next(
            (node for node in tree.iter("node") if self._node_label(node) in wanted),
            None,
        )

    def wait_for(self, label: str, timeout: float = WAIT_SECONDS) -> ET.Element:
        deadline = time.monotonic() + timeout
        while time.monotonic() < deadline:
            root = self.hierarchy()
            node = self.find(label, root)
            if node is not None:
                return node
            failure = self.find_any(PIN_FAILURES, root)
            if failure is not None:
                raise AssertionError(f"Widget evidence harness failed closed: {self._node_label(failure)}")
            time.sleep(0.2)
        raise AssertionError(f"Launcher/app did not expose {label!r}")

    def tap_node(self, node: ET.Element) -> None:
        left, top, right, bottom = self._bounds(node)
        if right <= left or bottom <= top:
            raise AssertionError(f"Empty tap bounds: {node.get('bounds')!r}")
        self.adb(
            "shell",
            "input",
            "tap",
            str((left + right) // 2),
            str((top + bottom) // 2),
        )
        time.sleep(0.5)

    def home(self) -> None:
        self.adb("shell", "input", "keyevent", "3")
        time.sleep(0.8)

    def launch_evidence(self, *, command: str, kind: str | None = None, state: str | None = None) -> None:
        args = [
            "shell", "am", "start", "-W", "-n", EVIDENCE_ACTIVITY,
            "--es", "command", command,
        ]
        if kind is not None:
            args += ["--es", "kind", kind]
        if state is not None:
            args += ["--es", "state", state]
        result = self.adb(*args)
        if "Error:" in result or "Exception" in result:
            raise AssertionError(f"Widget evidence activity failed to start: {result.strip()}")

    def request_pin(self, kind: str, expected_label: str) -> None:
        self.home()
        self.launch_evidence(command="pin", kind=kind)
        deadline = time.monotonic() + WAIT_SECONDS
        while time.monotonic() < deadline:
            root = self.hierarchy()
            failure = self.find_any(PIN_FAILURES, root)
            if failure is not None:
                raise AssertionError(f"Widget pin failed closed: {self._node_label(failure)}")
            action = self.find_any(PIN_LABELS, root)
            if action is not None:
                self.tap_node(action)
                self.home()
                self.wait_for(expected_label)
                return
            if self.find(expected_label, root) is not None:
                self.home()
                return
            time.sleep(0.2)
        raise AssertionError(f"Launcher did not offer/complete pinning for {kind}")

    def project_runtime(self, state: str, expected_label: str) -> None:
        self.launch_evidence(command="snapshot", state=state)
        self.home()
        self.wait_for(expected_label)

    def _relevant_nodes(self, root: ET.Element) -> list[dict]:
        normalized: list[dict] = []
        for node in root.iter("node"):
            label = self._node_label(node)
            package = node.get("package") or ""
            if package != PACKAGE and not label:
                continue
            normalized.append(
                {
                    "text": node.get("text") or "",
                    "content_desc": node.get("content-desc") or "",
                    "class": node.get("class") or "",
                    "package": package,
                    "bounds": node.get("bounds") or "",
                    "clickable": node.get("clickable") or "",
                    "enabled": node.get("enabled") or "",
                    "selected": node.get("selected") or "",
                }
            )
        normalized.sort(
            key=lambda row: (
                row["bounds"],
                row["package"],
                row["class"],
                row["text"],
                row["content_desc"],
            )
        )
        return normalized

    @staticmethod
    def _parent_map(root: ET.Element) -> dict[ET.Element, ET.Element]:
        return {child: parent for parent in root.iter() for child in parent}

    def _action_assertion(self, root: ET.Element, label: str) -> dict:
        text_node = self.find(label, root)
        if text_node is None:
            raise AssertionError(f"Widget action label missing: {label}")
        parents = self._parent_map(root)
        owner = text_node
        while owner.get("clickable") != "true" and owner in parents:
            owner = parents[owner]
        if owner.get("clickable") != "true" or owner.get("enabled") != "true":
            raise AssertionError(f"Widget action has no enabled clickable owner: {label}")
        return {
            "label": label,
            "text_bounds": text_node.get("bounds") or "",
            "owner_bounds": owner.get("bounds") or "",
            "owner_class": owner.get("class") or "",
            "clickable": True,
            "enabled": True,
        }

    def capture_bundle(
        self,
        name: str,
        *,
        required_labels: tuple[str, ...],
        action_labels: tuple[str, ...] = (),
    ) -> None:
        root = self.hierarchy()
        for label in required_labels:
            if self.find(label, root) is None:
                raise AssertionError(f"{name}: required text missing: {label}")
        actions = [self._action_assertion(root, label) for label in action_labels]
        screenshot = self.adb("exec-out", "screencap", "-p", binary=True)
        if not screenshot.startswith(b"\x89PNG\r\n\x1a\n"):
            raise AssertionError(f"{name}: launcher did not return a PNG")
        ui = {
            "source_sha": verified_source_sha(None),
            "scenario": name,
            "nodes": self._relevant_nodes(root),
        }
        action_twin = {
            "source_sha": ui["source_sha"],
            "scenario": name,
            "actions": actions,
        }
        png_path = self.output / f"{name}.png"
        ui_path = self.output / f"{name}.ui.json"
        actions_path = self.output / f"{name}.actions.json"
        png_path.write_bytes(screenshot)
        ui_bytes = (json.dumps(ui, indent=2, sort_keys=True) + "\n").encode()
        actions_bytes = (json.dumps(action_twin, indent=2, sort_keys=True) + "\n").encode()
        ui_path.write_bytes(ui_bytes)
        actions_path.write_bytes(actions_bytes)
        self.bundles.append(
            {
                "scenario": name,
                "screenshot": png_path.name,
                "screenshot_sha256": hashlib.sha256(screenshot).hexdigest(),
                "text_twin": ui_path.name,
                "text_twin_sha256": hashlib.sha256(ui_bytes).hexdigest(),
                "assertions": actions_path.name,
                "assertions_sha256": hashlib.sha256(actions_bytes).hexdigest(),
                "visual_verdict": "requires-vision-review",
            }
        )

    def tap_action_and_assert_route(self, action: str, route_label: str, scenario: str) -> None:
        self.home()
        root = self.hierarchy()
        assertion = self._action_assertion(root, action)
        node = self.find(action, root)
        assert node is not None
        self.tap_node(node)
        self.wait_for(route_label)
        self.route_assertions.append(
            {
                "scenario": scenario,
                "action": action,
                "expected_route": route_label,
                "passed": True,
                "owner": assertion,
            }
        )

    def set_display_profile(self, *, target_width_dp: int, font_scale: float) -> None:
        if self._size_before_profile is not None:
            raise AssertionError("Display profile already overridden")
        self._size_before_profile = self.adb("shell", "wm", "size")
        self._font_scale_before_profile = self.adb(
            "shell", "settings", "get", "system", "font_scale"
        ).strip()
        size = self._size_before_profile
        matches = re.findall(r"(\d+)x(\d+)", size)
        if not matches:
            raise AssertionError(f"Unable to parse display size: {size!r}")
        width, height = map(int, matches[-1])
        density_text = self.adb("shell", "wm", "density")
        density_values = re.findall(r"(\d+)", density_text)
        if not density_values:
            raise AssertionError(f"Unable to parse display density: {density_text!r}")
        density = int(density_values[-1])
        target_width = max(1, round(target_width_dp * density / 160))
        target_height = max(target_width, round(height * target_width / width))
        self.adb("shell", "wm", "size", f"{target_width}x{target_height}")
        self.adb("shell", "settings", "put", "system", "font_scale", f"{font_scale:.1f}")
        self.profiles.append(
            {
                "target_width_dp": target_width_dp,
                "font_scale": font_scale,
                "size_px": f"{target_width}x{target_height}",
            }
        )
        time.sleep(1.2)

    def restore_display_profile(self) -> None:
        if self._size_before_profile is None:
            return
        override = re.search(r"Override size:\s*(\d+x\d+)", self._size_before_profile)
        if override:
            self.adb("shell", "wm", "size", override.group(1))
        else:
            self.adb("shell", "wm", "size", "reset")
        font_scale = self._font_scale_before_profile
        self.adb(
            "shell",
            "settings",
            "put",
            "system",
            "font_scale",
            font_scale if font_scale and font_scale != "null" else "1.0",
        )
        self._size_before_profile = None
        self._font_scale_before_profile = None
        time.sleep(1.0)

    def select_theme(self, theme: str) -> None:
        self.adb(
            "shell", "am", "start", "-W",
            "-a", "android.intent.action.MAIN",
            "-c", "android.intent.category.LAUNCHER",
            "-n", "ai.zara.app/.MainActivity",
        )
        self.wait_for("Chat")
        self.tap_node(self.wait_for("Open navigation menu"))
        self.tap_node(self.wait_for("Settings"))
        self.tap_node(self.wait_for("Appearance"))
        self.tap_node(self.wait_for(theme))
        self.home()

    def write_route_assertions(self, source_sha: str) -> dict:
        payload = {
            "source_sha": source_sha,
            "route_assertions": self.route_assertions,
        }
        data = (json.dumps(payload, indent=2, sort_keys=True) + "\n").encode()
        path = self.output / "route.actions.json"
        path.write_bytes(data)
        return {"file": path.name, "sha256": hashlib.sha256(data).hexdigest()}


def exercise(device: Device, source_sha: str) -> dict:
    device.request_pin("assistant", "Symbolic intelligence")
    device.capture_bundle(
        "assistant-outrun",
        required_labels=("Symbolic intelligence", "CHAT", "VOICE", "LOGIC"),
        action_labels=("CHAT", "VOICE", "LOGIC"),
    )

    device.tap_action_and_assert_route("CHAT", "Chat", "route-cold")
    device.tap_action_and_assert_route("CHAT", "Chat", "route-warm")
    device.home()
    device.adb("shell", "am", "kill", "ai.zara.app")
    time.sleep(0.8)
    device.tap_action_and_assert_route("CHAT", "Chat", "route-process-death")

    device.select_theme("Light")
    device.capture_bundle(
        "assistant-light",
        required_labels=("Symbolic intelligence", "CHAT", "VOICE"),
        action_labels=("CHAT", "VOICE"),
    )
    device.select_theme("Outrun")

    device.set_display_profile(target_width_dp=320, font_scale=2.0)
    try:
        device.home()
        device.wait_for("Symbolic intelligence")
        device.capture_bundle(
            "assistant-narrow-200pct",
            required_labels=("Symbolic intelligence", "CHAT"),
            action_labels=("CHAT",),
        )
    finally:
        device.restore_display_profile()

    device.request_pin("runtime", "Runtime")
    device.project_runtime("fresh", "CONNECTED · LOCAL READY")
    device.capture_bundle(
        "runtime-fresh",
        required_labels=("Runtime", "CONNECTED · LOCAL READY", "MODE AUTO", "RUNTIME"),
        action_labels=("RUNTIME",),
    )
    device.project_runtime("stale", "STALE · LOCAL UNKNOWN")
    device.capture_bundle(
        "runtime-stale",
        required_labels=("Runtime", "STALE · LOCAL UNKNOWN", "MODE UNKNOWN"),
        action_labels=("RUNTIME",),
    )
    device.project_runtime("corrupt", "DISCONNECTED · LOCAL UNKNOWN")
    device.capture_bundle(
        "runtime-corrupt",
        required_labels=("Runtime", "DISCONNECTED · LOCAL UNKNOWN", "MODE AUTO"),
        action_labels=("RUNTIME",),
    )
    return device.write_route_assertions(source_sha)


def main() -> None:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--serial", default=os.environ.get("ANDROID_SERIAL"))
    parser.add_argument(
        "--output",
        type=Path,
        default=Path("android/app/build/reports/device/widgets"),
    )
    parser.add_argument("--source-sha")
    args = parser.parse_args()
    if not args.serial:
        parser.error("Select a test emulator explicitly with --serial or ANDROID_SERIAL")
    source_sha = verified_source_sha(args.source_sha)
    args.output.mkdir(parents=True, exist_ok=True)
    device = Device(args.serial, args.output)
    result = {
        "source_sha": source_sha,
        "serial": args.serial,
        "passed": False,
        "evidence": device.bundles,
        "profiles": device.profiles,
        "route_assertions": None,
        "talkback_spoken_traversal": False,
        "visual_verdict": "requires-vision-review",
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
        result["route_assertions"] = exercise(device, source_sha)
        result["passed"] = True
    except BaseException as error:
        result["failure"] = str(error)
        raise
    finally:
        try:
            device.restore_display_profile()
        except Exception as restore_error:
            result["profile_restore_failure"] = str(restore_error)
        result["evidence"] = device.bundles
        manifest = (json.dumps(result, indent=2, sort_keys=True) + "\n").encode()
        (args.output / "manifest.json").write_bytes(manifest)


if __name__ == "__main__":
    main()
