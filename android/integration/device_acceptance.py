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

from PIL import Image


REPO_ROOT = Path(__file__).resolve().parents[2]
SOURCE_SHA_RE = re.compile(r"[0-9a-f]{40}")
UI_DUMP_PATH = "/data/local/tmp/zara-acceptance.xml"
UI_DUMP_ATTEMPTS = 3
UI_DUMP_RETRY_DELAY_SECONDS = 0.2


def verified_source_sha(claimed_source_sha: str | None) -> str:
    actual_source_sha = subprocess.check_output(
        ["git", "rev-parse", "HEAD"],
        cwd=REPO_ROOT,
        text=True,
    ).strip()
    if not SOURCE_SHA_RE.fullmatch(actual_source_sha):
        raise RuntimeError(f"Repository HEAD is not an immutable source SHA: {actual_source_sha!r}")
    if claimed_source_sha is None:
        return actual_source_sha
    if not SOURCE_SHA_RE.fullmatch(claimed_source_sha):
        raise ValueError(f"Claimed source SHA is invalid: {claimed_source_sha!r}")
    if claimed_source_sha != actual_source_sha:
        raise RuntimeError(
            "Claimed source SHA does not match the checked-out repository: "
            f"claimed={claimed_source_sha} actual={actual_source_sha}"
        )
    return actual_source_sha


class Device:
    def __init__(self, serial: str, output: Path) -> None:
        self.serial = serial
        self.output = output
        self.screenshots: list[dict] = []
        self.profiles: list[dict] = []
        self.accessibility_semantics: list[dict] = []
        self.visual_checks: list[dict] = []
        self._size_before_profile: str | None = None
        self._font_scale_before_profile: str | None = None

    def adb(self, *arguments: str, binary: bool = False):
        return subprocess.check_output(
            ["adb", "-s", self.serial, *arguments],
            timeout=30,
            text=not binary,
        )

    def _hierarchy_text(self) -> str:
        last_error: subprocess.CalledProcessError | None = None
        diagnostic = "no uiautomator diagnostic"
        for attempt in range(1, UI_DUMP_ATTEMPTS + 1):
            self.adb("shell", "rm", "-f", UI_DUMP_PATH)
            dump_output = self.adb("shell", "uiautomator", "dump", UI_DUMP_PATH)
            try:
                hierarchy = self.adb("shell", "cat", UI_DUMP_PATH)
            except subprocess.CalledProcessError as error:
                last_error = error
                diagnostic = dump_output.strip() or "no uiautomator diagnostic"
                if attempt < UI_DUMP_ATTEMPTS:
                    time.sleep(UI_DUMP_RETRY_DELAY_SECONDS)
                    continue
                break
            ET.fromstring(hierarchy)
            return hierarchy
        raise AssertionError(
            f"UIAutomator did not create {UI_DUMP_PATH}: {diagnostic}"
        ) from last_error

    def nodes(self):
        # Hosted API-35 emulators can occasionally report a successful dump before
        # the hierarchy file becomes available. Retry only that exact missing-file
        # condition; command failures and malformed XML still fail immediately.
        return ET.fromstring(self._hierarchy_text()).iter("node")

    def find(self, label: str):
        return next(
            (
                node
                for node in self.nodes()
                if label in (node.get("text"), node.get("content-desc"))
            ),
            None,
        )

    def find_contains(self, fragment: str):
        return next(
            (
                node
                for node in self.nodes()
                if any(
                    fragment in (node.get(attribute) or "")
                    for attribute in ("text", "content-desc")
                )
            ),
            None,
        )

    def assert_contains_count(self, fragment: str, *, minimum: int) -> None:
        if minimum < 1:
            raise ValueError("minimum must be at least one")
        matches = [
            node
            for node in self.nodes()
            if any(
                fragment in (node.get(attribute) or "")
                for attribute in ("text", "content-desc")
            )
        ]
        if len(matches) < minimum:
            raise AssertionError(
                f"Expected at least {minimum} UI nodes containing {fragment!r}; "
                f"found {len(matches)}"
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

    def tap_contains(self, fragment: str) -> tuple[int, int, int, int]:
        node = self.find_contains(fragment)
        if node is None:
            raise AssertionError(f"Control is not reachable: {fragment}")
        left, top, right, bottom = self.bounds(node)
        if right <= left or bottom <= top:
            raise AssertionError(f"Control has empty bounds: {fragment}")
        self.adb(
            "shell",
            "input",
            "tap",
            str((left + right) // 2),
            str((top + bottom) // 2),
        )
        return left, top, right, bottom

    def type_text(self, text: str) -> None:
        if not re.fullmatch(r"[A-Za-z0-9_]+", text):
            raise AssertionError("Acceptance text must be adb-input-safe")
        self.adb("shell", "input", "text", text)
        time.sleep(0.4)

    def dismiss_pixel_launcher_anr(self) -> bool:
        # The hosted Pixel emulator can surface a launcher ANR over an otherwise
        # healthy Zara activity. Prefer closing only that OS-owned launcher process
        # so the same hung launcher cannot immediately re-present the dialog. Keep
        # Wait only as a compatibility fallback for platform variants that do not
        # expose Close app. Never hide a Zara crash/ANR or weaken app assertions.
        if self.find_contains("Pixel Launcher isn't responding") is None:
            return False
        action = self.find("Close app")
        if action is None:
            action = self.find("Wait")
        if action is None:
            raise AssertionError("Pixel Launcher ANR did not expose a dismissal action")
        left, top, right, bottom = self.bounds(action)
        self.adb(
            "shell",
            "input",
            "tap",
            str((left + right) // 2),
            str((top + bottom) // 2),
        )
        time.sleep(0.2)
        return True

    def dismiss_release_notes(self, timeout: float = 2.0) -> bool:
        # A fresh install legitimately opens the versioned changelog before Chat.
        # Prefer Zara's exact release-notes title. Hosted Compose can occasionally
        # render that title visually while UIAutomator exposes only the action and
        # changelog-section semantics. Accept that exact fallback pair; unrelated
        # Continue buttons still do not satisfy the release-notes contract.
        release_notes = self.find_contains("What's new in Zara ")
        if release_notes is None:
            release_notes = self.find_contains("What's new in Zara")
        continue_button = None
        if release_notes is None:
            continue_button = self.find("Continue")
            changelog_marker = self.find("Added")
            if changelog_marker is None:
                changelog_marker = self.find("Fixed")
            if continue_button is None or changelog_marker is None:
                return False
        deadline = time.monotonic() + timeout
        while time.monotonic() < deadline:
            if continue_button is None:
                continue_button = self.find("Continue")
            if continue_button is not None:
                left, top, right, bottom = self.bounds(continue_button)
                self.adb(
                    "shell",
                    "input",
                    "tap",
                    str((left + right) // 2),
                    str((top + bottom) // 2),
                )
                time.sleep(0.2)
                return True
            if self.dismiss_pixel_launcher_anr():
                continue_button = None
                continue
            time.sleep(0.1)
        raise AssertionError("Zara release notes did not expose Continue")

    def await_label(self, label: str, timeout: float = 20.0) -> None:
        deadline = time.monotonic() + timeout
        while time.monotonic() < deadline:
            # UIAutomator includes nodes from the activity behind a system ANR
            # dialog. Never accept those background labels as proof that Zara is
            # interactive; clear only the known Pixel Launcher dialog first.
            if self.dismiss_pixel_launcher_anr():
                continue
            if self.find(label) is not None:
                return
            if self.dismiss_release_notes():
                continue
            time.sleep(0.2)
        raise AssertionError(f"Screen did not show {label}")

    def await_contains(self, fragment: str, timeout: float = 20.0) -> None:
        deadline = time.monotonic() + timeout
        while time.monotonic() < deadline:
            if self.dismiss_pixel_launcher_anr():
                continue
            if self.find_contains(fragment) is not None:
                return
            time.sleep(0.2)
        raise AssertionError(f"Screen did not retain text containing {fragment}")

    def assert_accessible_targets(self, labels: tuple[str, ...]) -> None:
        for label in labels:
            node = self.find(label)
            if node is None:
                raise AssertionError(f"Accessibility target is missing: {label}")
            left, top, right, bottom = self.bounds(node)
            if right <= left or bottom <= top:
                raise AssertionError(f"Accessibility target has empty bounds: {label}")
            self.accessibility_semantics.append(
                {
                    "label": label,
                    "class": node.get("class"),
                    "bounds": node.get("bounds"),
                    "clickable": node.get("clickable"),
                    "enabled": node.get("enabled"),
                }
            )

    def capture_text_twin(self, name: str) -> dict:
        hierarchy = self._hierarchy_text()
        data = hierarchy.encode("utf-8")
        path = self.output / f"{name}.xml"
        path.write_bytes(data)
        return {
            "file": path.name,
            "sha256": hashlib.sha256(data).hexdigest(),
        }

    def assert_transient_surface_visible(
        self,
        *,
        trigger_fragment: str,
        action_labels: tuple[str, ...],
        screenshot_name: str,
        trigger_bounds: tuple[int, int, int, int] | None = None,
    ) -> None:
        if trigger_bounds is None:
            trigger = self.find_contains(trigger_fragment)
            if trigger is None:
                raise AssertionError(f"Transient-surface trigger is missing: {trigger_fragment}")
            trigger_bounds = self.bounds(trigger)
        if trigger_bounds[2] <= trigger_bounds[0] or trigger_bounds[3] <= trigger_bounds[1]:
            raise AssertionError(f"Transient-surface trigger has empty bounds: {trigger_bounds}")

        action_nodes = []
        for label in action_labels:
            node = self.find(label)
            if node is None:
                raise AssertionError(f"Transient-surface action is missing: {label}")
            action_nodes.append((label, node))

        path = self.capture(screenshot_name)
        screenshot_sha256 = hashlib.sha256(path.read_bytes()).hexdigest()
        text_twin = self.capture_text_twin(screenshot_name)
        with Image.open(path) as opened:
            image = opened.convert("RGB")
        viewport_width, viewport_height = image.size

        checks: list[dict] = []
        lefts: list[int] = []
        tops: list[int] = []
        rights: list[int] = []
        bottoms: list[int] = []

        for label, node in action_nodes:
            left, top, right, bottom = self.bounds(node)
            if not (0 <= left < right <= viewport_width and 0 <= top < bottom <= viewport_height):
                raise AssertionError(
                    f"Transient-surface action escaped viewport: {label} {node.attrib.get('bounds')}"
                )
            lefts.append(left)
            tops.append(top)
            rights.append(right)
            bottoms.append(bottom)

            crop = image.crop((left, top, right, bottom)).convert("L")
            crop_width, crop_height = crop.size
            edge_inset = max(1, min(6, crop_width // 10, crop_height // 10))
            if crop_width <= edge_inset * 2 or crop_height <= edge_inset * 2:
                raise AssertionError(
                    "Transient-surface action is too small for interior visual evidence: "
                    f"{label} size={crop_width}x{crop_height} inset={edge_inset}"
                )
            content_crop = crop.crop(
                (edge_inset, edge_inset, crop_width - edge_inset, crop_height - edge_inset)
            )
            low, high = content_crop.getextrema()
            occupied_bins = sum(1 for count in content_crop.histogram() if count)
            luma_span = int(high) - int(low)
            if luma_span < 18 or occupied_bins < 4:
                raise AssertionError(
                    "Transient-surface action is semantically present but visually blank: "
                    f"{label} luma_span={luma_span} occupied_bins={occupied_bins}"
                )
            checks.append(
                {
                    "label": label,
                    "bounds": node.attrib.get("bounds"),
                    "content_inset_px": edge_inset,
                    "luma_span": luma_span,
                    "occupied_luma_bins": occupied_bins,
                }
            )

        union = (
            min(lefts),
            min(tops),
            max(rights),
            max(bottoms),
        )
        union_width = union[2] - union[0]
        union_height = union[3] - union[1]
        if union_width > viewport_width * 0.65 or union_height > viewport_height * 0.45:
            raise AssertionError(
                "Transient menu occupies implausibly large viewport area: "
                f"union={union} viewport={viewport_width}x{viewport_height}"
            )

        trigger_center = (
            (trigger_bounds[0] + trigger_bounds[2]) // 2,
            (trigger_bounds[1] + trigger_bounds[3]) // 2,
        )
        nearest_x = min(max(trigger_center[0], union[0]), union[2])
        nearest_y = min(max(trigger_center[1], union[1]), union[3])
        distance = abs(trigger_center[0] - nearest_x) + abs(trigger_center[1] - nearest_y)
        if distance > max(viewport_width, viewport_height) * 0.35:
            raise AssertionError(
                "Transient menu is not anchored near its trigger: "
                f"trigger={trigger_bounds} union={union} distance={distance}"
            )

        self.visual_checks.append(
            {
                "state": screenshot_name,
                "trigger": trigger_fragment,
                "trigger_bounds": list(trigger_bounds),
                "action_union": union,
                "viewport": [viewport_width, viewport_height],
                "screenshot_file": path.name,
                "screenshot_sha256": screenshot_sha256,
                "text_twin_file": text_twin["file"],
                "text_twin_sha256": text_twin["sha256"],
                "source_sha": getattr(self, "source_sha", None),
                "device_api": getattr(self, "device_api", None),
                "profile": getattr(self, "current_profile", "default"),
                "actions": checks,
            }
        )

    def capture(self, name: str) -> Path:
        data = self.adb("exec-out", "screencap", "-p", binary=True)
        if not data.startswith(b"\x89PNG\r\n\x1a\n"):
            raise AssertionError("Device did not produce a PNG screenshot")
        path = self.output / f"{name}.png"
        path.write_bytes(data)
        self.screenshots.append(
            {
                "state": name,
                "file": path.name,
                "sha256": hashlib.sha256(data).hexdigest(),
            }
        )
        return path

    def launch_surface(self, component: str, label: str) -> None:
        self.adb(
            "shell",
            "am",
            "start",
            "-W",
            "-a",
            "android.intent.action.MAIN",
            "-c",
            "android.intent.category.LAUNCHER",
            "-f",
            "0x10200000",
            "-n",
            component,
        )
        self.dismiss_pixel_launcher_anr()
        self.dismiss_release_notes()
        self.await_label(label)

    def assert_launcher_task_isolation(self) -> None:
        sequence = (
            ("ai.zara.app/.automation.AutomationActivity", "Prolog Automation", "launcher-automation"),
            ("ai.zara.app/.watch.WatchSetupActivity", "ZARA WATCH SETUP", "launcher-watch-setup"),
            ("ai.zara.app/.automation.AutomationActivity", "Prolog Automation", None),
            ("ai.zara.app/.MainActivity", "Chat", "launcher-main-return"),
        )
        for component, label, screenshot in sequence:
            self.launch_surface(component, label)
            if screenshot is not None:
                self.capture(screenshot)

    def start(self) -> None:
        self.adb("shell", "am", "force-stop", "ai.zara.app")
        self.launch_surface("ai.zara.app/.MainActivity", "Chat")

    def press_back(self) -> None:
        self.adb("shell", "input", "keyevent", "4")
        time.sleep(0.5)

    def recreate(self) -> None:
        # HOME + am kill keeps the Android task/saved-state path while killing the app
        # process. This is stronger than a same-process Compose recreation and remains
        # distinct from a force-stop/new-task smoke.
        self.adb("shell", "input", "keyevent", "3")
        time.sleep(0.5)
        self.adb("shell", "am", "kill", "ai.zara.app")
        time.sleep(0.8)
        self.adb(
            "shell",
            "am",
            "start",
            "-W",
            "-a",
            "android.intent.action.MAIN",
            "-c",
            "android.intent.category.LAUNCHER",
            "-f",
            "0x10200000",
            "-n",
            "ai.zara.app/.MainActivity",
        )
        time.sleep(0.8)

    def set_display_profile(
        self,
        name: str,
        *,
        target_width_dp: int,
        font_scale: float,
    ) -> None:
        if self._size_before_profile is not None:
            raise AssertionError("Display profile already overridden")
        self._size_before_profile = self.adb("shell", "wm", "size")
        self._font_scale_before_profile = self.adb(
            "shell", "settings", "get", "system", "font_scale"
        ).strip()
        width, height = self.size()
        density = self.density()
        target_width = max(1, round(target_width_dp * density / 160))
        target_height = max(target_width, round(height * target_width / width))
        font_scale_text = f"{font_scale:.2f}"
        self.adb("shell", "wm", "size", f"{target_width}x{target_height}")
        self.adb(
            "shell", "settings", "put", "system", "font_scale", font_scale_text
        )
        self.current_profile = name
        self.profiles.append(
            {
                "name": name,
                "size_px": f"{target_width}x{target_height}",
                "target_width_dp": target_width_dp,
                "font_scale": font_scale_text,
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
            self.adb(
                "shell", "settings", "put", "system", "font_scale", original_font
            )
        else:
            self.adb("shell", "settings", "put", "system", "font_scale", "1.0")
        self._size_before_profile = None
        self._font_scale_before_profile = None
        self.current_profile = "default"
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
    device.assert_launcher_task_isolation()
    device.capture("empty-shell")
    device.assert_accessible_targets(("Open navigation menu", "Chat", "Voice"))

    device.tap("Open navigation menu")
    for menu in ("Chat", "Workspace", "Settings"):
        device.await_label(menu)
    device.assert_accessible_targets(("Chat", "Workspace", "Settings"))

    # Exercise the real New chat UI twice so the overflow catcher cannot pass on
    # a synthetic/single-row drawer. Each tap crosses MainActivity's canonical
    # ConversationStore path, then we reopen the drawer and prove multiple action
    # triggers exist before capturing any overflow evidence.
    device.tap_contains("New chat")
    device.await_label("Chat")
    device.tap("Open navigation menu")
    device.await_contains("New chat")
    device.tap_contains("New chat")
    device.await_label("Chat")
    device.tap("Open navigation menu")
    for menu in ("Chat", "Workspace", "Settings"):
        device.await_label(menu)
    device.assert_contains_count("Actions for ", minimum=2)
    device.capture("drawer-open")

    device.await_contains("Actions for ")
    trigger_bounds = device.tap_contains("Actions for ")
    device.await_label("Rename")
    device.await_label("Move to project")
    pin_label = "Pin" if device.find("Pin") is not None else "Unpin"
    if device.find(pin_label) is None:
        raise AssertionError("Conversation overflow did not expose Pin/Unpin")
    overflow_actions = (pin_label, "Rename", "Move to project")
    device.assert_accessible_targets(overflow_actions)
    device.assert_transient_surface_visible(
        trigger_fragment="Actions for ",
        trigger_bounds=trigger_bounds,
        action_labels=overflow_actions,
        screenshot_name="drawer-conversation-overflow",
    )
    device.press_back()

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
        device.assert_accessible_targets((tab,))
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
    device.set_display_profile(
        "wide-navigation-rail", target_width_dp=700, font_scale=1.0
    )
    device.await_label("Workspace")
    device.assert_accessible_targets(("Chat", "Workspace", "Settings"))
    device.capture("wide-navigation-rail")
    device.restore_profile()
    device.await_label("Chat")

    device.set_display_profile(
        "narrow-large-font", target_width_dp=320, font_scale=1.30
    )
    device.await_label("Chat")
    device.capture("narrow-large-font")
    device.restore_profile()
    device.await_label("Chat")

    # Keep a draft only; never submit it. The local chat path must become usable for
    # this lifecycle acceptance or the screenshot gate fails honestly.
    device.await_label("Ask anything…", timeout=30.0)
    device.tap("Ask anything…")
    device.type_text("ui_draft_939")
    device.capture("ime-composer")
    device.press_back()
    device.recreate()
    device.await_contains("ui_draft_939")
    device.capture("recreated-chat-draft")

    open_menu(device, "Settings")
    device.tap_tab("Connection")
    device.await_label("tcp://host:port")
    device.tap("tcp://host:port")
    device.type_text("ui_connection_draft")
    device.press_back()
    device.recreate()
    device.await_contains("ui_connection_draft")
    device.capture("recreated-connection-draft")

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
    parser.add_argument("--source-sha")
    args = parser.parse_args()
    if not args.serial:
        parser.error("Select a test emulator explicitly with --serial or ANDROID_SERIAL")
    source_sha = verified_source_sha(args.source_sha)
    args.output.mkdir(parents=True, exist_ok=True)
    device = Device(args.serial, args.output)
    device.source_sha = source_sha
    device.current_profile = "default"
    result = {
        "source_sha": source_sha,
        "serial": args.serial,
        "passed": False,
        "screenshots": device.screenshots,
        "profiles": device.profiles,
        "accessibility_semantics": device.accessibility_semantics,
        "visual_checks": device.visual_checks,
        # UIAutomator semantics are useful accessibility evidence, but they are not
        # proof of real TalkBack spoken traversal. Keep that hardware/service claim false.
        "talkback_spoken_traversal": False,
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
        device.device_api = result["device"]["api"]
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