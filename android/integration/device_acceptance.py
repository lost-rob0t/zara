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


REPO_ROOT = Path(__file__).resolve().parents[2]
SOURCE_SHA_RE = re.compile(r"[0-9a-f]{40}")
UI_DUMP_PATH = "/data/local/tmp/zara-acceptance.xml"
UI_DUMP_ATTEMPTS = 3
UI_DUMP_RETRY_DELAY_SECONDS = 0.2
SYSTEM_ANR_DIALOGS = (
    (
        "com.google.android.apps.nexuslauncher",
        "Pixel Launcher isn't responding",
    ),
    (
        "com.google.android.googlesdksetup",
        "com.google.android.googlesdksetup isn't responding",
    ),
)
SYSTEM_ANR_ACTIONS = {
    "Close app": "android:id/aerr_close",
    "Wait": "android:id/aerr_wait",
}
ANR_HIERARCHY_PATH_ATTR = "zara-anr-hierarchy-path"
SYSTEM_ANR_DISMISSAL_LIMIT = 2
SYSTEM_ANR_CLEAR_ATTEMPTS = 3
SYSTEM_ANR_CLEAR_RETRY_DELAY_SECONDS = 0.1


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
        self.system_anr_sanitation: list[dict] = []
        self._size_before_profile: str | None = None
        self._font_scale_before_profile: str | None = None

    def adb(self, *arguments: str, binary: bool = False):
        return subprocess.check_output(
            ["adb", "-s", self.serial, *arguments],
            timeout=30,
            text=not binary,
        )

    def nodes(self):
        # Hosted API-35 emulators can occasionally report a successful dump before
        # the hierarchy file becomes available. Retry only that exact missing-file
        # condition; command failures and malformed XML still fail immediately.
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
            root = ET.fromstring(hierarchy)
            return iter(self.annotate_hierarchy(root))
        raise AssertionError(
            f"UIAutomator did not create {UI_DUMP_PATH}: {diagnostic}"
        ) from last_error

    def find(self, label: str):
        matches = [
            node
            for node in self.nodes()
            if label in (node.get("text"), node.get("content-desc"))
        ]
        expected_resource_id = SYSTEM_ANR_ACTIONS.get(label)
        if expected_resource_id is not None:
            return next(
                (
                    node
                    for node in matches
                    if node.get("package") == "android"
                    and node.get("resource-id") == expected_resource_id
                ),
                None,
            )
        return matches[0] if matches else None

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

    @staticmethod
    def node_label(node) -> str:
        return (
            node.get("text")
            or node.get("content-desc")
            or "<unknown ANR dialog>"
        )

    @staticmethod
    def annotate_hierarchy(root) -> list:
        nodes: list = []

        def visit(element, path: tuple[int, ...]) -> None:
            if element.tag == "node":
                element.set(
                    ANR_HIERARCHY_PATH_ATTR,
                    ".".join(str(index) for index in path),
                )
                nodes.append(element)
            for index, child in enumerate(list(element)):
                visit(child, path + (index,))

        visit(root, ())
        return nodes

    @staticmethod
    def hierarchy_path(node) -> tuple[int, ...] | None:
        raw_path = node.get(ANR_HIERARCHY_PATH_ATTR)
        if raw_path is None:
            return None
        if not raw_path:
            return ()
        return tuple(int(index) for index in raw_path.split("."))

    @staticmethod
    def common_path_depth(left: tuple[int, ...], right: tuple[int, ...]) -> int:
        depth = 0
        for left_index, right_index in zip(left, right):
            if left_index != right_index:
                break
            depth += 1
        return depth

    @classmethod
    def dialog_owner_boundary(cls, nodes, owner_path: tuple[int, ...]):
        node_by_path = {
            path: node
            for node in nodes
            for path in (cls.hierarchy_path(node),)
            if path is not None
        }
        owner = node_by_path.get(owner_path)
        parent = node_by_path.get(owner_path[:-1])
        if owner is None or parent is None:
            return None
        if not (owner.get("resource-id") or "").strip():
            return None
        try:
            owner_bounds = cls.bounds(owner)
            parent_bounds = cls.bounds(parent)
        except (AssertionError, KeyError):
            return None
        owner_left, owner_top, owner_right, owner_bottom = owner_bounds
        parent_left, parent_top, parent_right, parent_bottom = parent_bounds
        if not (
            parent_left <= owner_left < owner_right <= parent_right
            and parent_top <= owner_top < owner_bottom <= parent_bottom
        ):
            return None
        if owner_bounds == parent_bounds:
            return None
        return owner

    def exact_anr_is_present(self, package: str, dialog_text: str) -> bool:
        return any(
            self.node_label(node) == dialog_text and node.get("package") == package
            for node in self.nodes()
        )

    def anr_snapshot(self):
        nodes = list(self.nodes())
        candidates = [
            node
            for node in nodes
            if "isn't responding" in self.node_label(node)
        ]
        if not candidates:
            return None, nodes
        unexpected = [
            node
            for node in candidates
            if (node.get("package"), self.node_label(node)) not in SYSTEM_ANR_DIALOGS
        ]
        return (unexpected[0] if unexpected else candidates[0]), nodes

    def bound_anr_action(self, nodes, selected_anr, label: str):
        expected_resource_id = SYSTEM_ANR_ACTIONS[label]
        matches = [
            node
            for node in nodes
            if label in (node.get("text"), node.get("content-desc"))
            and node.get("package") == "android"
            and node.get("resource-id") == expected_resource_id
        ]
        if not matches:
            return None

        anr_candidates = [
            node
            for node in nodes
            if "isn't responding" in self.node_label(node)
        ]
        selected_path = self.hierarchy_path(selected_anr)
        if selected_path is None or not selected_path:
            raise AssertionError(f"ANR action ownership is not provable: {label}")
        candidate_paths = {
            id(candidate): self.hierarchy_path(candidate)
            for candidate in anr_candidates
        }
        if any(path is None or not path for path in candidate_paths.values()):
            raise AssertionError(f"ANR action ownership is not provable: {label}")

        scored: list[tuple[int, object]] = []
        for action in matches:
            action_path = self.hierarchy_path(action)
            if action_path is None or not action_path:
                raise AssertionError(f"ANR action ownership is not provable: {label}")

            # The hierarchy root and its top-level window are not dialog ownership
            # evidence. Require a deeper structural boundary that uniquely contains
            # both the selected ANR and the candidate Android system action.
            owner_depth = self.common_path_depth(selected_path, action_path)
            if owner_depth < 2:
                continue
            owner_path = selected_path[:owner_depth]
            if self.dialog_owner_boundary(nodes, owner_path) is None:
                continue
            owned_anrs = [
                candidate
                for candidate in anr_candidates
                for path in (candidate_paths[id(candidate)],)
                if path is not None and path[:owner_depth] == owner_path
            ]
            if len(owned_anrs) != 1 or owned_anrs[0] is not selected_anr:
                continue

            competing_depth = max(
                (
                    self.common_path_depth(path, action_path)
                    for candidate in anr_candidates
                    if candidate is not selected_anr
                    for path in (candidate_paths[id(candidate)],)
                    if path is not None
                ),
                default=-1,
            )
            if owner_depth > competing_depth:
                scored.append((owner_depth, action))

        if not scored:
            return None
        best_depth = max(depth for depth, _action in scored)
        best_matches = [
            action for depth, action in scored if depth == best_depth
        ]
        if len(best_matches) != 1:
            raise AssertionError(f"Ambiguous system ANR action owner: {label}")
        return best_matches[0]

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

    def type_text(self, text: str) -> None:
        if not re.fullmatch(r"[A-Za-z0-9_]+", text):
            raise AssertionError("Acceptance text must be adb-input-safe")
        self.adb("shell", "input", "text", text)
        time.sleep(0.4)

    def dismiss_pixel_launcher_anr(self) -> bool:
        anr, snapshot = self.anr_snapshot()
        if anr is None:
            return False
        dialog_text = self.node_label(anr)
        package = anr.get("package")
        if (package, dialog_text) not in SYSTEM_ANR_DIALOGS:
            self.system_anr_sanitation.append(
                {
                    "package": package,
                    "dialog": dialog_text,
                    "action": None,
                    "cleared": False,
                }
            )
            raise AssertionError(f"Unexpected ANR dialog blocks acceptance: {dialog_text}")

        prior_attempts = sum(
            1
            for receipt in self.system_anr_sanitation
            if receipt["package"] == package
            and receipt["dialog"] == dialog_text
            and receipt["action"] is not None
        )
        while prior_attempts < SYSTEM_ANR_DISMISSAL_LIMIT:
            action_label = "Close app"
            action = self.bound_anr_action(snapshot, anr, action_label)
            if action is None:
                action_label = "Wait"
                action = self.bound_anr_action(snapshot, anr, action_label)
            receipt = {
                "package": package,
                "dialog": dialog_text,
                "action": action_label if action is not None else None,
                "cleared": False,
            }
            self.system_anr_sanitation.append(receipt)
            if action is None:
                raise AssertionError(
                    f"Known system ANR did not expose a dismissal action: {dialog_text}"
                )
            left, top, right, bottom = self.bounds(action)
            self.adb(
                "shell",
                "input",
                "tap",
                str((left + right) // 2),
                str((top + bottom) // 2),
            )
            for clear_attempt in range(SYSTEM_ANR_CLEAR_ATTEMPTS):
                if not self.exact_anr_is_present(package, dialog_text):
                    receipt["cleared"] = True
                    return True
                if clear_attempt + 1 < SYSTEM_ANR_CLEAR_ATTEMPTS:
                    time.sleep(SYSTEM_ANR_CLEAR_RETRY_DELAY_SECONDS)
            prior_attempts += 1
            if prior_attempts < SYSTEM_ANR_DISMISSAL_LIMIT:
                next_anr, snapshot = self.anr_snapshot()
                if next_anr is None:
                    receipt["cleared"] = True
                    return True
                if (
                    next_anr.get("package") != package
                    or self.node_label(next_anr) != dialog_text
                ):
                    raise AssertionError(
                        f"System ANR changed during sanitation: {dialog_text}"
                    )
                anr = next_anr

        raise AssertionError(
            f"System ANR sanitation limit exceeded for {dialog_text}"
        )

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
            # interactive; clear only explicitly allowlisted system dialogs first.
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

    def capture(self, name: str) -> None:
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
    result = {
        "source_sha": source_sha,
        "serial": args.serial,
        "passed": False,
        "screenshots": device.screenshots,
        "profiles": device.profiles,
        "accessibility_semantics": device.accessibility_semantics,
        "system_anr_sanitation": device.system_anr_sanitation,
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
