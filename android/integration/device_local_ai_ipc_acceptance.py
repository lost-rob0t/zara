#!/usr/bin/env python3
from __future__ import annotations

import argparse
from enum import Enum
import hashlib
import json
from pathlib import Path
import re
import subprocess
import sys
import time
import xml.etree.ElementTree as ET

UI_DUMP = "/data/local/tmp/zara-local-ai-ipc.xml"
UI_DUMP_ATTEMPTS = 3
UI_DUMP_RETRY_DELAY_SECONDS = 0.2
PERMISSION = "ai.zara.app.permission.LOCAL_AI"
HOST_PACKAGE = "ai.zara.app"
HOST_PROCESS = "ai.zara.app:voice"
NORMAL_PACKAGE = "ai.zara.llmserve"
ADVERSARY_PACKAGE = "ai.zara.llmserve.adversary"
ACTIVITY_CLASS = "ai.zara.llmserve.MainActivity"


def sha256(path: Path) -> str:
    digest = hashlib.sha256()
    with path.open("rb") as stream:
        for chunk in iter(lambda: stream.read(1024 * 1024), b""):
            digest.update(chunk)
    return digest.hexdigest()


class PermissionState(str, Enum):
    GRANTED = "granted"
    DENIED = "denied"
    QUERY_ERROR = "query_error"


def parse_install_permission_state(
    package: str,
    dump: str,
) -> tuple[PermissionState, str]:
    if f"Package [{package}]" not in dump:
        return PermissionState.QUERY_ERROR, "package stanza missing from dumpsys"

    requested = re.search(
        rf"(?m)^\s+{re.escape(PERMISSION)}\s*$",
        dump,
    )
    if requested is None:
        return PermissionState.QUERY_ERROR, "permission missing from requested permissions"

    grant = re.search(
        rf"(?m)^\s+{re.escape(PERMISSION)}:\s+granted=(true|false)\b",
        dump,
    )
    if grant is None:
        # Signature permissions are install-time permissions. Android's package
        # dump lists granted install permissions explicitly; a requested
        # install-time permission absent from the grant list is denied.
        return PermissionState.DENIED, "requested install permission absent from grant list"

    if grant.group(1) == "true":
        return PermissionState.GRANTED, "install permission granted"
    return PermissionState.DENIED, "install permission explicitly denied"


class Device:
    def __init__(self, serial: str) -> None:
        self.serial = serial

    def adb(self, *args: str) -> str:
        return subprocess.check_output(
            ["adb", "-s", self.serial, *args],
            text=True,
            timeout=30,
        ).strip()

    def install(self, apk: Path) -> None:
        out = self.adb("install", "-r", str(apk))
        if "Success" not in out:
            raise AssertionError(f"APK install failed: {apk}: {out}")

    def uninstall(self, package: str) -> None:
        subprocess.run(
            ["adb", "-s", self.serial, "uninstall", package],
            text=True,
            stdout=subprocess.DEVNULL,
            stderr=subprocess.DEVNULL,
            check=False,
            timeout=30,
        )

    def launch(self, package: str) -> None:
        result = self.adb(
            "shell", "am", "start", "-W", "-n", f"{package}/{ACTIVITY_CLASS}"
        )
        if "Status: ok" not in result:
            raise AssertionError(f"Failed to launch {package}: {result}")

    def force_stop(self, package: str) -> None:
        self.adb("shell", "am", "force-stop", package)

    def nodes(self):
        # Hosted emulators can report a successful UIAutomator dump before the
        # hierarchy file is visible to the following shell command. Retry only
        # that missing-file read; dump failures and malformed XML still fail
        # immediately so acceptance cannot turn an app failure into a green.
        last_error: subprocess.CalledProcessError | None = None
        diagnostic = "no uiautomator diagnostic"
        for attempt in range(1, UI_DUMP_ATTEMPTS + 1):
            self.adb("shell", "rm", "-f", UI_DUMP)
            dump_output = self.adb("shell", "uiautomator", "dump", UI_DUMP)
            try:
                raw = self.adb("shell", "cat", UI_DUMP)
            except subprocess.CalledProcessError as error:
                last_error = error
                diagnostic = dump_output.strip() or "no uiautomator diagnostic"
                if attempt < UI_DUMP_ATTEMPTS:
                    time.sleep(UI_DUMP_RETRY_DELAY_SECONDS)
                    continue
                break
            return ET.fromstring(raw).iter("node")
        raise AssertionError(
            f"UIAutomator did not create {UI_DUMP}: {diagnostic}"
        ) from last_error

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
                if fragment in (node.get("text") or "")
                or fragment in (node.get("content-desc") or "")
            ),
            None,
        )

    @staticmethod
    def bounds(node) -> tuple[int, int, int, int]:
        values = [int(v) for v in re.findall(r"\d+", node.attrib.get("bounds", ""))]
        if len(values) != 4:
            raise AssertionError(f"Malformed bounds: {node.attrib.get('bounds')}")
        return tuple(values)

    def tap(self, label: str) -> None:
        node = self.find(label)
        if node is None:
            raise AssertionError(f"Missing control: {label}")
        left, top, right, bottom = self.bounds(node)
        self.adb(
            "shell", "input", "tap", str((left + right) // 2), str((top + bottom) // 2)
        )

    def refresh_until(self, fragment: str, timeout: float = 15.0) -> str:
        deadline = time.monotonic() + timeout
        last_text = ""
        while time.monotonic() < deadline:
            self.tap("Refresh status")
            time.sleep(0.15)
            node = self.find_contains(fragment)
            if node is not None:
                return node.get("text") or node.get("content-desc") or fragment
            texts = [node.get("text") or "" for node in self.nodes()]
            last_text = " | ".join(text for text in texts if text)
            time.sleep(0.2)
        raise AssertionError(
            f"UI did not show refreshed status containing {fragment!r}; last={last_text!r}"
        )

    def permission(self, package: str) -> tuple[PermissionState, str]:
        result = subprocess.run(
            ["adb", "-s", self.serial, "shell", "dumpsys", "package", package],
            text=True,
            capture_output=True,
            timeout=30,
            check=False,
        )
        if result.returncode != 0:
            detail = (result.stderr or result.stdout).strip()[:256]
            return (
                PermissionState.QUERY_ERROR,
                f"dumpsys exited {result.returncode}: {detail or 'no output'}",
            )
        if result.stderr.strip():
            return (
                PermissionState.QUERY_ERROR,
                f"dumpsys stderr: {result.stderr.strip()[:256]}",
            )
        return parse_install_permission_state(package, result.stdout)

    def pid(self) -> str | None:
        result = subprocess.run(
            ["adb", "-s", self.serial, "shell", "pidof", HOST_PROCESS],
            text=True,
            capture_output=True,
            timeout=30,
            check=False,
        )
        value = result.stdout.strip()
        return value or None

    def kill_owner_process(self, pid: str) -> None:
        self.adb("shell", "run-as", HOST_PACKAGE, "kill", "-9", pid)

    def wait_pid_gone(self, old_pid: str, timeout: float = 10.0) -> None:
        deadline = time.monotonic() + timeout
        while time.monotonic() < deadline:
            current = self.pid()
            if current is None or current != old_pid:
                return
            time.sleep(0.2)
        raise AssertionError(f"Canonical owner process did not terminate: {old_pid}")

    def wait_new_pid(self, old_pid: str, timeout: float = 15.0) -> str:
        deadline = time.monotonic() + timeout
        while time.monotonic() < deadline:
            current = self.pid()
            if current and current != old_pid:
                return current
            time.sleep(0.2)
        raise AssertionError("Canonical owner process did not recreate with a new pid")

    def capture(self, path: Path) -> str:
        data = subprocess.check_output(
            ["adb", "-s", self.serial, "exec-out", "screencap", "-p"], timeout=30
        )
        if not data.startswith(b"\x89PNG\r\n\x1a\n"):
            raise AssertionError("Device screenshot is not PNG")
        path.write_bytes(data)
        return hashlib.sha256(data).hexdigest()


def main() -> None:
    parser = argparse.ArgumentParser()
    parser.add_argument("--serial", required=True)
    parser.add_argument("--source-sha", required=True)
    parser.add_argument("--phone-apk", type=Path, required=True)
    parser.add_argument("--llm-serve-apk", type=Path, required=True)
    parser.add_argument("--adversary-apk", type=Path, required=True)
    parser.add_argument("--output", type=Path, required=True)
    args = parser.parse_args()

    if not re.fullmatch(r"[0-9a-f]{40}", args.source_sha):
        raise SystemExit("source SHA must be immutable lowercase git SHA")
    for apk in (args.phone_apk, args.llm_serve_apk, args.adversary_apk):
        if not apk.is_file():
            raise SystemExit(f"missing APK: {apk}")

    args.output.mkdir(parents=True, exist_ok=True)
    device = Device(args.serial)
    device.adb("wait-for-device")

    evidence: dict[str, object] = {
        "schema": 1,
        "source_sha": args.source_sha,
        "phone_apk_sha256": sha256(args.phone_apk),
        "llm_serve_apk_sha256": sha256(args.llm_serve_apk),
        "adversary_apk_sha256": sha256(args.adversary_apk),
        "wrong_signer_denied": False,
        "signed_client_reached_owner": False,
        "owner_process_recreated": False,
        "passed": False,
        "stage": "setup",
        "screenshots": [],
    }
    manifest = args.output / "local-ai-ipc-manifest.json"
    previous_excepthook = sys.excepthook

    def persist_failure(exc_type, exc_value, exc_tb) -> None:
        evidence["passed"] = False
        evidence["failure"] = {
            "type": exc_type.__name__[:96],
            "message": str(exc_value)[:512],
        }
        try:
            device.adb("shell", "rm", "-f", UI_DUMP)
            device.adb("shell", "uiautomator", "dump", UI_DUMP)
            raw = device.adb("shell", "cat", UI_DUMP)
            ui_dump = args.output / "local-ai-ipc-failure-ui.xml"
            ui_dump.write_text(raw, encoding="utf-8")
            evidence["failure_ui_dump"] = ui_dump.name
        except Exception as capture_error:
            evidence["failure_ui_dump_error"] = str(capture_error)[:256]
        try:
            screenshot = args.output / "local-ai-ipc-failure.png"
            digest = device.capture(screenshot)
            evidence["failure_screenshot"] = {
                "file": screenshot.name,
                "sha256": digest,
            }
        except Exception as capture_error:
            evidence["failure_screenshot_error"] = str(capture_error)[:256]
        manifest.write_text(
            json.dumps(evidence, indent=2, sort_keys=True) + "\n",
            encoding="utf-8",
        )
        previous_excepthook(exc_type, exc_value, exc_tb)

    sys.excepthook = persist_failure

    evidence["stage"] = "wrong_signer"
    device.uninstall(ADVERSARY_PACKAGE)
    device.install(args.adversary_apk)
    try:
        permission, permission_detail = device.permission(ADVERSARY_PACKAGE)
        evidence["adversary_permission"] = permission.value
        evidence["adversary_permission_query"] = permission_detail
        if permission is PermissionState.QUERY_ERROR:
            raise AssertionError(
                f"Adversary permission query failed: {permission_detail}"
            )
        if permission is not PermissionState.DENIED:
            raise AssertionError(
                f"Adversary unexpectedly has signature permission: {permission.value}"
            )
        device.launch(ADVERSARY_PACKAGE)
        device.tap("Start server")
        status = device.refresh_until("permission denied")
        evidence["adversary_status"] = status
        evidence["wrong_signer_denied"] = True
        screenshot = args.output / "local-ai-ipc-wrong-signer.png"
        digest = device.capture(screenshot)
        evidence["screenshots"].append({"file": screenshot.name, "sha256": digest})
    finally:
        device.force_stop(ADVERSARY_PACKAGE)
        device.uninstall(ADVERSARY_PACKAGE)

    evidence["stage"] = "same_lineage"
    device.install(args.llm_serve_apk)
    device.force_stop(NORMAL_PACKAGE)
    device.launch(NORMAL_PACKAGE)
    normal_permission, normal_permission_detail = device.permission(NORMAL_PACKAGE)
    evidence["signed_client_permission"] = normal_permission.value
    evidence["signed_client_permission_query"] = normal_permission_detail
    if normal_permission is PermissionState.QUERY_ERROR:
        raise AssertionError(
            f"Same-lineage permission query failed: {normal_permission_detail}"
        )
    if normal_permission is not PermissionState.GRANTED:
        raise AssertionError(
            f"Same-lineage llm-serve lacks signature permission: {normal_permission.value}"
        )
    device.tap("Start server")
    status = device.refresh_until("no model")
    evidence["signed_client_status"] = status
    evidence["signed_client_reached_owner"] = True
    pid_before = device.pid()
    if not pid_before:
        raise AssertionError("Canonical local AI owner process is not running after signed bind")
    evidence["owner_pid_before"] = pid_before
    screenshot = args.output / "local-ai-ipc-signed-client.png"
    digest = device.capture(screenshot)
    evidence["screenshots"].append({"file": screenshot.name, "sha256": digest})

    # Kill only the :voice process without force-stopping the package. The
    # llm-serve process and its LocalAiRemoteClient stay alive, so the next
    # request must observe binder death and rebind to a newly created canonical
    # owner process instead of constructing a second runtime.
    evidence["stage"] = "owner_rebind"
    device.kill_owner_process(pid_before)
    device.wait_pid_gone(pid_before)
    device.tap("Start server")
    pid_after = device.wait_new_pid(pid_before)
    evidence["owner_pid_after"] = pid_after
    recovered_status = device.refresh_until("no model")
    evidence["recovered_status"] = recovered_status
    evidence["owner_process_recreated"] = True
    screenshot = args.output / "local-ai-ipc-rebound.png"
    digest = device.capture(screenshot)
    evidence["screenshots"].append({"file": screenshot.name, "sha256": digest})

    evidence["stage"] = "complete"
    evidence["passed"] = True
    sys.excepthook = previous_excepthook
    manifest.write_text(json.dumps(evidence, indent=2, sort_keys=True) + "\n", encoding="utf-8")


if __name__ == "__main__":
    main()
