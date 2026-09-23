"""Prove the installed Zara Android APK can authenticate and complete a remote turn."""

from __future__ import annotations

import argparse
import base64
import json
import os
from pathlib import Path
import re
import socket
import time

from device_acceptance import Device, open_menu
from zara.security_admin import SecurityAdminClient
from zmq.utils import z85


APP_PACKAGE = "ai.zara.app"
APP_DIAGNOSTICS_PATH = "no_backup/zara/diagnostics/local-runtime.log"
_FATAL_LOG_MARKERS = (
    "FATAL EXCEPTION",
    "ANR in ai.zara.app",
    "Process: ai.zara.app, PID:",
)

_APP_SCOPED_FATAL_LOG_MARKERS = (
    "ANR in ai.zara.app",
    "Process: ai.zara.app, PID:",
)


def read_fixture(path: Path) -> dict[str, str]:
    values: dict[str, str] = {}
    for line in path.read_text(encoding="utf-8").splitlines():
        if not line:
            continue
        key, separator, value = line.partition("=")
        if not separator or not key or not value:
            raise AssertionError("Malformed stock Zara fixture entry")
        values[key] = value
    required = {
        "endpoint",
        "server_public",
        "security_admin_path",
        "acceptance_host",
        "acceptance_port",
    }
    missing = required - values.keys()
    if missing:
        raise AssertionError(f"Stock Zara fixture is missing: {sorted(missing)}")
    return values


def endpoint_port(endpoint: str) -> int:
    match = re.fullmatch(r"tcp://127[.]0[.]0[.]1:(\d+)", endpoint)
    if match is None:
        raise AssertionError(f"Stock Zara fixture endpoint is not loopback TCP: {endpoint!r}")
    port = int(match.group(1))
    if port not in range(1, 65536):
        raise AssertionError("Stock Zara fixture port is invalid")
    return port


def require_reverse_mapping(device: Device, port: int) -> str:
    mappings = device.adb("reverse", "--list")
    expected = f"tcp:{port} tcp:{port}"
    if expected not in mappings:
        raise AssertionError(
            f"adb reverse does not expose the stock Zara server port: expected {expected!r}, "
            f"got {mappings!r}"
        )
    return mappings


def visible_device_text(device: Device) -> list[str]:
    values: set[str] = set()
    for node in device.nodes():
        for attribute in ("text", "content-desc"):
            value = (node.get(attribute) or "").strip()
            if value:
                values.add(value)
    return sorted(values)


def find_curve_public_key(device: Device) -> str:
    for node in device.nodes():
        for attribute in ("text", "content-desc"):
            candidate = (node.get(attribute) or "").strip()
            if len(candidate) != 40:
                continue
            try:
                decoded = z85.decode(candidate.encode("ascii"))
            except (UnicodeError, ValueError):
                continue
            if len(decoded) == 32:
                return candidate
    raise AssertionError("Android UI did not expose the client CURVE public key")


def type_printable_ascii(device: Device, value: str) -> None:
    if not value or any(ord(char) < 0x20 or ord(char) > 0x7E for char in value):
        raise AssertionError("Remote acceptance input must be printable ASCII")
    # Avoid adb-shell metacharacter handling entirely: only a base64 token enters
    # the remote command. Keep the whole shell pipeline in one adb shell argument;
    # splitting it through `sh -c` makes adb join the argv before the device shell
    # sees it, so only `input` becomes the -c program and no text reaches Compose.
    encoded = base64.b64encode(value.encode("ascii")).decode("ascii")
    command = f'input text "$(printf %s \'{encoded}\' | base64 -d)"'
    device.adb("shell", command)
    time.sleep(0.5)


def signal_turn_acceptance(fixture: dict[str, str]) -> None:
    port = int(fixture["acceptance_port"])
    if port not in range(1, 65536):
        raise AssertionError("Stock Zara acceptance port is invalid")
    with socket.create_connection((fixture["acceptance_host"], port), timeout=5.0) as barrier:
        barrier.sendall(b"A")


def enroll_live_server(fixture: dict[str, str], public_key: str) -> None:
    result = SecurityAdminClient(fixture["security_admin_path"]).request(
        "enroll",
        public_key=public_key,
        device_id="android-emulator-acceptance",
    )
    if not isinstance(result, dict) or result.get("active") is not True:
        raise AssertionError("Stock Zara server did not activate Android client enrollment")
    if result.get("public_key") != public_key:
        raise AssertionError("Stock Zara server enrolled a different Android public key")


def collect_app_diagnostics(device: Device, output: Path) -> dict[str, object]:
    evidence: dict[str, object] = {}
    try:
        diagnostics = device.adb(
            "shell",
            "run-as",
            APP_PACKAGE,
            "cat",
            APP_DIAGNOSTICS_PATH,
        )
        path = output / "remote-app-diagnostics.log"
        path.write_text(diagnostics, encoding="utf-8")
        evidence["app_diagnostics"] = path.name
    except Exception as error:
        evidence["app_diagnostics_failure"] = str(error)

    try:
        try:
            pid = device.adb("shell", "pidof", APP_PACKAGE).strip()
            if not re.fullmatch(r"\d+", pid):
                raise AssertionError(f"Zara app pid is unavailable: {pid!r}")
            logcat = device.adb("logcat", "-d", "--pid", pid, "-v", "threadtime")
            evidence["logcat_pid_filtered"] = True
        except Exception:
            logcat = device.adb("logcat", "-d", "-v", "threadtime")
            evidence["logcat_pid_filtered"] = False
        path = output / "remote-logcat.log"
        path.write_text(logcat, encoding="utf-8")
        evidence["logcat"] = path.name
        markers = (
            _FATAL_LOG_MARKERS
            if evidence.get("logcat_pid_filtered") is True
            else _APP_SCOPED_FATAL_LOG_MARKERS
        )
        fatal_markers = [marker for marker in markers if marker in logcat]
        evidence["fatal_log_markers"] = fatal_markers
    except Exception as error:
        evidence["logcat_failure"] = str(error)
    return evidence


def exercise_remote_connection(device: Device, fixture: dict[str, str]) -> dict[str, object]:
    port = endpoint_port(fixture["endpoint"])
    android_endpoint = f"tcp://127.0.0.1:{port}"
    reverse_mapping = require_reverse_mapping(device, port)

    device.adb("shell", "pm", "clear", APP_PACKAGE)
    device.start()

    # First prove the embedded Android Local server through the installed UI.
    open_menu(device, "Settings")
    device.tap_tab("Runtime")
    device.await_contains("LOCAL ZARA SERVER", timeout=20.0)
    device.await_label("ready", timeout=20.0)
    device.tap("Local")
    open_menu(device, "Chat")
    device.await_label("Ask anything…", timeout=20.0)
    device.tap("Ask anything…")
    type_printable_ascii(device, "?- Result = zara_ready.")
    device.press_back()
    device.tap("↑")
    device.await_contains("zara_ready", timeout=20.0)
    device.await_contains("LOCAL", timeout=5.0)
    device.capture("local-text-turn")

    # Then enroll the same installed app and prove the desktop/server path.
    open_menu(device, "Settings")
    device.tap_tab("Connection")
    device.await_label("Create client identity")
    device.tap("Create client identity")
    device.await_label("CLIENT PUBLIC KEY")
    client_public = find_curve_public_key(device)
    enroll_live_server(fixture, client_public)

    device.await_label("Server CURVE public key")
    device.tap("Server CURVE public key")
    type_printable_ascii(device, fixture["server_public"])
    device.press_back()
    device.tap("Pin server key")
    device.await_contains("Client identity and server pin are ready", timeout=10.0)
    if fixture["server_public"] not in visible_device_text(device):
        raise AssertionError("Android UI did not retain the exact stock server CURVE public key")

    device.await_label("tcp://host:port")
    device.tap("tcp://host:port")
    type_printable_ascii(device, android_endpoint)
    device.press_back()
    device.tap("Connect")
    device.await_label("connected", timeout=20.0)
    device.await_contains("session", timeout=5.0)
    device.capture("remote-connected")

    # Force the exact Remote routing policy for the turn so a Local response
    # cannot accidentally satisfy this end-to-end gate.
    device.tap_tab("Runtime")
    device.tap("Remote")
    open_menu(device, "Chat")
    device.await_label("Ask anything…", timeout=10.0)
    device.tap("Ask anything…")
    device.type_text("device_remote_ping")
    device.press_back()
    device.tap("↑")
    signal_turn_acceptance(fixture)
    device.await_contains("stock server response", timeout=20.0)
    device.await_contains("REMOTE", timeout=5.0)
    device.capture("remote-text-turn")

    return {
        "endpoint": android_endpoint,
        "reverse_mapping": reverse_mapping.splitlines(),
        "local_turn_completed": True,
        "client_enrolled": True,
        "server_pin_verified": True,
        "connected": True,
        "remote_turn_completed": True,
    }


def main() -> None:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--serial", default=os.environ.get("ANDROID_SERIAL"))
    parser.add_argument("--fixture-file", type=Path, required=True)
    parser.add_argument(
        "--output",
        type=Path,
        default=Path("android/app/build/reports/device"),
    )
    args = parser.parse_args()
    if not args.serial:
        parser.error("Select a test emulator explicitly with --serial or ANDROID_SERIAL")

    args.output.mkdir(parents=True, exist_ok=True)
    fixture = read_fixture(args.fixture_file)
    device = Device(args.serial, args.output)
    result: dict[str, object] = {
        "serial": args.serial,
        "passed": False,
        "screenshots": device.screenshots,
    }

    try:
        result.update(exercise_remote_connection(device, fixture))
        result["passed"] = True
    except BaseException as error:
        result["failure"] = str(error)
        try:
            result["reverse_mapping"] = device.adb("reverse", "--list").splitlines()
        except Exception as reverse_error:
            result["reverse_mapping_failure"] = str(reverse_error)
        try:
            result["visible_device_text"] = visible_device_text(device)
        except Exception as text_error:
            result["visible_device_text_failure"] = str(text_error)
        try:
            device.capture("remote-failure")
        except Exception as capture_error:
            result["capture_failure"] = str(capture_error)
        raise
    finally:
        result.update(collect_app_diagnostics(device, args.output))
        try:
            device.adb("shell", "am", "force-stop", APP_PACKAGE)
        finally:
            (args.output / "remote-manifest.json").write_text(
                json.dumps(result, indent=2) + "\n",
                encoding="utf-8",
            )


if __name__ == "__main__":
    main()
