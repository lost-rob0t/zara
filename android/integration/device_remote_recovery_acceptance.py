"""Reproduce the remote voice -> protocol failure -> recovery class on a real APK.

Drives the installed Zara app on an emulator against the deterministic
recovery fixture (remote_recovery_fixture.py): a successful text turn, a
real voice turn through capture -> STT stream -> TTS playback, one injected
malformed frame mid-turn, one injected transport close, typed UI error
truth, Diagnostics v2 evidence, reconnect recovery, a second successful
turn, and activity recreation fencing.
"""

from __future__ import annotations

import argparse
import json
import os
import re
from pathlib import Path
import time

from device_acceptance import Device, open_menu
from device_remote_acceptance import (
    collect_app_diagnostics,
    find_curve_public_key,
    type_printable_ascii,
    visible_device_text,
)


APP_PACKAGE = "ai.zara.app"


def read_fixture(path: Path) -> dict[str, str]:
    values: dict[str, str] = {}
    for line in path.read_text(encoding="utf-8").splitlines():
        if not line:
            continue
        key, separator, value = line.partition("=")
        if not separator or not key or not value:
            raise AssertionError("Malformed recovery fixture entry")
        values[key] = value
    required = {"endpoint", "server_public", "client_public", "client_secret", "control_fifo"}
    missing = required - values.keys()
    if missing:
        raise AssertionError(f"Recovery fixture is missing: {sorted(missing)}")
    return values


def arm_fixture(fixture: dict[str, str], mode: str) -> None:
    fifo = fixture["control_fifo"]
    descriptor = os.open(fifo, os.O_WRONLY)
    try:
        os.write(descriptor, f"ARM {mode}\n".encode("ascii"))
    finally:
        os.close(descriptor)
    time.sleep(0.2)


def endpoint_port(endpoint: str) -> int:
    match = re.fullmatch(r"tcp://127[.]0[.]0[.]1:(\d+)", endpoint)
    if match is None:
        raise AssertionError(f"Recovery fixture endpoint is not loopback TCP: {endpoint!r}")
    return int(match.group(1))


def scroll_chat_to_bottom(device: Device, swipes: int = 6) -> None:
    size = device.adb("shell", "wm", "size").strip()
    match = re.search(r"(\d+)x(\d+)", size)
    if match is None:
        raise AssertionError(f"Emulator did not report its size: {size!r}")
    width, height = int(match.group(1)), int(match.group(2))
    center_x, lower_y, upper_y = width // 2, int(height * 0.72), int(height * 0.28)
    for _ in range(swipes):
        device.adb(
            "shell", "input", "swipe", str(center_x), str(lower_y),
            str(center_x), str(upper_y), "250",
        )
    time.sleep(0.5)


def send_chat_turn(device: Device, text: str, expect: str, timeout: float = 25.0) -> None:
    open_chat_and_submit(device, text)
    scroll_chat_to_bottom(device)
    device.await_contains(expect, timeout=timeout)


def open_chat_and_submit(device: Device, text: str) -> None:
    open_menu(device, "Chat")
    device.tap_tab("Chat")
    device.await_label("Ask anything…", timeout=10.0)
    device.tap("Ask anything…")
    type_printable_ascii(device, text)
    device.press_back()
    device.tap("↑")


def diagnostics_preview_text(device: Device) -> str:
    try:
        open_menu(device, "Settings")
        device.press_back()
        device.reveal("Runtime & local AI")
        device.await_label("Runtime & local AI")
        device.tap("Diagnostics")
        lines: list[str] = []
        deadline = time.monotonic() + 10.0
        while time.monotonic() < deadline:
            lines = [
                value
                for node in device.nodes()
                for value in ((node.get("text") or "").strip(),)
                if value
            ]
            if any("primary_failure" in line for line in lines):
                break
            time.sleep(0.5)
        return "\n".join(lines)
    except Exception as error:
        return f"diagnostics preview unavailable: {error}"


def connect_recovery_fixture(device: Device, fixture: dict[str, str]) -> dict[str, object]:
    port = endpoint_port(fixture["endpoint"])
    android_endpoint = f"tcp://127.0.0.1:{port}"
    device.adb("reverse", f"tcp:{port}", f"tcp:{port}")

    device.adb("shell", "pm", "clear", APP_PACKAGE)
    device.adb(
        "shell", "pm", "grant", APP_PACKAGE, "android.permission.RECORD_AUDIO",
    )
    device.start()

    open_menu(device, "Settings")
    device.tap("Connection")
    device.await_label("Create client identity")
    device.tap("Create client identity")
    device.await_label("CLIENT PUBLIC KEY")
    client_public = find_curve_public_key(device)

    device.await_label("Server CURVE public key")
    device.tap("Server CURVE public key")
    type_printable_ascii(device, fixture["server_public"])
    device.press_back()
    device.tap("Pin server key")
    device.await_contains("Client identity and server pin are ready", timeout=10.0)

    device.await_label("tcp://host:port")
    device.tap("tcp://host:port")
    type_printable_ascii(device, android_endpoint)
    device.press_back()
    device.tap("Connect")
    device.await_label("connected", timeout=20.0)
    device.capture("recovery-connected")

    device.press_back()
    device.reveal("Runtime & local AI")
    device.await_label("Runtime & local AI")
    device.tap("Runtime & local AI")
    device.tap("Remote")
    open_menu(device, "Chat")
    return {"endpoint": android_endpoint, "client_public": client_public}


def complete_voice_turn(device: Device, expect_transcript: str) -> None:
    open_menu(device, "Chat")
    device.tap_tab("Voice")
    device.await_label("Start talking", timeout=15.0)
    device.tap("Start talking")
    device.await_label("Stop & send", timeout=10.0)
    time.sleep(1.5)
    device.tap("Stop & send")
    device.await_contains(expect_transcript, timeout=25.0)
    device.capture("recovery-voice-turn")


def assert_typed_error_card(device: Device, expected_code: str) -> None:
    open_menu(device, "Chat")
    device.await_contains("Remote protocol failed", timeout=20.0)
    device.await_contains(f"Code: {expected_code}", timeout=10.0)
    device.await_contains("Connection: ", timeout=10.0)
    text = " ".join(visible_device_text(device))
    if "operation_failed" in text:
        raise AssertionError("UI still renders the generic operation_failed umbrella")
    if text.count("Remote protocol failed") != 1:
        raise AssertionError(
            f"expected exactly one specific error surface, saw {text.count('Remote protocol failed')}"
        )
    device.capture("recovery-typed-error-card")


def assert_diagnostics_names_primary_failure(
    device: Device,
    expected_code: str,
    expected_subsystem: str,
    evidence_state: str,
) -> None:
    open_menu(device, "Settings")
    device.press_back()
    device.reveal("Runtime & local AI")
    device.await_label("Runtime & local AI")
    device.tap("Diagnostics")
    device.await_contains("ZARA-LOCAL-DIAGNOSTICS/2", timeout=10.0)
    device.await_contains(f"primary_failure.code={expected_code}", timeout=10.0)
    device.await_contains(f"primary_failure.subsystem={expected_subsystem}", timeout=10.0)
    device.await_contains("primary_failure.operation=", timeout=10.0)
    device.await_contains("primary_failure.last_success=", timeout=10.0)
    device.capture(evidence_state)


def assert_footer_truth(device: Device, fragment: str) -> None:
    open_menu(device, "Chat")
    device.await_contains(fragment, timeout=15.0)


def exercise_recovery(device: Device, fixture: dict[str, str]) -> dict[str, object]:
    evidence: dict[str, object] = {}
    evidence.update(connect_recovery_fixture(device, fixture))

    send_chat_turn(device, "recovery device ping", "stock server response")
    evidence["first_text_turn"] = True
    assert_footer_truth(device, "REMOTE")
    device.capture("recovery-text-turn")

    complete_voice_turn(device, "stock voice transcript")
    evidence["voice_turn_completed"] = True

    arm_fixture(fixture, "MALFORMED")
    send_chat_turn(device, "trigger malformed", "Remote protocol failed")
    assert_typed_error_card(device, "protocol.malformed")
    evidence["malformed_typed_error"] = True
    assert_diagnostics_names_primary_failure(
        device,
        "protocol.malformed",
        "protocol",
        "recovery-malformed-diagnostics-v2",
    )
    evidence["diagnostics_v2_primary_failure"] = True

    send_chat_turn(device, "after malformed recovery", "stock server response")
    evidence["reconnected_second_turn"] = True
    assert_footer_truth(device, "CONNECTED")

    arm_fixture(fixture, "CLOSE")
    open_chat_and_submit(device, "trigger close")
    time.sleep(9.0)
    assert_footer_truth(device, "REMOTE")
    assert_diagnostics_names_primary_failure(
        device,
        "transport.timeout",
        "transport",
        "recovery-close-diagnostics-v2",
    )
    evidence["close_typed_error"] = True

    send_chat_turn(device, "after close recovery", "stock server response")
    evidence["second_recovery_turn"] = True

    device.recreate()
    time.sleep(2.0)
    send_chat_turn(device, "after recreation", "stock server response")
    evidence["recreation_fenced"] = True

    return evidence


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
        result.update(exercise_recovery(device, fixture))
        result["passed"] = True
    except BaseException as error:
        result["failure"] = str(error)
        try:
            result["visible_device_text"] = visible_device_text(device)
        except Exception as text_error:
            result["visible_device_text_failure"] = str(text_error)
        result["diagnostics_v2_on_failure"] = diagnostics_preview_text(device)
        try:
            device.capture("recovery-failure")
        except Exception as capture_error:
            result["capture_failure"] = str(capture_error)
        raise
    finally:
        result.update(collect_app_diagnostics(device, args.output))
        try:
            device.adb("shell", "am", "force-stop", APP_PACKAGE)
        except Exception as stop_error:
            result["force_stop_failure"] = str(stop_error)
        (args.output / "recovery-manifest.json").write_text(
            json.dumps(result, indent=2) + "\n",
            encoding="utf-8",
        )


if __name__ == "__main__":
    main()
