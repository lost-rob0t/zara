"""Exercise the flagship Org APK against a real SAF-backed ordinary-Org corpus.

The fixture path exists only inside the disposable emulator. Production code still
owns no default Org root: the test selects an arbitrary directory through Android's
OpenDocumentTree flow and then consumes that persisted SAF grant.
"""

from __future__ import annotations

import argparse
from datetime import datetime, timedelta, timezone
import hashlib
import json
import os
from pathlib import Path
import re
import tempfile
import time

from device_acceptance import Device, verified_source_sha


PACKAGE = "ai.zara.org.app"
COMPONENT = f"{PACKAGE}/.MainActivity"
FIXTURE_RELATIVE_ROOT = "Documents/ZaraOrgAcceptance"
FIXTURE_TREE_URI = (
    "content://com.android.externalstorage.documents/tree/"
    "primary%3ADocuments%2FZaraOrgAcceptance"
)
PICKER_UI_DUMP_ATTEMPTS = 3


def _push_text(device: Device, remote_path: str, text: str) -> None:
    """Upload exact fixture bytes without relying on nested remote-shell quoting."""

    local_path: Path | None = None
    try:
        with tempfile.NamedTemporaryFile(
            mode="w", encoding="utf-8", newline="", delete=False
        ) as handle:
            handle.write(text)
            local_path = Path(handle.name)
        device.adb("push", str(local_path), remote_path)
    finally:
        if local_path is not None:
            local_path.unlink(missing_ok=True)


def _write_external_text(device: Device, relative_path: str, text: str) -> None:
    if not re.fullmatch(r"[A-Za-z0-9_./-]+", relative_path):
        raise AssertionError(f"unsafe acceptance path: {relative_path!r}")
    absolute = f"/sdcard/{FIXTURE_RELATIVE_ROOT}/{relative_path}"
    parent = absolute.rsplit("/", 1)[0]
    device.adb("shell", "mkdir", "-p", parent)
    _push_text(device, absolute, text)


def prepare_canonical_saf_fixture(device: Device) -> tuple[str, str]:
    """Create ordinary Org files in an arbitrary external-storage test root."""

    today = datetime.now(timezone.utc).date()
    previous = today - timedelta(days=1)
    device.adb("shell", "rm", "-rf", f"/sdcard/{FIXTURE_RELATIVE_ROOT}")

    _write_external_text(
        device,
        "tasks.org",
        "\n".join(
            (
                "#+title: Acceptance Tasks",
                "#+TODO: TODO NEXT | DONE",
                "* TODO Acceptance task",
                ":PROPERTIES:",
                ":ID: acceptance-task",
                ":END:",
                "See [[id:beta][Beta note]].",
                "",
            )
        ),
    )
    _write_external_text(
        device,
        "notes/beta.org",
        "\n".join(
            (
                "#+title: Beta",
                "* Beta note",
                ":PROPERTIES:",
                ":ID: beta",
                ":END:",
                "Back to [[id:acceptance-task][Acceptance task]].",
                "",
            )
        ),
    )
    _write_external_text(
        device,
        f"daily/{today.isoformat()}.org",
        "\n".join(
            (
                f"#+title: Daily {today.isoformat()}",
                "* TODO Today acceptance",
                ":PROPERTIES:",
                ":ID: acceptance-today",
                ":END:",
                "Ordinary Org text is canonical.",
                "",
            )
        ),
    )
    _write_external_text(
        device,
        f"daily/{previous.isoformat()}.org",
        "\n".join(
            (
                f"#+title: Daily {previous.isoformat()}",
                "* Previous daily acceptance",
                "Separate daily file retained for Emacs/org-roam-dailies compatibility.",
                "",
            )
        ),
    )
    return today.isoformat(), previous.isoformat()


def seed_picker_and_daily_configuration(device: Device) -> None:
    """Seed only canonical Org-home configuration, never a product data store."""

    xml = "\n".join(
        (
            "<?xml version='1.0' encoding='utf-8' standalone='yes' ?>",
            "<map>",
            "    <string name=\"mode\">SHARED</string>",
            f"    <string name=\"custom-tree-uri\">{FIXTURE_TREE_URI}</string>",
            "    <string name=\"daily-path-template\">daily/{date}.org</string>",
            "    <string name=\"daily-date-pattern\">yyyy-MM-dd</string>",
            "    <string name=\"daily-zone-id\">UTC</string>",
            "</map>",
            "",
        )
    )
    remote_stage = "/data/local/tmp/zara-org-home.xml"
    _push_text(device, remote_stage, xml)
    try:
        device.adb("shell", "chmod", "0644", remote_stage)
        device.adb("shell", "run-as", PACKAGE, "mkdir", "-p", "shared_prefs")
        device.adb(
            "shell",
            "run-as",
            PACKAGE,
            "cp",
            remote_stage,
            "shared_prefs/zara-org-home.xml",
        )
    finally:
        device.adb("shell", "rm", "-f", remote_stage)


def _picker_nodes(device: Device):
    """Read DocumentsUI semantics through a tiny, fail-closed transition budget."""

    last_error: AssertionError | None = None
    for attempt in range(PICKER_UI_DUMP_ATTEMPTS):
        try:
            return tuple(device.nodes())
        except AssertionError as error:
            if "UIAutomator did not create" not in str(error):
                raise
            last_error = error
            if attempt + 1 < PICKER_UI_DUMP_ATTEMPTS:
                time.sleep(0.2)
    assert last_error is not None
    raise AssertionError(
        "DocumentsUI hierarchy remained unavailable after "
        f"{PICKER_UI_DUMP_ATTEMPTS} bounded attempts: {last_error}"
    ) from last_error


def _find_picker_contains(device: Device, fragment: str):
    """Find OS-owned DocumentsUI text without assuming presentation casing."""

    needle = fragment.casefold()
    return next(
        (
            node
            for node in _picker_nodes(device)
            if any(
                needle in (node.get(attribute) or "").casefold()
                for attribute in ("text", "content-desc")
            )
        ),
        None,
    )


def _find_picker_action(device: Device, action: str):
    """Find an OS-owned picker action exactly, ignoring presentation casing only."""

    needle = action.strip().casefold()
    return next(
        (
            node
            for node in _picker_nodes(device)
            if any(
                (node.get(attribute) or "").strip().casefold() == needle
                for attribute in ("text", "content-desc")
            )
        ),
        None,
    )


def _await_picker_action(
    device: Device, action: str, timeout: float = 20.0
) -> None:
    deadline = time.monotonic() + timeout
    while time.monotonic() < deadline:
        if _find_picker_action(device, action) is not None:
            return
        time.sleep(0.2)
    raise AssertionError(f"System picker did not retain action {action}")


def _tap_picker_action(device: Device, action: str) -> None:
    node = _find_picker_action(device, action)
    if node is None:
        raise AssertionError(f"System picker action is not reachable: {action}")
    left, top, right, bottom = device.bounds(node)
    if right <= left or bottom <= top:
        raise AssertionError(f"System picker action has empty bounds: {action}")
    device.adb(
        "shell",
        "input",
        "tap",
        str((left + right) // 2),
        str((top + bottom) // 2),
    )
    time.sleep(0.4)


def _tap_contains(device: Device, fragment: str) -> None:
    node = _find_picker_contains(device, fragment)
    if node is None:
        raise AssertionError(f"Control is not reachable: {fragment}")
    left, top, right, bottom = device.bounds(node)
    if right <= left or bottom <= top:
        raise AssertionError(f"Control has empty bounds: {fragment}")
    device.adb(
        "shell",
        "input",
        "tap",
        str((left + right) // 2),
        str((top + bottom) // 2),
    )
    time.sleep(0.4)


def _tap_app_control_through_launcher_anr(
    device: Device, label: str, timeout: float = 5.0
) -> None:
    """Tap a Zara control without letting a hosted Pixel Launcher ANR mask it."""

    deadline = time.monotonic() + timeout
    while time.monotonic() < deadline:
        if device.dismiss_pixel_launcher_anr():
            continue
        node = device.find(label)
        if node is not None:
            left, top, right, bottom = device.bounds(node)
            if right <= left or bottom <= top:
                raise AssertionError(f"Control has empty bounds: {label}")
            device.adb(
                "shell",
                "input",
                "tap",
                str((left + right) // 2),
                str((top + bottom) // 2),
            )
            time.sleep(0.2)
            return
        time.sleep(0.2)
    raise AssertionError(f"Control is not reachable after launcher ANR recovery: {label}")


def _navigate_picker_to_fixture_if_needed(device: Device) -> None:
    """Recover when hosted DocumentsUI ignores the requested initial tree URI."""

    if device.find_contains("Can’t use this folder") is None:
        return

    _tap_contains(device, "Documents")
    device.await_contains("ZaraOrgAcceptance")
    _tap_contains(device, "ZaraOrgAcceptance")
    _await_picker_action(device, "Use this folder")
    if device.find_contains("Can’t use this folder") is not None:
        raise AssertionError("SAF picker did not enter the acceptance fixture directory")


def _visible_text(device: Device) -> str:
    values: list[str] = []
    seen: set[str] = set()
    for node in device.nodes():
        for attribute in ("text", "content-desc"):
            value = (node.get(attribute) or "").strip()
            if value and value not in seen:
                seen.add(value)
                values.append(value)
    return "\n".join(values) + "\n"


def capture_with_text(device: Device, name: str, text_evidence: list[dict]) -> None:
    device.capture(name)
    text = _visible_text(device)
    path = device.output / f"{name}.txt"
    path.write_text(text, encoding="utf-8")
    text_evidence.append(
        {
            "state": name,
            "file": path.name,
            "sha256": hashlib.sha256(text.encode("utf-8")).hexdigest(),
        }
    )


def connect_fixture_through_saf(device: Device) -> None:
    device.adb("shell", "am", "force-stop", PACKAGE)
    device.launch_surface(COMPONENT, "Org")
    device.await_contains("Shared Org workspace is unavailable")
    _tap_app_control_through_launcher_anr(device, "Choose Org directory")
    _await_picker_action(device, "Use this folder")
    _navigate_picker_to_fixture_if_needed(device)
    _tap_picker_action(device, "Use this folder")
    _await_picker_action(device, "Allow")
    _tap_picker_action(device, "Allow")
    device.await_contains("Acceptance task", timeout=20.0)


def exercise_org_surfaces(
    device: Device,
    *,
    today: str,
    previous: str,
    text_evidence: list[dict],
) -> None:
    device.await_label("Todo")
    device.await_contains("Acceptance task")
    device.await_contains("Today acceptance")
    capture_with_text(device, "org-todo", text_evidence)

    device.tap("Roam")
    device.await_label("Search Org-roam nodes")
    device.await_contains("Acceptance task")
    device.await_contains("Beta note")
    capture_with_text(device, "org-roam", text_evidence)

    device.tap("Daily")
    device.await_contains(f"Today · {today}")
    device.await_contains("Ordinary Org text is canonical")
    capture_with_text(device, "org-daily-today", text_evidence)

    device.reveal(previous)
    device.await_contains("Separate daily file retained")
    capture_with_text(device, "org-daily-previous", text_evidence)


def main() -> None:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--serial", default=os.environ.get("ANDROID_SERIAL"))
    parser.add_argument(
        "--output", type=Path, default=Path("android/org-app/build/reports/device")
    )
    parser.add_argument("--source-sha")
    args = parser.parse_args()
    if not args.serial:
        parser.error("Select a test emulator explicitly with --serial or ANDROID_SERIAL")

    source_sha = verified_source_sha(args.source_sha)
    args.output.mkdir(parents=True, exist_ok=True)
    device = Device(args.serial, args.output)
    text_evidence: list[dict] = []
    result = {
        "source_sha": source_sha,
        "serial": args.serial,
        "passed": False,
        "corpus_authority": "ordinary Org files through persisted Android SAF",
        "fixture_root": FIXTURE_RELATIVE_ROOT,
        "fixture_root_is_test_only": True,
        "screenshots": device.screenshots,
        "text_evidence": text_evidence,
    }

    try:
        today, previous = prepare_canonical_saf_fixture(device)
        seed_picker_and_daily_configuration(device)
        connect_fixture_through_saf(device)
        exercise_org_surfaces(
            device,
            today=today,
            previous=previous,
            text_evidence=text_evidence,
        )
        result["passed"] = True
    except BaseException as error:
        result["failure"] = str(error)
        try:
            capture_with_text(device, "org-failure", text_evidence)
        except Exception as capture_error:
            result["capture_failure"] = str(capture_error)
        raise
    finally:
        (args.output / "manifest.json").write_text(
            json.dumps(result, indent=2) + "\n", encoding="utf-8"
        )


if __name__ == "__main__":
    main()
