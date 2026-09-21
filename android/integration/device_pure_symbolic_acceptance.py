"""Prove the installed Zara APK keeps pure-symbolic dialogue durable with zero model use."""

from __future__ import annotations

import argparse
import base64
import json
import os
from pathlib import Path
import sqlite3
import subprocess
import tempfile
import time

from device_acceptance import Device, verified_source_sha


APP_PACKAGE = "ai.zara.app"
DATABASE_PATH = "databases/zara.db"
CONTEXT_VERSION = "ZARA-SYMBOLIC-DIALOGUE-CONTEXT/1"


def type_printable_ascii(device: Device, value: str) -> None:
    if not value or any(ord(char) < 0x20 or ord(char) > 0x7E for char in value):
        raise AssertionError("Pure-symbolic acceptance input must be printable ASCII")
    encoded = base64.b64encode(value.encode("ascii")).decode("ascii")
    command = f'input text "$(printf %s \'{encoded}\' | base64 -d)"'
    device.adb("shell", command)
    time.sleep(0.5)


def send_chat(device: Device, text: str, expected: str) -> None:
    device.await_label("Ask anything…", timeout=30.0)
    device.tap("Ask anything…")
    type_printable_ascii(device, text)
    device.press_back()
    device.tap("↑")
    device.await_contains(expected, timeout=30.0)


def airplane_mode_enabled(device: Device) -> bool:
    observed = device.adb("shell", "settings", "get", "global", "airplane_mode_on").strip()
    if observed not in {"0", "1"}:
        raise AssertionError(f"Could not read emulator airplane-mode state: {observed!r}")
    return observed == "1"


def set_airplane_mode(device: Device, enabled: bool) -> None:
    verb = "enable" if enabled else "disable"
    device.adb("shell", "cmd", "connectivity", "airplane-mode", verb)
    deadline = time.monotonic() + 10.0
    while time.monotonic() < deadline:
        if airplane_mode_enabled(device) == enabled:
            return
        time.sleep(0.2)
    raise AssertionError(f"Emulator airplane mode did not become {verb}d")


def pull_app_file(device: Device, relative_path: str, destination: Path, *, required: bool) -> bool:
    try:
        data = device.adb(
            "exec-out",
            "run-as",
            APP_PACKAGE,
            "cat",
            relative_path,
            binary=True,
        )
    except subprocess.CalledProcessError:
        if required:
            raise
        return False
    destination.write_bytes(data)
    return True


def inspect_pure_symbolic_database(device: Device, output: Path) -> dict[str, object]:
    # Stop the process before copying SQLite files so the acceptance evidence is a
    # stable on-disk snapshot rather than a race against a live WAL writer.
    device.adb("shell", "am", "force-stop", APP_PACKAGE)
    time.sleep(0.5)

    with tempfile.TemporaryDirectory(prefix="zara-pure-symbolic-db-") as temporary:
        root = Path(temporary)
        database = root / "zara.db"
        pull_app_file(device, DATABASE_PATH, database, required=True)
        pull_app_file(device, f"{DATABASE_PATH}-wal", root / "zara.db-wal", required=False)
        pull_app_file(device, f"{DATABASE_PATH}-shm", root / "zara.db-shm", required=False)

        connection = sqlite3.connect(database)
        connection.row_factory = sqlite3.Row
        try:
            projections = connection.execute(
                """
                SELECT conversation_id, principal_id, turn_id, outcome,
                       projection_generation, runtime_generation, dialogue_act,
                       dialogue_state_json, renderer_provenance, providers_enabled,
                       max_model_calls, provider_calls, model_calls
                FROM desktop_symbolic_projections
                WHERE principal_id = 'local:owner'
                ORDER BY updated_at DESC
                """
            ).fetchall()
            if len(projections) != 1:
                raise AssertionError(
                    "Installed pure-symbolic run must leave exactly one local-owner projection; "
                    f"found {len(projections)}"
                )
            projection = dict(projections[0])
            if projection["outcome"] != "success":
                raise AssertionError(f"Pure-symbolic projection is not successful: {projection!r}")
            if projection["providers_enabled"] != 0:
                raise AssertionError("Pure-symbolic projection has providers enabled")
            if projection["max_model_calls"] != 0:
                raise AssertionError("Pure-symbolic projection changed max_model_calls")
            if projection["provider_calls"] != 0:
                raise AssertionError("Pure-symbolic projection recorded provider calls")
            if projection["model_calls"] != 0:
                raise AssertionError("Pure-symbolic projection recorded model calls")
            if projection["renderer_provenance"] != "symbolic-dcg/v1":
                raise AssertionError("Pure-symbolic projection lost deterministic renderer provenance")

            dialogue_state = json.loads(projection["dialogue_state_json"])
            if dialogue_state.get("version") != CONTEXT_VERSION:
                raise AssertionError("Pure-symbolic projection has the wrong context version")
            context_term = dialogue_state.get("term")
            if not isinstance(context_term, str) or not context_term.startswith("completed_frame("):
                raise AssertionError("Pure-symbolic continuation did not retain a completed frame")

            conversations = connection.execute(
                """
                SELECT id, provider, model
                FROM desktop_conversations
                WHERE principal_id = 'local:owner'
                """
            ).fetchall()
            if len(conversations) != 1:
                raise AssertionError(
                    "Installed pure-symbolic run must leave exactly one canonical conversation; "
                    f"found {len(conversations)}"
                )
            conversation = conversations[0]
            if conversation["provider"] or conversation["model"]:
                raise AssertionError("Pure-symbolic conversation persisted provider/model selection")
            if conversation["id"] != projection["conversation_id"]:
                raise AssertionError("Projection is not owned by the canonical conversation")

            messages = connection.execute(
                """
                SELECT sequence, turn_id, role, content, status
                FROM desktop_messages
                WHERE conversation_id = ? AND principal_id = 'local:owner'
                ORDER BY sequence
                """,
                (projection["conversation_id"],),
            ).fetchall()
            if any(row["status"] in {"pending", "streaming"} for row in messages):
                raise AssertionError("Pure-symbolic process recreation left a nonterminal message")
            contents = [row["content"] for row in messages]
            required_fragments = (
                "Pure symbolic mode enabled",
                "How long should I set the timer for?",
                "capability-checked execution",
                "welcome",
                "handle that symbolically yet",
            )
            for fragment in required_fragments:
                if not any(fragment.lower() in content.lower() for content in contents):
                    raise AssertionError(f"Canonical history lost pure-symbolic transcript fragment: {fragment}")

            latest_turn = next(
                (row["turn_id"] for row in reversed(messages) if row["turn_id"]),
                None,
            )
            if latest_turn != projection["turn_id"]:
                raise AssertionError("Projection turn id does not match canonical terminal history")
        finally:
            connection.close()

        evidence_db = output / "pure-symbolic-zara.db"
        evidence_db.write_bytes(database.read_bytes())
        return {
            "conversation_id": projection["conversation_id"],
            "turn_id": projection["turn_id"],
            "projection_generation": projection["projection_generation"],
            "runtime_generation": projection["runtime_generation"],
            "dialogue_act": projection["dialogue_act"],
            "dialogue_context_version": dialogue_state["version"],
            "dialogue_context_term": dialogue_state["term"],
            "renderer_provenance": projection["renderer_provenance"],
            "providers_enabled": False,
            "max_model_calls": 0,
            "provider_calls": 0,
            "model_calls": 0,
            "message_count": len(messages),
            "database_evidence": evidence_db.name,
        }


def exercise_pure_symbolic_dialogue(device: Device, output: Path) -> dict[str, object]:
    clear_result = device.adb("shell", "pm", "clear", APP_PACKAGE).strip()
    if clear_result != "Success":
        raise AssertionError(f"Could not clear installed Zara state: {clear_result!r}")

    # A cleared app has no enrolled server/client identity or provider credentials.
    # The installed transcript now also runs with Android airplane mode asserted, so a
    # parse miss, ambiguity, missing expert, renderer gap, or runtime error cannot hide
    # a network/provider fallback behind otherwise-zero accounting.
    original_airplane_mode = airplane_mode_enabled(device)
    try:
        set_airplane_mode(device, True)
        if not airplane_mode_enabled(device):
            raise AssertionError("Pure-symbolic acceptance requires verified offline execution")

        device.start()
        send_chat(device, "/symbolic on", "Pure symbolic mode enabled")
        device.capture("pure-symbolic-enabled")

        send_chat(device, "timer", "How long should I set the timer for?")
        device.capture("pure-symbolic-clarification")

        device.recreate()
        device.await_contains("How long should I set the timer for?", timeout=30.0)
        send_chat(
            device,
            "5 minutes",
            "That action needs capability-checked execution before I can report success.",
        )
        device.capture("pure-symbolic-follow-up-after-restart")

        device.recreate()
        device.await_contains("capability-checked execution", timeout=30.0)
        send_chat(device, "thanks", "welcome")
        device.capture("pure-symbolic-social-follow-up")

        device.recreate()
        device.await_contains("welcome", timeout=30.0)
        send_chat(device, "frobnicate the moon", "handle that symbolically yet")
        device.capture("pure-symbolic-unsupported-no-fallback")

        device.recreate()
        device.await_contains("handle that symbolically yet", timeout=30.0)
        device.capture("pure-symbolic-final-recreated")

        projection = inspect_pure_symbolic_database(device, output)
        projection["offline_verified"] = True
        return projection
    finally:
        if airplane_mode_enabled(device) != original_airplane_mode:
            set_airplane_mode(device, original_airplane_mode)


def main() -> None:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--serial", default=os.environ.get("ANDROID_SERIAL"))
    parser.add_argument(
        "--output",
        type=Path,
        default=Path("android/app/build/reports/device"),
    )
    parser.add_argument("--source-sha")
    args = parser.parse_args()
    if not args.serial:
        parser.error("Select a test emulator explicitly with --serial or ANDROID_SERIAL")

    source_sha = verified_source_sha(args.source_sha)
    args.output.mkdir(parents=True, exist_ok=True)
    device = Device(args.serial, args.output)
    result: dict[str, object] = {
        "source_sha": source_sha,
        "serial": args.serial,
        "passed": False,
        "providers_disabled": True,
        "provider_credentials_required": False,
        "offline_required": True,
        "screenshots": device.screenshots,
    }
    try:
        result["projection"] = exercise_pure_symbolic_dialogue(device, args.output)
        result["passed"] = True
    except BaseException as error:
        result["failure"] = str(error)
        try:
            device.capture("pure-symbolic-failure")
        except Exception as capture_error:
            result["capture_failure"] = str(capture_error)
        raise
    finally:
        (args.output / "pure-symbolic-manifest.json").write_text(
            json.dumps(result, indent=2) + "\n",
            encoding="utf-8",
        )


if __name__ == "__main__":
    main()
