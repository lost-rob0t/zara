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
ACCEPTANCE_EXPERT_PATH = "files/prolog-workspace/p0_acceptance_expert.pl"
ACCEPTANCE_EXPERT_SOURCE = """\
expert_activation(diagnosis, inspect).
symptom(alex, fever).
symptom(alex, cough).
diagnosis_explain(Person, Result) :-
    symptom(Person, fever),
    symptom(Person, cough),
    Result = diagnosis(Person, flu).
"""


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


def install_acceptance_expert(device: Device) -> None:
    """Install and verify a private-workspace expert without shell or stdin parsing."""
    expected = ACCEPTANCE_EXPERT_SOURCE.encode("utf-8")
    staged_remote = f"/data/local/tmp/zara-p0-expert-{os.getpid()}.pl"
    with tempfile.TemporaryDirectory(prefix="zara-p0-expert-") as temporary:
        staged_host = Path(temporary) / "p0_acceptance_expert.pl"
        staged_host.write_bytes(expected)
        device.adb("push", str(staged_host), staged_remote)
        try:
            device.adb("shell", "chmod", "0644", staged_remote)
            device.adb(
                "shell",
                "run-as",
                APP_PACKAGE,
                "mkdir",
                "-p",
                "files/prolog-workspace",
            )
            device.adb(
                "shell",
                "run-as",
                APP_PACKAGE,
                "cp",
                staged_remote,
                ACCEPTANCE_EXPERT_PATH,
            )
        finally:
            device.adb("shell", "rm", "-f", staged_remote)

    observed = device.adb(
        "exec-out",
        "run-as",
        APP_PACKAGE,
        "/system/bin/cat",
        ACCEPTANCE_EXPERT_PATH,
        binary=True,
    )
    if observed != expected:
        raise AssertionError(
            "Installed acceptance expert did not round-trip through app-private storage"
        )


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


def inspect_hard_zero_accounting(
    device: Device,
    *,
    stage: str,
    expected_dialogue_act: str | None = None,
    require_expert_evidence: bool = False,
) -> dict[str, object]:
    """Snapshot the durable per-turn counters before a later turn can reset them."""
    device.adb("shell", "am", "force-stop", APP_PACKAGE)
    time.sleep(0.3)

    with tempfile.TemporaryDirectory(prefix=f"zara-pure-symbolic-{stage}-") as temporary:
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
                SELECT conversation_id, turn_id, outcome, dialogue_act, expert_evidence_json,
                       providers_enabled, max_model_calls, provider_calls, model_calls
                FROM desktop_symbolic_projections
                WHERE principal_id = 'local:owner'
                ORDER BY updated_at DESC
                """
            ).fetchall()
            if len(projections) != 1:
                raise AssertionError(
                    f"{stage}: expected exactly one local-owner symbolic projection; "
                    f"found {len(projections)}"
                )
            projection = dict(projections[0])
            if projection["outcome"] in {"unknown", "pending"}:
                raise AssertionError(
                    f"{stage}: hard-zero checkpoint observed nonterminal projection: {projection!r}"
                )
            if projection["providers_enabled"] != 0:
                raise AssertionError(f"{stage}: pure-symbolic projection has providers enabled")
            if projection["max_model_calls"] != 0:
                raise AssertionError(f"{stage}: max_model_calls changed from zero")
            if projection["provider_calls"] != 0:
                raise AssertionError(f"{stage}: provider_calls is nonzero")
            if projection["model_calls"] != 0:
                raise AssertionError(f"{stage}: model_calls is nonzero")
            if expected_dialogue_act is not None and projection["dialogue_act"] != expected_dialogue_act:
                raise AssertionError(
                    f"{stage}: expected dialogue act {expected_dialogue_act!r}; "
                    f"observed {projection['dialogue_act']!r}"
                )

            expert_evidence = json.loads(projection["expert_evidence_json"])
            if not isinstance(expert_evidence, list):
                raise AssertionError(f"{stage}: expert evidence must decode to a list")
            if require_expert_evidence:
                if len(expert_evidence) != 1 or not isinstance(expert_evidence[0], dict):
                    raise AssertionError(f"{stage}: expected exactly one durable expert evidence record")
                evidence_ref = expert_evidence[0].get("ref")
                if not isinstance(evidence_ref, str) or not evidence_ref.startswith("expert:"):
                    raise AssertionError(
                        f"{stage}: canonical expert evidence ref is missing or malformed: {expert_evidence!r}"
                    )

            conversations = connection.execute(
                """
                SELECT id, provider, model
                FROM desktop_conversations
                WHERE principal_id = 'local:owner'
                """
            ).fetchall()
            if len(conversations) != 1:
                raise AssertionError(
                    f"{stage}: expected exactly one canonical local-owner conversation; "
                    f"found {len(conversations)}"
                )
            conversation = conversations[0]
            if conversation["provider"] or conversation["model"]:
                raise AssertionError(
                    f"{stage}: canonical conversation persisted provider/model selection"
                )
        finally:
            connection.close()

    return {
        "stage": stage,
        "conversation_id": projection["conversation_id"],
        "turn_id": projection["turn_id"],
        "outcome": projection["outcome"],
        "dialogue_act": projection["dialogue_act"],
        "expert_evidence": expert_evidence,
        "providers_enabled": False,
        "max_model_calls": 0,
        "provider_calls": 0,
        "model_calls": 0,
    }


def assert_checkpoint_continuity(
    checkpoints: list[dict[str, object]],
    projection: dict[str, object],
) -> None:
    """Reject recreation that swaps durable conversation identity or canonical expert evidence."""
    if not checkpoints:
        raise AssertionError("Pure-symbolic acceptance recorded no durable checkpoints")

    expected_conversation_id = checkpoints[0].get("conversation_id")
    if not isinstance(expected_conversation_id, str) or not expected_conversation_id:
        raise AssertionError("First durable checkpoint has no canonical conversation_id")

    by_stage: dict[str, dict[str, object]] = {}
    for checkpoint in checkpoints:
        stage = checkpoint.get("stage")
        if not isinstance(stage, str) or not stage:
            raise AssertionError(f"Durable checkpoint has no stage: {checkpoint!r}")
        if stage in by_stage:
            raise AssertionError(f"Duplicate durable checkpoint stage: {stage}")
        by_stage[stage] = checkpoint
        if checkpoint.get("conversation_id") != expected_conversation_id:
            raise AssertionError(
                f"{stage}: process recreation replaced canonical conversation identity"
            )

    if projection.get("conversation_id") != expected_conversation_id:
        raise AssertionError("Final durable snapshot is not the original canonical conversation")

    def evidence_ref(stage: str, value: dict[str, object]) -> str:
        evidence = value.get("expert_evidence")
        if not isinstance(evidence, list) or len(evidence) != 1 or not isinstance(evidence[0], dict):
            raise AssertionError(f"{stage}: expected exactly one canonical expert evidence record")
        ref = evidence[0].get("ref")
        if not isinstance(ref, str) or not ref.startswith("expert:"):
            raise AssertionError(f"{stage}: canonical expert evidence ref is missing or malformed")
        return ref

    expert_answer = by_stage.get("expert-answer")
    expert_follow_up = by_stage.get("expert-follow-up-after-restart")
    if expert_answer is None or expert_follow_up is None:
        raise AssertionError("Installed acceptance is missing expert continuity checkpoints")

    original_ref = evidence_ref("expert-answer", expert_answer)
    follow_up_ref = evidence_ref("expert-follow-up-after-restart", expert_follow_up)
    final_ref = evidence_ref("final", projection)
    if follow_up_ref != original_ref or final_ref != original_ref:
        raise AssertionError(
            "Process recreation or follow-up replaced the admitted canonical expert evidence ref"
        )


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
                       dialogue_state_json, expert_evidence_json, renderer_provenance,
                       providers_enabled, max_model_calls, provider_calls, model_calls
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
            if projection["dialogue_act"] != "expert_answer":
                raise AssertionError("Final pure-symbolic projection is not the expert why-follow-up")

            expert_evidence = json.loads(projection["expert_evidence_json"])
            if len(expert_evidence) != 1 or not isinstance(expert_evidence[0], dict):
                raise AssertionError("Final expert answer lost durable evidence")
            evidence_ref = expert_evidence[0].get("ref")
            if not isinstance(evidence_ref, str) or not evidence_ref.startswith("expert:"):
                raise AssertionError("Final expert answer has malformed evidence")

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
                "diagnosis(alex,flu)",
                "I answered from evidence expert:",
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
            "expert_evidence": expert_evidence,
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
    accounting_checkpoints: list[dict[str, object]] = []
    try:
        set_airplane_mode(device, True)
        if not airplane_mode_enabled(device):
            raise AssertionError("Pure-symbolic acceptance requires verified offline execution")

        # Boot once to create the private workspace, install only an ordinary registered
        # expert source, then restart so the production LocalZaraServer loads it normally.
        # The fixture does not write conversation projections or expert evidence.
        device.start()
        device.await_label("Ask anything…", timeout=30.0)
        install_acceptance_expert(device)
        device.adb("shell", "am", "force-stop", APP_PACKAGE)
        time.sleep(0.3)
        device.start()

        send_chat(device, "/symbolic on", "Pure symbolic mode enabled")
        device.capture("pure-symbolic-enabled")

        send_chat(device, "timer", "How long should I set the timer for?")
        device.capture("pure-symbolic-clarification")
        accounting_checkpoints.append(
            inspect_hard_zero_accounting(device, stage="clarification")
        )

        device.recreate()
        device.await_contains("How long should I set the timer for?", timeout=30.0)
        send_chat(
            device,
            "5 minutes",
            "That action needs capability-checked execution before I can report success.",
        )
        device.capture("pure-symbolic-follow-up-after-restart")
        accounting_checkpoints.append(
            inspect_hard_zero_accounting(device, stage="follow-up-after-restart")
        )

        device.recreate()
        device.await_contains("capability-checked execution", timeout=30.0)
        send_chat(device, "thanks", "welcome")
        device.capture("pure-symbolic-social-follow-up")
        accounting_checkpoints.append(
            inspect_hard_zero_accounting(device, stage="social-follow-up")
        )

        device.recreate()
        device.await_contains("welcome", timeout=30.0)
        send_chat(device, "frobnicate the moon", "handle that symbolically yet")
        device.capture("pure-symbolic-unsupported-no-fallback")
        accounting_checkpoints.append(
            inspect_hard_zero_accounting(device, stage="unsupported-no-fallback")
        )

        device.recreate()
        device.await_contains("handle that symbolically yet", timeout=30.0)
        send_chat(device, "inspect alex", "diagnosis(alex,flu)")
        device.capture("pure-symbolic-expert-answer")
        accounting_checkpoints.append(
            inspect_hard_zero_accounting(
                device,
                stage="expert-answer",
                expected_dialogue_act="expert_answer",
                require_expert_evidence=True,
            )
        )

        device.recreate()
        device.await_contains("diagnosis(alex,flu)", timeout=30.0)
        send_chat(device, "why?", "I answered from evidence expert:")
        device.capture("pure-symbolic-expert-follow-up-after-restart")
        accounting_checkpoints.append(
            inspect_hard_zero_accounting(
                device,
                stage="expert-follow-up-after-restart",
                expected_dialogue_act="expert_answer",
                require_expert_evidence=True,
            )
        )

        device.recreate()
        device.await_contains("I answered from evidence expert:", timeout=30.0)
        device.capture("pure-symbolic-final-recreated")

        projection = inspect_pure_symbolic_database(device, output)
        assert_checkpoint_continuity(accounting_checkpoints, projection)
        projection["offline_verified"] = True
        projection["accounting_checkpoints"] = accounting_checkpoints
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
