#!/usr/bin/env python3
"""Validate exact-SHA desktop and Android screenshot evidence."""

from __future__ import annotations

import argparse
import hashlib
import json
from pathlib import Path
import re
import sys
from typing import Any


PNG_SIGNATURE = b"\x89PNG\r\n\x1a\n"
ANDROID_SCENARIO_ID = re.compile(r"android\.ui\.([a-z0-9][a-z0-9-]*)")
RUNTIME_EVIDENCE_FIELDS = ("mode", "runtime_id", "model", "quantization", "phase")


class EvidenceError(ValueError):
    pass


def _load_manifest(path: Path) -> dict[str, Any]:
    if not path.is_file():
        raise EvidenceError(f"manifest is missing: {path}")
    try:
        payload = json.loads(path.read_text(encoding="utf-8"))
    except (OSError, json.JSONDecodeError) as error:
        raise EvidenceError(f"manifest is unreadable: {path}: {error}") from error
    if not isinstance(payload, dict):
        raise EvidenceError(f"manifest must contain a JSON object: {path}")
    return payload


def _safe_child(root: Path, name: str) -> Path:
    candidate = (root / name).resolve()
    resolved_root = root.resolve()
    try:
        candidate.relative_to(resolved_root)
    except ValueError as error:
        raise EvidenceError(f"evidence path escapes manifest directory: {name}") from error
    return candidate


def _require_png(path: Path) -> bytes:
    if not path.is_file():
        raise EvidenceError(f"screenshot is missing: {path}")
    data = path.read_bytes()
    if len(data) <= len(PNG_SIGNATURE) or not data.startswith(PNG_SIGNATURE):
        raise EvidenceError(f"screenshot is not a PNG: {path}")
    return data


def _require_sha256(value: object, *, label: str) -> str:
    if not isinstance(value, str) or len(value) != 64:
        raise EvidenceError(f"{label} hash is invalid")
    try:
        int(value, 16)
    except ValueError as error:
        raise EvidenceError(f"{label} hash is invalid") from error
    return value.lower()


def _require_hashed_file(
    root: Path,
    evidence: object,
    *,
    label: str,
) -> tuple[str, str]:
    if not isinstance(evidence, dict):
        raise EvidenceError(f"{label} evidence is missing")
    file_value = evidence.get("file")
    if not isinstance(file_value, str) or not file_value:
        raise EvidenceError(f"{label} evidence filename is missing")
    expected_hash = _require_sha256(evidence.get("sha256"), label=label)
    path = _safe_child(root, file_value)
    if not path.is_file():
        raise EvidenceError(f"scenario evidence file is missing: {path}")
    actual_hash = hashlib.sha256(path.read_bytes()).hexdigest()
    if actual_hash != expected_hash:
        raise EvidenceError(
            f"{label} hash mismatch: expected {expected_hash}, got {actual_hash}"
        )
    return file_value, expected_hash


def _require_text(path: Path, *, label: str) -> str:
    try:
        return path.read_text(encoding="utf-8")
    except (OSError, UnicodeError) as error:
        raise EvidenceError(f"{label} is not valid UTF-8 text: {path}: {error}") from error


def _assertion_trace(actions: list[str], assertions: list[dict[str, Any]]) -> str:
    lines = [f"ACTION {index} {action}" for index, action in enumerate(actions, 1)]
    lines.extend(
        " ".join(
            (
                "ASSERT",
                "PASS" if assertion["passed"] else "FAIL",
                assertion["name"],
                assertion["detail"],
            )
        ).rstrip()
        for assertion in assertions
    )
    return "\n".join(lines) + ("\n" if lines else "")


def validate_desktop(manifest_path: Path, source_sha: str) -> int:
    manifest = _load_manifest(manifest_path)
    fixtures = manifest.get("fixtures")
    if not isinstance(fixtures, list) or not fixtures:
        raise EvidenceError("desktop manifest must contain at least one fixture")

    seen_states: set[str] = set()
    seen_paths: set[str] = set()
    for entry in fixtures:
        if not isinstance(entry, dict):
            raise EvidenceError("desktop fixture entry must be an object")
        state = entry.get("state")
        path_value = entry.get("path")
        entry_sha = entry.get("source_commit")
        expected_hash = entry.get("sha256")
        width = entry.get("width")
        height = entry.get("height")
        if not isinstance(state, str) or not state:
            raise EvidenceError("desktop fixture state is missing")
        if state in seen_states:
            raise EvidenceError(f"desktop fixture state is duplicated: {state}")
        seen_states.add(state)
        if not isinstance(path_value, str) or not path_value:
            raise EvidenceError(f"desktop fixture path is missing: {state}")
        if path_value in seen_paths:
            raise EvidenceError(f"desktop fixture path is duplicated: {path_value}")
        seen_paths.add(path_value)
        if entry_sha != source_sha:
            raise EvidenceError(
                f"desktop source commit mismatch for {state}: expected {source_sha}, got {entry_sha}"
            )
        if not isinstance(width, int) or width <= 0 or not isinstance(height, int) or height <= 0:
            raise EvidenceError(f"desktop fixture has invalid dimensions: {state}")
        expected_hash = _require_sha256(expected_hash, label=f"desktop screenshot {state}")
        data = _require_png(_safe_child(manifest_path.parent, path_value))
        actual_hash = hashlib.sha256(data).hexdigest()
        if actual_hash != expected_hash:
            raise EvidenceError(
                f"desktop screenshot hash mismatch for {state}: expected {expected_hash}, got {actual_hash}"
            )
    return len(fixtures)


def validate_android(
    manifest_path: Path,
    source_sha: str,
    *,
    include_supplemental: bool = True,
) -> int:
    manifest = _load_manifest(manifest_path)
    if manifest.get("source_sha") != source_sha:
        raise EvidenceError(
            f"android source SHA mismatch: expected {source_sha}, got {manifest.get('source_sha')}"
        )
    manifest_apk_sha256 = _require_sha256(
        manifest.get("apk_sha256"), label="android manifest apk_sha256"
    )
    if manifest.get("passed") is not True:
        raise EvidenceError("android acceptance manifest must record passed=true")
    screenshots = manifest.get("screenshots")
    if not isinstance(screenshots, list) or not screenshots:
        raise EvidenceError("android manifest must contain at least one screenshot")

    seen_states: set[str] = set()
    seen_files: set[str] = set()
    screenshots_by_state: dict[str, dict[str, Any]] = {}
    for entry in screenshots:
        if not isinstance(entry, dict):
            raise EvidenceError("android screenshot entry must be an object")
        state = entry.get("state")
        file_value = entry.get("file")
        expected_hash = entry.get("sha256")
        if not isinstance(state, str) or not state:
            raise EvidenceError("android screenshot state is missing")
        if state in seen_states:
            raise EvidenceError(f"android screenshot state is duplicated: {state}")
        seen_states.add(state)
        if not isinstance(file_value, str) or not file_value:
            raise EvidenceError(f"android screenshot filename is missing: {state}")
        if file_value in seen_files:
            raise EvidenceError(f"android screenshot filename is duplicated: {file_value}")
        seen_files.add(file_value)
        expected_hash = _require_sha256(expected_hash, label=f"android screenshot {state}")
        data = _require_png(_safe_child(manifest_path.parent, file_value))
        actual_hash = hashlib.sha256(data).hexdigest()
        if actual_hash != expected_hash:
            raise EvidenceError(
                f"android screenshot hash mismatch for {state}: expected {expected_hash}, got {actual_hash}"
            )
        screenshots_by_state[state] = entry

    scenarios = manifest.get("scenarios")
    if not isinstance(scenarios, list) or not scenarios:
        raise EvidenceError("android manifest omitted per-scenario evidence")

    manifest_device_api = manifest.get("device", {}).get("api")
    seen_scenario_ids: set[str] = set()
    seen_scenario_states: set[str] = set()
    for scenario in scenarios:
        if not isinstance(scenario, dict):
            raise EvidenceError("android scenario entry must be an object")
        scenario_id = scenario.get("scenario_id")
        if not isinstance(scenario_id, str):
            raise EvidenceError("android scenario id is missing")
        match = ANDROID_SCENARIO_ID.fullmatch(scenario_id)
        if match is None:
            raise EvidenceError(f"android scenario id is invalid: {scenario_id}")
        if scenario_id in seen_scenario_ids:
            raise EvidenceError(f"duplicate scenario id: {scenario_id}")
        seen_scenario_ids.add(scenario_id)
        state = match.group(1)
        if state in seen_scenario_states:
            raise EvidenceError(f"duplicate scenario state: {state}")
        seen_scenario_states.add(state)

        if scenario.get("source_sha") != source_sha:
            raise EvidenceError(f"android scenario source SHA mismatch: {scenario_id}")
        scenario_apk_sha256 = _require_sha256(
            scenario.get("apk_sha256"),
            label=f"android scenario apk_sha256 {scenario_id}",
        )
        if scenario_apk_sha256 != manifest_apk_sha256:
            raise EvidenceError(f"android scenario apk_sha256 mismatch: {scenario_id}")
        if scenario.get("device_api") != manifest_device_api:
            raise EvidenceError(f"android scenario device API mismatch: {scenario_id}")
        profile = scenario.get("profile")
        if not isinstance(profile, str) or not profile:
            raise EvidenceError(f"android scenario profile is missing: {scenario_id}")
        route = scenario.get("route")
        if not isinstance(route, str) or not route.strip():
            raise EvidenceError(f"android scenario route is missing: {scenario_id}")
        runtime = scenario.get("runtime")
        if not isinstance(runtime, dict):
            raise EvidenceError(f"android scenario runtime is missing: {scenario_id}")
        for field in RUNTIME_EVIDENCE_FIELDS:
            if field not in runtime:
                raise EvidenceError(
                    f"android scenario runtime {field} is missing: {scenario_id}"
                )
            value = runtime[field]
            if value is not None and (not isinstance(value, str) or not value):
                raise EvidenceError(
                    f"android scenario runtime {field} is invalid: {scenario_id}"
                )

        actions = scenario.get("actions")
        if not isinstance(actions, list) or not actions or not all(
            isinstance(action, str) and action for action in actions
        ):
            raise EvidenceError(f"android scenario action evidence is missing: {scenario_id}")
        assertions = scenario.get("assertions")
        if not isinstance(assertions, list) or not assertions:
            raise EvidenceError(f"android scenario assertion evidence is missing: {scenario_id}")
        screenshot_assertion = False
        for assertion in assertions:
            if not isinstance(assertion, dict):
                raise EvidenceError(f"android scenario assertion is malformed: {scenario_id}")
            if not isinstance(assertion.get("name"), str) or not assertion["name"]:
                raise EvidenceError(f"android scenario assertion name is missing: {scenario_id}")
            if not isinstance(assertion.get("passed"), bool):
                raise EvidenceError(f"android scenario assertion result is invalid: {scenario_id}")
            if not isinstance(assertion.get("detail"), str):
                raise EvidenceError(f"android scenario assertion detail is invalid: {scenario_id}")
            if assertion["passed"] is False:
                raise EvidenceError(
                    f"android scenario contains failed assertion: {scenario_id}: "
                    f"{assertion['name']}"
                )
            if assertion["name"] == "screenshot-png" and assertion["passed"] is True:
                screenshot_assertion = True
        if not screenshot_assertion:
            raise EvidenceError(f"android scenario omitted successful screenshot assertion: {scenario_id}")

        screenshot_file, screenshot_hash = _require_hashed_file(
            manifest_path.parent,
            scenario.get("screenshot"),
            label=f"android scenario screenshot {scenario_id}",
        )
        text_file, _ = _require_hashed_file(
            manifest_path.parent,
            scenario.get("text_evidence"),
            label=f"android scenario text {scenario_id}",
        )
        assertion_file, _ = _require_hashed_file(
            manifest_path.parent,
            scenario.get("assertion_evidence"),
            label=f"android scenario assertions {scenario_id}",
        )

        text_lines = _require_text(
            _safe_child(manifest_path.parent, text_file),
            label=f"android scenario text {scenario_id}",
        ).splitlines()
        expected_route = f"route={json.dumps(route, ensure_ascii=False)}"
        expected_runtime = (
            "runtime="
            + json.dumps(runtime, sort_keys=True, separators=(",", ":"))
        )
        if not text_lines or text_lines[0] != expected_route:
            raise EvidenceError(f"android scenario text route mismatch: {scenario_id}")
        if len(text_lines) < 2 or text_lines[1] != expected_runtime:
            raise EvidenceError(f"android scenario text runtime mismatch: {scenario_id}")

        actual_assertion_trace = _require_text(
            _safe_child(manifest_path.parent, assertion_file),
            label=f"android scenario assertion trace {scenario_id}",
        )
        if actual_assertion_trace != _assertion_trace(actions, assertions):
            raise EvidenceError(f"android scenario assertion trace mismatch: {scenario_id}")

        screenshot_entry = screenshots_by_state.get(state)
        if screenshot_entry is None:
            raise EvidenceError(f"android scenario has no screenshot manifest entry: {scenario_id}")
        if (
            screenshot_entry.get("file") != screenshot_file
            or screenshot_entry.get("sha256") != screenshot_hash
        ):
            raise EvidenceError(
                f"scenario screenshot differs from screenshot manifest: {scenario_id}"
            )

        scenario_path = _safe_child(manifest_path.parent, f"{state}.json")
        persisted_scenario = _load_manifest(scenario_path)
        if persisted_scenario != scenario:
            raise EvidenceError(f"persisted scenario record differs from manifest: {scenario_id}")

    if seen_scenario_states != seen_states:
        missing = sorted(seen_states - seen_scenario_states)
        extra = sorted(seen_scenario_states - seen_states)
        raise EvidenceError(
            f"android scenario/screenshot state mismatch: missing={missing} extra={extra}"
        )

    count = len(screenshots)
    if include_supplemental:
        supplemental_path = manifest_path.with_name("remote-manifest.json")
        if supplemental_path != manifest_path and supplemental_path.is_file():
            count += validate_android(
                supplemental_path,
                source_sha,
                include_supplemental=False,
            )
    return count


def main() -> int:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--source-sha", required=True)
    parser.add_argument("--desktop", type=Path, required=True)
    parser.add_argument("--android", type=Path, required=True)
    args = parser.parse_args()

    try:
        desktop_count = validate_desktop(args.desktop, args.source_sha)
        android_count = validate_android(args.android, args.source_sha)
    except EvidenceError as error:
        print(f"UI evidence validation failed: {error}", file=sys.stderr)
        return 1

    print(
        "UI evidence valid: "
        f"desktop={desktop_count} android={android_count} source_sha={args.source_sha}"
    )
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
