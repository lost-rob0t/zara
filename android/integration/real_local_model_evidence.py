#!/usr/bin/env python3
"""Validate installed-device evidence for real local model inference."""

from __future__ import annotations

import argparse
import json
from pathlib import Path
import re
import sys
from typing import Any


SHA1_RE = re.compile(r"[0-9a-f]{40}")
SHA256_RE = re.compile(r"[0-9a-f]{64}")
MODEL_ID_RE = re.compile(r"[A-Za-z0-9._-]{1,96}")
MODEL_VERSION_RE = re.compile(r"[A-Za-z0-9._+-]{1,64}")
SUPPORTED_QUANTIZATIONS = {
    "dynamic-int4",
    "int4",
    "int8",
    "fp8",
    "bf16",
    "fp16",
    "fp32",
    "q8_0",
    "q6_k",
    "q5_k_m",
    "q5_k_s",
    "q4_k_m",
    "q4_k_s",
    "q4_0",
    "q3_k_m",
    "q2_k",
}
SUPPORTED_BACKENDS = {"CPU", "GPU", "NPU"}
REQUIRED_LIFECYCLE_PROOFS = (
    "load_succeeded",
    "unload_reached_stopped",
    "reload_reached_ready",
    "cancel_observed",
    "no_late_chunk_after_cancel",
    "no_late_final_after_cancel",
    "stale_generation_fenced",
    "voice_owner_recreated",
    "oom_failure_observed",
    "oom_recovered_without_remote",
)
REQUIRED_EVIDENCE_STATES = {
    "real-local-inference",
    "real-local-cancel-recovery",
}


class EvidenceValidationError(ValueError):
    pass


def _require_object(value: object, label: str) -> dict[str, Any]:
    if not isinstance(value, dict):
        raise EvidenceValidationError(f"{label} must be an object")
    return value


def _require_nonempty_string(value: object, label: str) -> str:
    if not isinstance(value, str) or not value.strip():
        raise EvidenceValidationError(f"{label} is required")
    return value.strip()


def _require_hash(value: object, label: str, pattern: re.Pattern[str]) -> str:
    if not isinstance(value, str) or pattern.fullmatch(value) is None:
        raise EvidenceValidationError(f"{label} is invalid")
    return value


def _require_true(value: object, label: str) -> None:
    if value is not True:
        raise EvidenceValidationError(f"lifecycle proof {label} must be true")


def _require_zero(value: object, label: str) -> None:
    if type(value) is not int or value != 0:
        raise EvidenceValidationError(f"{label} must be zero")


def validate_manifest(manifest: object) -> None:
    root = _require_object(manifest, "manifest")
    if root.get("schema") != 1:
        raise EvidenceValidationError("schema must be 1")
    _require_hash(root.get("source_sha"), "source_sha", SHA1_RE)
    _require_hash(root.get("phone_apk_sha256"), "phone_apk_sha256", SHA256_RE)

    model = _require_object(root.get("model"), "model")
    model_id = _require_nonempty_string(model.get("id"), "model id")
    if MODEL_ID_RE.fullmatch(model_id) is None:
        raise EvidenceValidationError("model id is invalid")
    model_version = _require_nonempty_string(model.get("version"), "model version")
    if MODEL_VERSION_RE.fullmatch(model_version) is None:
        raise EvidenceValidationError("model version is invalid")
    if model.get("format") != "litert-lm":
        raise EvidenceValidationError("model format must be litert-lm")
    quantization = model.get("quantization")
    if quantization not in SUPPORTED_QUANTIZATIONS:
        raise EvidenceValidationError("model quantization is unsupported")
    backend = model.get("backend")
    if backend not in SUPPORTED_BACKENDS:
        raise EvidenceValidationError("model backend is unsupported")
    _require_hash(model.get("sha256"), "model sha256", SHA256_RE)

    routing = _require_object(root.get("routing"), "routing")
    if routing.get("runtime_mode") != "Local":
        raise EvidenceValidationError("routing runtime_mode must be Local")
    if routing.get("symbolic_result") != "miss":
        raise EvidenceValidationError("routing symbolic_result must be miss")
    if routing.get("local_model_started") is not True:
        raise EvidenceValidationError("routing must prove the local model started")
    if routing.get("outbound_network_denied") is not True:
        raise EvidenceValidationError("routing must prove outbound network was denied")
    _require_zero(routing.get("remote_route_calls"), "Remote route calls")
    _require_zero(routing.get("provider_calls"), "provider_calls")

    inference = _require_object(root.get("inference"), "inference")
    _require_nonempty_string(inference.get("text"), "generated text")
    if inference.get("model_id") != model_id or inference.get("model_version") != model_version:
        raise EvidenceValidationError("inference model identity does not match the installed model")
    if inference.get("quantization") != quantization:
        raise EvidenceValidationError("inference quantization does not match the installed model")
    if inference.get("backend") != backend:
        raise EvidenceValidationError("inference backend does not match the installed model")

    lifecycle = _require_object(root.get("lifecycle"), "lifecycle")
    for proof in REQUIRED_LIFECYCLE_PROOFS:
        _require_true(lifecycle.get(proof), proof)

    evidence = root.get("evidence")
    if not isinstance(evidence, list) or not evidence:
        raise EvidenceValidationError("evidence must contain rendered states")
    seen_states: set[str] = set()
    seen_hashes: set[str] = set()
    for index, value in enumerate(evidence):
        entry = _require_object(value, f"evidence[{index}]")
        state = _require_nonempty_string(entry.get("state"), f"evidence[{index}] state")
        if state in seen_states:
            raise EvidenceValidationError(f"evidence state is duplicated: {state}")
        seen_states.add(state)
        for field in ("screenshot_sha256", "text_sha256", "action_sha256"):
            digest = _require_hash(entry.get(field), f"evidence {state} {field}", SHA256_RE)
            if digest in seen_hashes:
                raise EvidenceValidationError(f"evidence hash is duplicated: {digest}")
            seen_hashes.add(digest)
    missing_states = REQUIRED_EVIDENCE_STATES - seen_states
    if missing_states:
        raise EvidenceValidationError(
            f"evidence states are missing: {', '.join(sorted(missing_states))}"
        )


def _load_manifest(path: Path) -> object:
    try:
        return json.loads(path.read_text(encoding="utf-8"))
    except (OSError, json.JSONDecodeError) as error:
        raise EvidenceValidationError(f"manifest is unreadable: {path}: {error}") from error


def main() -> int:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("manifest", type=Path)
    args = parser.parse_args()
    try:
        validate_manifest(_load_manifest(args.manifest))
    except EvidenceValidationError as error:
        print(f"Real local model evidence invalid: {error}", file=sys.stderr)
        return 2
    print(f"Real local model evidence valid: {args.manifest}")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
