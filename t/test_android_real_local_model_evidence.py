from __future__ import annotations

import importlib.util
import json
from pathlib import Path
import subprocess
import sys

import pytest


ROOT = Path(__file__).resolve().parents[1]
VALIDATOR = ROOT / "android" / "integration" / "real_local_model_evidence.py"
HEX40 = "1" * 40
HEX64 = "a" * 64


def _load_validator():
    spec = importlib.util.spec_from_file_location("zara_real_local_model_evidence", VALIDATOR)
    assert spec is not None
    assert spec.loader is not None
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)
    return module


def valid_manifest() -> dict[str, object]:
    return {
        "schema": 1,
        "source_sha": HEX40,
        "phone_apk_sha256": HEX64,
        "model": {
            "id": "gemma3n-test",
            "version": "1.0",
            "format": "litert-lm",
            "quantization": "int4",
            "backend": "CPU",
            "sha256": "b" * 64,
        },
        "routing": {
            "runtime_mode": "Local",
            "symbolic_result": "miss",
            "local_model_started": True,
            "outbound_network_denied": True,
            "remote_route_calls": 0,
            "provider_calls": 0,
        },
        "inference": {
            "text": "local model answer",
            "model_id": "gemma3n-test",
            "model_version": "1.0",
            "quantization": "int4",
            "backend": "CPU",
        },
        "lifecycle": {
            "load_succeeded": True,
            "unload_reached_stopped": True,
            "reload_reached_ready": True,
            "cancel_observed": True,
            "no_late_chunk_after_cancel": True,
            "no_late_final_after_cancel": True,
            "stale_generation_fenced": True,
            "voice_owner_recreated": True,
            "oom_failure_observed": True,
            "oom_recovered_without_remote": True,
        },
        "evidence": [
            {
                "state": "real-local-inference",
                "screenshot_sha256": "c" * 64,
                "text_sha256": "d" * 64,
                "action_sha256": "e" * 64,
            },
            {
                "state": "real-local-cancel-recovery",
                "screenshot_sha256": "f" * 64,
                "text_sha256": "0" * 64,
                "action_sha256": "1" * 64,
            },
        ],
    }


def test_valid_real_local_model_manifest_is_accepted() -> None:
    module = _load_validator()
    module.validate_manifest(valid_manifest())


@pytest.mark.parametrize(
    ("mutation", "message"),
    [
        (lambda m: m["model"].update({"format": "gguf"}), "format"),
        (lambda m: m["model"].update({"quantization": "mystery-int4"}), "quantization"),
        (lambda m: m["routing"].update({"outbound_network_denied": False}), "network"),
        (lambda m: m["routing"].update({"remote_route_calls": 1}), "Remote route"),
        (lambda m: m["routing"].update({"provider_calls": 1}), "provider"),
        (lambda m: m["inference"].update({"text": "   "}), "generated text"),
        (lambda m: m["inference"].update({"model_id": "other"}), "model identity"),
        (lambda m: m["lifecycle"].update({"no_late_chunk_after_cancel": False}), "lifecycle"),
        (lambda m: m["lifecycle"].update({"oom_recovered_without_remote": False}), "lifecycle"),
    ],
)
def test_manifest_fails_closed_on_unproven_real_model_boundaries(mutation, message: str) -> None:
    module = _load_validator()
    manifest = valid_manifest()
    mutation(manifest)
    with pytest.raises(module.EvidenceValidationError, match=message):
        module.validate_manifest(manifest)


def test_manifest_rejects_duplicate_evidence_state_and_hashes() -> None:
    module = _load_validator()
    manifest = valid_manifest()
    manifest["evidence"][1]["state"] = manifest["evidence"][0]["state"]
    manifest["evidence"][1]["screenshot_sha256"] = manifest["evidence"][0]["screenshot_sha256"]
    with pytest.raises(module.EvidenceValidationError, match="evidence state"):
        module.validate_manifest(manifest)


def test_cli_rejects_missing_required_proof(tmp_path: Path) -> None:
    manifest = valid_manifest()
    manifest["routing"].pop("provider_calls")
    path = tmp_path / "manifest.json"
    path.write_text(json.dumps(manifest), encoding="utf-8")

    result = subprocess.run(
        [sys.executable, str(VALIDATOR), str(path)],
        text=True,
        capture_output=True,
        check=False,
    )

    assert result.returncode == 2
    assert "provider_calls" in result.stderr
