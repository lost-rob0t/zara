import importlib.util
import json
from pathlib import Path

import pytest


ROOT = Path(__file__).resolve().parents[1]


def _load_script(name: str, relative: str):
    spec = importlib.util.spec_from_file_location(name, ROOT / relative)
    assert spec is not None and spec.loader is not None
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)
    return module


release_facts = _load_script(
    "generate_autonomous_release_facts",
    "scripts/generate-autonomous-release-facts.py",
)
brave_facts = _load_script(
    "brave_results_to_prolog",
    "scripts/brave-results-to-prolog.py",
)
promote_release = _load_script(
    "promote_autonomous_release",
    "scripts/promote-autonomous-release.py",
)


def test_release_plan_generates_candidate_bound_prolog_facts():
    plan = {
        "schema": 1,
        "program_issue": 1357,
        "target_version": "0.3.0",
        "target_android_version_code": 6,
        "slices": [{"id": "pairing-e2e", "issue": 1358, "required": True}],
        "required_gates": ["android_apk_e2e"],
    }
    sha = "a" * 40

    output = release_facts.render(plan, sha, None)

    assert "program_issue(1357)." in output
    assert f"release_candidate_sha('{sha}')." in output
    assert "planned_slice('pairing-e2e', 1358, true)." in output
    assert "required_gate(android_apk_e2e)." in output


def test_release_evidence_must_bind_exact_candidate():
    plan = {
        "schema": 1,
        "program_issue": 1357,
        "target_version": "0.3.0",
        "target_android_version_code": 6,
        "slices": [{"id": "pairing-e2e", "issue": 1358, "required": True}],
        "required_gates": ["pairing_e2e"],
    }
    evidence = {
        "schema": 1,
        "candidate_sha": "b" * 40,
        "slices": {"pairing-e2e": "passed"},
        "gates": {"pairing_e2e": "passed"},
        "apk_source_sha": "b" * 40,
        "model_calls": 0,
        "unresolved_review_threads": 0,
    }

    with pytest.raises(ValueError, match="does not match"):
        release_facts.render(plan, "a" * 40, evidence)


def test_release_plan_rejects_duplicate_slice_ids():
    plan = {
        "schema": 1,
        "program_issue": 1357,
        "target_version": "0.3.0",
        "target_android_version_code": 6,
        "slices": [
            {"id": "same", "issue": 1},
            {"id": "same", "issue": 2},
        ],
        "required_gates": ["core_tests"],
    }

    with pytest.raises(ValueError, match="duplicate slice"):
        release_facts.render(plan, "a" * 40, None)


def test_brave_results_are_deduplicated_and_string_escaped():
    payload = {
        "web": {
            "results": [
                {
                    "url": "HTTPS://Example.COM/a#fragment",
                    "title": "a'); halt. %",
                    "description": "line one\nline two",
                },
                {
                    "url": "https://example.com/a",
                    "title": "duplicate",
                    "description": "duplicate",
                },
                {
                    "url": "https://example.org/b?q=1",
                    "title": "second",
                    "description": "safe",
                },
            ]
        }
    }

    output = brave_facts.convert(
        payload,
        "zara expert systems",
        "2026-09-21T03:00:00Z",
        20,
    )

    assert output.count("search_evidence(") == 2
    assert '"https://example.com/a"' in output
    assert '"https://example.org/b?q=1"' in output
    assert '"a\'); halt. %"' in output
    assert '"line one\\nline two"' in output


def test_brave_results_reject_non_http_urls():
    payload = {
        "web": {
            "results": [
                {
                    "url": "file:///etc/passwd",
                    "title": "bad",
                    "description": "bad",
                }
            ]
        }
    }

    with pytest.raises(ValueError, match="http"):
        brave_facts.convert(payload, "query", "2026-09-21T03:00:00Z", 20)


def test_committed_autonomous_plan_is_valid_json():
    plan = json.loads((ROOT / "release/autonomous-plan.json").read_text())
    assert plan["schema"] == 1
    assert plan["program_issue"] == 1357
    assert plan["target_version"] == "0.3.0"
    assert len(plan["slices"]) >= 7


def test_release_promotion_uses_declared_target_only():
    plan = {
        "schema": 1,
        "target_version": "0.3.0",
        "target_android_version_code": 6,
    }
    current = {
        "schema": "1",
        "zara.version": "0.2.2-alpha",
        "android.versionCode": "4",
        "release.target": "0.2.2-alpha",
        "release.targetAndroidVersionCode": "4",
    }

    output = promote_release.promoted_text(plan, current)

    assert "zara.version=0.3.0" in output
    assert "android.versionCode=6" in output
    assert "release.target=0.3.0" in output
    assert "release.targetAndroidVersionCode=6" in output


def test_release_promotion_rejects_android_code_regression():
    plan = {
        "schema": 1,
        "target_version": "0.3.0",
        "target_android_version_code": 3,
    }
    current = {
        "schema": "1",
        "zara.version": "0.2.2-alpha",
        "android.versionCode": "4",
        "release.target": "0.2.2-alpha",
        "release.targetAndroidVersionCode": "4",
    }

    with pytest.raises(ValueError, match="regresses"):
        promote_release.promoted_text(plan, current)


def test_release_promotion_cuts_unreleased_changelog():
    source = (
        "# Zara Changelog\n\n"
        "## Unreleased\n\n"
        "### Added\n\n"
        "- New thing.\n\n"
        "## 0.2.2-alpha\n\n"
        "- Old thing.\n"
    )

    output = promote_release.promote_changelog(source, "0.3.0")

    assert "## Unreleased\n\n## 0.3.0" in output
    assert "### Added\n\n- New thing." in output
    assert output.index("## 0.3.0") < output.index("## 0.2.2-alpha")
