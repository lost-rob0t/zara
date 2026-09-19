from pathlib import Path


ACCEPTANCE = Path("android/integration/org_device_acceptance.py")
VALIDATOR = Path("scripts/validate-org-ui-evidence.py")
WORKFLOW = Path(".github/workflows/org-android-ui.yml")


def test_org_acceptance_uses_real_saf_ordinary_org_corpus_not_product_store():
    text = ACCEPTANCE.read_text(encoding="utf-8")
    assert 'corpus_authority": "ordinary Org files through persisted Android SAF"' in text
    assert 'ActivityResultContracts.OpenDocumentTree' not in text
    assert 'device.tap("Choose Org directory")' in text
    assert '"Use this folder"' in text
    assert '"fixture_root_is_test_only": True' in text
    assert "Room" not in text
    assert "SQLite" not in text


def test_org_acceptance_captures_todo_roam_and_scrolled_separate_dailies_with_text_twins():
    text = ACCEPTANCE.read_text(encoding="utf-8")
    for state in (
        "org-todo",
        "org-roam",
        "org-daily-today",
        "org-daily-previous",
    ):
        assert f'"{state}"' in text
    assert 'device.tap("Roam")' in text
    assert 'device.tap("Daily")' in text
    assert "device.reveal(previous)" in text
    assert 'f"{name}.txt"' in text
    assert '"sha256"' in text


def test_org_acceptance_daily_layout_is_acceptance_configuration_not_a_product_default():
    text = ACCEPTANCE.read_text(encoding="utf-8")
    assert '"daily-path-template">daily/{date}.org' in text
    assert '"daily-zone-id">UTC' in text
    assert 'FIXTURE_RELATIVE_ROOT = "Documents/ZaraOrgAcceptance"' in text
    assert "Documents/Notes/org" not in text
    assert "fixture root exists only inside the disposable emulator" in text


def test_org_evidence_validator_requires_exact_sha_pass_and_all_text_twins():
    text = VALIDATOR.read_text(encoding="utf-8")
    assert 'manifest.get("source_sha") != source_sha' in text
    assert 'manifest.get("passed") is not True' in text
    assert "REQUIRED_STATES" in text
    assert 'manifest.get("corpus_authority")' in text
    assert 'manifest.get("fixture_root_is_test_only") is not True' in text
    assert '"text_evidence"' in text


def test_org_evidence_workflow_checks_out_and_names_artifact_by_exact_pr_head():
    text = WORKFLOW.read_text(encoding="utf-8")
    assert "SOURCE_SHA: ${{ github.event.pull_request.head.sha }}" in text
    assert "ref: ${{ github.event.pull_request.head.sha }}" in text
    assert 'test "$actual" = "$SOURCE_SHA"' in text
    assert ":org-app:testDebugUnitTest :org-app:assembleDebug" in text
    assert "org_device_acceptance.py" in text
    assert "validate-org-ui-evidence.py" in text
    assert "org-android-ui-evidence-${{ github.event.pull_request.head.sha }}" in text
