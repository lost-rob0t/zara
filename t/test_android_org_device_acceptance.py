from pathlib import Path


ACCEPTANCE = Path("android/integration/org_device_acceptance.py")
RUNNER = Path("android/integration/org_device_acceptance_runner.py")
VALIDATOR = Path("scripts/validate-org-ui-evidence.py")
WORKFLOW = Path(".github/workflows/org-android-ui.yml")


def test_org_acceptance_uses_real_saf_ordinary_org_corpus_not_product_store():
    text = ACCEPTANCE.read_text(encoding="utf-8")
    assert 'corpus_authority\": \"ordinary Org files through persisted Android SAF\"' in text
    assert 'ActivityResultContracts.OpenDocumentTree' not in text
    assert '_tap_app_control_through_launcher_anr(device, "Choose Org directory")' in text
    assert '"Use this folder"' in text
    assert '"fixture_root_is_test_only": True' in text
    assert "Room" not in text
    assert "SQLite" not in text


def test_org_acceptance_navigates_picker_when_root_selection_is_disabled():
    text = ACCEPTANCE.read_text(encoding="utf-8")
    # Hosted API-35 may ignore EXTRA_INITIAL_URI and open the picker at storage
    # root, where Android explicitly disables "Use this folder". Evidence must
    # navigate the disposable acceptance fixture rather than tap a disabled root.
    assert 'device.find_contains("Can’t use this folder") is not None' in text
    assert '_tap_contains(device, "Documents")' in text
    assert '_tap_contains(device, "ZaraOrgAcceptance")' in text


def test_org_acceptance_matches_documentsui_actions_case_insensitively_without_weakening_app_assertions():
    text = ACCEPTANCE.read_text(encoding="utf-8")
    # AOSP DocumentsUI may expose action labels as USE THIS FOLDER / ALLOW while
    # other images use title case. Scope normalization to OS-owned picker actions;
    # app-owned Org assertions must keep the shared exact/case-sensitive helper.
    assert "def _find_picker_action" in text
    assert "action.strip().casefold()" in text
    assert '(node.get(attribute) or "").strip().casefold() == needle' in text
    assert "def _await_picker_action" in text
    assert "def _tap_picker_action" in text
    assert '_await_picker_action(device, "Use this folder")' in text
    assert '_tap_picker_action(device, "Use this folder")' in text
    assert '_await_picker_action(device, "Allow")' in text
    assert '_tap_picker_action(device, "Allow")' in text
    assert 'device.await_contains("Acceptance task", timeout=20.0)' in text


def test_org_acceptance_picker_actions_do_not_match_dialog_titles_by_substring():
    text = ACCEPTANCE.read_text(encoding="utf-8")
    # The confirmation title itself begins with "Allow ...". A substring matcher
    # can tap that title instead of the ALLOW button and leave SAF unconfirmed.
    assert "def _find_picker_action" in text
    action_block = text.split("def _find_picker_action", 1)[1].split(
        "def _await_picker_action", 1
    )[0]
    assert ".strip().casefold() == needle" in action_block
    assert "needle in" not in action_block


def test_org_acceptance_bounds_documentsui_null_root_recovery():
    text = ACCEPTANCE.read_text(encoding="utf-8")
    # API-35 DocumentsUI can briefly leave UIAutomator without a root while the
    # picker window is attaching. Recover only that hierarchy-read failure, with
    # a tiny fixed budget; never turn the evidence gate into retry-until-green.
    assert "PICKER_UI_DUMP_ATTEMPTS = 3" in text
    assert "def _picker_nodes" in text
    assert "for attempt in range(PICKER_UI_DUMP_ATTEMPTS)" in text
    assert '"UIAutomator did not create" not in str(error)' in text
    assert "raise" in text
    assert "time.sleep(0.2)" in text


def test_org_acceptance_runner_has_one_fail_closed_direct_stream_fallback():
    text = RUNNER.read_text(encoding="utf-8")
    # Hosted API-35 can report a successful file-backed UIAutomator dump without
    # materializing the file. The Org evidence runner gets one direct /dev/tty
    # stream fallback, and only for that exact failure mode.
    assert "class OrgEvidenceDevice(Device)" in text
    assert '"UIAutomator did not create" not in str(error)' in text
    assert 'self.adb("exec-out", "uiautomator", "dump", "/dev/tty")' in text
    assert 'hierarchy.find("<?xml")' in text
    assert 'hierarchy.rfind("</hierarchy>")' in text
    assert "ET.fromstring" in text
    assert "org_acceptance.Device = OrgEvidenceDevice" in text


def test_org_acceptance_captures_workspace_navigation_and_editor_modes_with_text_twins():
    text = ACCEPTANCE.read_text(encoding="utf-8")
    for state in (
        "org-todo",
        "org-drawer",
        "org-roam",
        "org-daily-today",
        "org-daily-previous",
        "org-reminders",
        "org-timers",
        "org-graph",
        "org-editor-pages",
        "org-editor-page",
        "org-editor-block-edit",
        "org-editor-raw",
        "org-editor-config",
        "org-home",
    ):
        assert f'"{state}"' in text
    assert 'device.tap("Menu")' in text
    assert 'open_surface(device, "Editor")' in text
    assert 'device.tap("Raw")' in text
    assert 'device.tap("config.pl")' in text
    assert "device.reveal(previous)" in text
    assert 'f"{name}.txt"' in text
    assert '"sha256"' in text


def test_org_acceptance_daily_layout_is_acceptance_configuration_not_a_product_default():
    text = ACCEPTANCE.read_text(encoding="utf-8")
    assert "daily/{date}.org" in text
    assert "yyyy-MM-dd" in text
    assert "UTC" in text
    assert 'FIXTURE_RELATIVE_ROOT = "Documents/ZaraOrgAcceptance"' in text
    assert "Documents/Notes/org" not in text
    assert "fixture path exists only inside the disposable emulator" in text


def test_org_acceptance_fixture_uploads_do_not_use_nested_adb_shell_quoting():
    text = ACCEPTANCE.read_text(encoding="utf-8")
    compact = " ".join(text.split())
    # adb shell reparses command arguments. Passing a multi-word script as the
    # argument to remote `sh -c` loses the intended command boundary on hosted
    # emulators, so fixture writes must use adb push + direct argv operations.
    assert '"sh", "-c",' not in compact
    assert '"push",' in text
    assert '"run-as", PACKAGE, "cp",' in compact


def test_org_evidence_validator_requires_exact_sha_pass_and_all_text_twins():
    text = VALIDATOR.read_text(encoding="utf-8")
    assert 'manifest.get("source_sha") != source_sha' in text
    assert 'manifest.get("passed") is not True' in text
    assert "REQUIRED_STATES" in text
    assert 'manifest.get("corpus_authority")' in text
    assert 'manifest.get("fixture_root_is_test_only") is not True' in text
    assert '"text_evidence"' in text


def test_org_evidence_validator_pins_safe_state_filenames():
    text = VALIDATOR.read_text(encoding="utf-8")
    # Artifact manifests are data, not authority. A passing exact-head manifest
    # must not redirect validation to a sibling/parent file or alias one state's
    # evidence as another state's file.
    assert "EXPECTED_FILES" in text
    assert "Path(filename).name != filename" in text
    assert "filename != expected_filename" in text


def test_org_evidence_workflow_checks_out_and_names_artifact_by_exact_pr_head():
    text = WORKFLOW.read_text(encoding="utf-8")
    assert "SOURCE_SHA: ${{ github.event.pull_request.head.sha }}" in text
    assert "ref: ${{ github.event.pull_request.head.sha }}" in text
    assert 'test "$actual" = "$SOURCE_SHA"' in text
    assert ":org-app:testDebugUnitTest :org-app:assembleDebug" in text
    assert "org_device_acceptance_runner.py" in text
    assert "validate-org-ui-evidence.py" in text
    assert "org-android-ui-evidence-${{ github.event.pull_request.head.sha }}" in text


def test_org_emulator_runner_script_is_posix_sh_compatible_and_line_independent():
    text = WORKFLOW.read_text(encoding="utf-8")
    emulator_block = text.split(
        "uses: reactivecircus/android-emulator-runner@v2", 1
    )[1].split("- name: Validate exact-head Org evidence", 1)[0]
    assert "set -eu\n" in emulator_block
    assert "set -euo pipefail" not in emulator_block
    # android-emulator-runner invokes each script line as a separate `sh -c`;
    # shell control structures split across lines can never be valid here.
    assert "while [" not in emulator_block
    assert "done\n" not in emulator_block
    assert "ready=" not in emulator_block
    assert "attempt=" not in emulator_block


def test_org_emulator_runner_does_not_probe_scoped_storage_with_raw_shell_writes():
    text = WORKFLOW.read_text(encoding="utf-8")
    emulator_block = text.split(
        "uses: reactivecircus/android-emulator-runner@v2", 1
    )[1].split("- name: Validate exact-head Org evidence", 1)[0]
    # API-35 scoped external storage can reject raw shell writes to Documents.
    # The evidence gate must exercise the real acceptance/SAF path rather than
    # fail early on an unrelated shell-writability probe.
    assert "adb -s emulator-5554 wait-for-device" in emulator_block
    assert "adb -s emulator-5554 shell touch /sdcard/Documents/.zara-org-storage-ready" not in emulator_block
    assert "adb -s emulator-5554 shell rm -f /sdcard/Documents/.zara-org-storage-ready" not in emulator_block
    assert "adb -s emulator-5554 shell sh -c" not in emulator_block
    assert "python android/integration/org_device_acceptance_runner.py" in emulator_block


def test_org_evidence_upload_retains_preflight_diagnostics_even_if_device_capture_fails():
    text = WORKFLOW.read_text(encoding="utf-8")
    assert "Prepare exact-head evidence directory" in text
    assert "preflight.json" in text
    assert (
        'printf \'{"source_sha":"%s","phase":"device-preflight"}\\n\' '
        '"$SOURCE_SHA" > "$evidence_dir/preflight.json"'
        in text
    )
    assert "if-no-files-found: error" in text
