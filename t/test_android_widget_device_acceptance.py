from pathlib import Path


WIDGET_ACCEPTANCE = Path("android/integration/widget_device_acceptance.py")
EMULATOR_GATE = Path("scripts/test-android-emulator-install.sh")


def test_emulator_gate_runs_real_launcher_widget_acceptance() -> None:
    gate = EMULATOR_GATE.read_text(encoding="utf-8")

    assert "android/integration/widget_device_acceptance.py" in gate
    assert '--serial "$serial"' in gate
    assert '--source-sha "$source_sha"' in gate
    assert '--output "$evidence_dir/widgets"' in gate


def test_widget_acceptance_captures_exact_sha_same_state_evidence() -> None:
    source = WIDGET_ACCEPTANCE.read_text(encoding="utf-8")

    assert "git\", \"rev-parse\", \"HEAD" in source
    assert "WidgetEvidenceActivity" in source
    assert "requestPinAppWidget" not in source
    assert "uiautomator" in source
    assert "screencap" in source
    assert ".ui.json" in source
    assert ".actions.json" in source
    assert "sha256" in source
    assert "source_sha" in source
    assert "visual_verdict" in source


def test_widget_acceptance_exercises_responsive_theme_runtime_and_process_death_paths() -> None:
    source = WIDGET_ACCEPTANCE.read_text(encoding="utf-8")

    for required in (
        "assistant-outrun",
        "assistant-light",
        "assistant-narrow-200pct",
        "runtime-fresh",
        "runtime-stale",
        "runtime-corrupt",
        "route-cold",
        "route-warm",
        "route-process-death",
        "font_scale",
        "2.0",
        "target_width_dp=320",
        "am\", \"kill\", \"ai.zara.app",
    ):
        assert required in source

    assert "LOCAL READY" in source
    assert "STALE" in source
    assert "LOCAL UNKNOWN" in source
    assert "Chat" in source
    assert "Runtime" in source


def test_widget_acceptance_keeps_talkback_hardware_claim_unproven() -> None:
    source = WIDGET_ACCEPTANCE.read_text(encoding="utf-8")

    assert '"talkback_spoken_traversal": False' in source
    assert '"visual_verdict": "requires-vision-review"' in source
