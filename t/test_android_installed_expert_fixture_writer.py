from pathlib import Path


ROOT = Path(__file__).resolve().parents[1]
ACCEPTANCE = ROOT / "android" / "integration" / "device_pure_symbolic_acceptance.py"


def _install_acceptance_expert_source() -> str:
    source = ACCEPTANCE.read_text(encoding="utf-8")
    start = source.index("def install_acceptance_expert(")
    end = source.index("\n\ndef pull_app_file(", start)
    return source[start:end]


def test_installed_expert_fixture_stages_file_without_run_as_streaming_or_shell_parsing() -> None:
    """API-35 acceptance must not rely on run-as stdin forwarding for fixture bytes."""
    install = _install_acceptance_expert_source()

    assert '"sh", "-c"' not in install, (
        "Installed acceptance must not pass multiline expert source through run-as sh -c."
    )
    assert '"/system/bin/tee"' not in install, (
        "Exact-head CI 35693436239 proved adb exec-out run-as /system/bin/tee can hang waiting "
        "for EOF; stage a regular file outside the app sandbox instead."
    )
    assert "input=ACCEPTANCE_EXPERT_SOURCE" not in install, (
        "Do not depend on adb stdin forwarding for the app-private fixture boundary."
    )
    assert '"push"' in install, (
        "Stage the exact fixture bytes with adb push before entering the app sandbox."
    )
    assert '"run-as"' in install
    assert '"cp"' in install, (
        "Copy the staged regular file into the existing app-private Prolog workspace under run-as."
    )
    assert "ACCEPTANCE_EXPERT_SOURCE.encode(" in install, (
        "The host staging file must be populated from the exact UTF-8 fixture bytes."
    )


def test_installed_expert_fixture_verifies_fresh_private_workspace_postcondition() -> None:
    """Acceptance setup must prove the installed bytes before trusting later expert behavior."""
    install = _install_acceptance_expert_source()

    assert "ACCEPTANCE_EXPERT_SOURCE.encode(" in install
    assert '"exec-out"' in install
    assert '"/system/bin/cat"' in install
    assert "observed" in install and "expected" in install
    assert "raise AssertionError" in install, (
        "A successful copy is not enough; require exact read-back evidence from app-private storage."
    )
