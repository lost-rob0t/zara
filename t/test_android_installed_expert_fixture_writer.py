from pathlib import Path


ROOT = Path(__file__).resolve().parents[1]
ACCEPTANCE = ROOT / "android" / "integration" / "device_pure_symbolic_acceptance.py"


def _install_acceptance_expert_source() -> str:
    source = ACCEPTANCE.read_text(encoding="utf-8")
    start = source.index("def install_acceptance_expert(")
    end = source.index("\n\ndef pull_app_file(", start)
    return source[start:end]


def test_installed_expert_fixture_streams_bytes_without_run_as_shell_redirection() -> None:
    """The API-35 run-as shell must not parse a multiline Prolog fixture as argv text."""
    install = _install_acceptance_expert_source()

    assert '"sh", "-c"' not in install, (
        "Installed acceptance must not pass multiline expert source through run-as sh -c; "
        "exact-head CI 35689330099 proved that path exits 1 before the expert transcript starts."
    )
    assert "subprocess.run(" in install, (
        "Write the fixture over adb stdin so Prolog source bytes are data, not shell syntax."
    )
    assert '"run-as"' in install
    assert '"/system/bin/tee"' in install, (
        "Use the platform-owned app-private writer instead of depending on shell redirection."
    )
    assert "input=ACCEPTANCE_EXPERT_SOURCE" in install
    assert "check=True" in install


def test_installed_expert_fixture_verifies_fresh_private_workspace_postcondition() -> None:
    """Acceptance setup must prove the installed bytes before trusting later expert behavior."""
    install = _install_acceptance_expert_source()

    assert "ACCEPTANCE_EXPERT_SOURCE.encode(" in install
    assert '"exec-out"' in install
    assert '"/system/bin/cat"' in install
    assert "observed" in install and "expected" in install
    assert "raise AssertionError" in install, (
        "A successful writer exit is not enough; require exact read-back evidence from app-private storage."
    )
