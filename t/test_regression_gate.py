"""Self-tests for the ZARA-021 regression gate.

These tests prove the gate's structural properties without running the
full suite (which would be recursive inside pytest):
- The gate script exists and uses fail-fast semantics.
- Cleanup occurs via a trap.
- Phase order is deterministic.
- Audio fixtures are generated and byte-identical across runs.
"""

from __future__ import annotations

import ast
import pathlib
import struct
import subprocess
import sys
import tempfile
import wave

import numpy as np
import pytest

ROOT = pathlib.Path(__file__).resolve().parent.parent
GATE_SCRIPT = ROOT / "scripts" / "test-all.sh"
FIXTURE_GENERATOR = ROOT / "scripts" / "generate-audio-fixtures.py"
FIXTURE_DIR = ROOT / "t" / "fixtures" / "audio"


def test_gate_script_exists_and_is_executable():
    assert GATE_SCRIPT.exists(), "scripts/test-all.sh must exist"
    assert GATE_SCRIPT.stat().st_mode & 0o111, "scripts/test-all.sh must be executable"


def test_gate_uses_fail_fast():
    source = GATE_SCRIPT.read_text()
    assert "set -euo pipefail" in source, "gate must use set -euo pipefail for fail-fast"


def test_gate_has_cleanup_trap():
    source = GATE_SCRIPT.read_text()
    assert "trap" in source and "rm -rf" in source, (
        "gate must have a cleanup trap that removes the temp dir"
    )


def test_gate_sets_isolated_environment():
    source = GATE_SCRIPT.read_text()
    required_vars = [
        "HOME=",
        "XDG_CONFIG_HOME=",
        "XDG_RUNTIME_DIR=",
        "ZARA_DICTATION_PIDFILE=",
        "ZARA_DICTATION_LOGFILE=",
    ]
    for var in required_vars:
        assert var in source, f"gate must set {var}"


def test_gate_phase_order_is_deterministic():
    """The script must run phases in the order defined by ZARA-021."""
    source = GATE_SCRIPT.read_text()
    phases = [
        "Python compile/import checks",
        "Prolog module load",
        "Prolog resolver corpus",
        "Pytest suite",
        "Config/process/file-tool security scripts",
        "Deterministic latency budgets",
        "Packaging/Nix checks",
    ]
    positions = []
    for phase in phases:
        pos = source.find(f'"{phase}"')
        assert pos != -1, f"phase '{phase}' not found in gate script"
        positions.append(pos)
    assert positions == sorted(positions), (
        "phases must appear in ZARA-021 order; got positions: "
        + ", ".join(str(p) for p in positions)
    )


def test_gate_produces_junit_xml():
    source = GATE_SCRIPT.read_text()
    assert "junit-xml" in source, "gate must produce JUnit XML output"
    assert "junit.xml" in source, "gate must write junit.xml to artifact dir"


def test_gate_fails_on_failing_subscript():
    """A failing sub-script must cause the gate to exit non-zero.

    This test creates a minimal gate-like script that sources the same
    fail-fast pattern (set -euo pipefail + trap) and verifies that a
    failing command causes non-zero exit.
    """
    with tempfile.TemporaryDirectory() as tmpdir:
        fake_gate = pathlib.Path(tmpdir) / "fake-gate.sh"
        fake_gate.write_text(
            "#!/usr/bin/env bash\n"
            "set -euo pipefail\n"
            "TEST_ROOT=$(mktemp -d)\n"
            "trap 'rm -rf $TEST_ROOT' EXIT\n"
            "false\n"  # always fails
            "echo 'should not reach here'\n"
        )
        fake_gate.chmod(0o755)
        result = subprocess.run(
            ["bash", str(fake_gate)], capture_output=True, text=True
        )
        assert result.returncode != 0, "gate must fail on failing command"
        assert "should not reach here" not in result.stdout


def test_gate_cleanup_occurs_after_failure():
    """The temp directory must be removed even when the gate fails."""
    with tempfile.TemporaryDirectory() as tmpdir:
        fake_gate = pathlib.Path(tmpdir) / "fake-gate.sh"
        marker = pathlib.Path(tmpdir) / "marker"
        fake_gate.write_text(
            "#!/usr/bin/env bash\n"
            "set -euo pipefail\n"
            f"TEST_ROOT=$(mktemp -d)\n"
            f"echo $TEST_ROOT > {marker}\n"
            "trap 'rm -rf $TEST_ROOT' EXIT\n"
            "false\n"
        )
        fake_gate.chmod(0o755)
        subprocess.run(["bash", str(fake_gate)], capture_output=True)
        temp_dir_path = pathlib.Path(marker.read_text().strip())
        assert not temp_dir_path.exists(), (
            "temp dir must be cleaned up after gate failure"
        )


def test_audio_fixture_generator_exists_and_is_executable():
    assert FIXTURE_GENERATOR.exists(), "audio fixture generator must exist"
    assert FIXTURE_GENERATOR.stat().st_mode & 0o111, (
        "audio fixture generator must be executable"
    )


def test_audio_fixtures_are_deterministic():
    """Generating fixtures twice must produce byte-identical files."""
    with tempfile.TemporaryDirectory() as tmpdir:
        dir_a = pathlib.Path(tmpdir) / "a"
        dir_b = pathlib.Path(tmpdir) / "b"
        subprocess.run(
            [sys.executable, str(FIXTURE_GENERATOR), str(dir_a)],
            capture_output=True, check=True,
        )
        subprocess.run(
            [sys.executable, str(FIXTURE_GENERATOR), str(dir_b)],
            capture_output=True, check=True,
        )
        for fixture_name in [
            "silence.wav", "wake_word.wav", "command_speech.wav",
            "trailing_silence.wav", "interruption.wav", "noise.wav",
            "long_speech.wav",
        ]:
            data_a = (dir_a / fixture_name).read_bytes()
            data_b = (dir_b / fixture_name).read_bytes()
            assert data_a == data_b, (
                f"{fixture_name} must be byte-identical across runs"
            )


def test_audio_fixtures_have_correct_format():
    """Every fixture must be a 16 kHz, 16-bit, mono WAV file."""
    if not FIXTURE_DIR.exists():
        pytest.skip("audio fixtures not generated yet")

    expected_files = [
        "silence.wav", "wake_word.wav", "command_speech.wav",
        "trailing_silence.wav", "interruption.wav", "noise.wav",
        "long_speech.wav",
    ]
    for name in expected_files:
        path = FIXTURE_DIR / name
        assert path.exists(), f"fixture {name} must exist"
        with wave.open(str(path), "rb") as wf:
            assert wf.getframerate() == 16000, f"{name}: sample rate must be 16000"
            assert wf.getsampwidth() == 2, f"{name}: sample width must be 16-bit"
            assert wf.getnchannels() == 1, f"{name}: must be mono"


def test_silence_fixture_is_all_zeros():
    """The silence fixture must contain only zero samples."""
    if not FIXTURE_DIR.exists():
        pytest.skip("audio fixtures not generated yet")
    with wave.open(str(FIXTURE_DIR / "silence.wav"), "rb") as wf:
        frames = wf.readframes(wf.getnframes())
        samples = np.frombuffer(frames, dtype=np.int16)
        assert np.all(samples == 0), "silence fixture must be all zeros"


def test_fixtures_are_small():
    """Each fixture must be under 500 KB for fast test execution."""
    if not FIXTURE_DIR.exists():
        pytest.skip("audio fixtures not generated yet")
    for path in FIXTURE_DIR.glob("*.wav"):
        size = path.stat().st_size
        assert size < 500_000, f"{path.name} is {size} bytes; must be under 500 KB"
