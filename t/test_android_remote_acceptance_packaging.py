from pathlib import Path


REMOTE_ACCEPTANCE = Path("android/integration/device_remote_acceptance.py")
STOCK_FIXTURE = Path("android/integration/stock_zara_server_fixture.py")
EMULATOR_GATE = Path("scripts/test-android-emulator-install.sh")


def test_emulator_gate_packages_remote_acceptance_helper() -> None:
    gate = EMULATOR_GATE.read_text(encoding="utf-8")

    assert 'python3 "$repo_root/android/integration/device_remote_acceptance.py"' in gate
    assert 'PYTHONPATH="$repo_root/android/integration:$repo_root' in gate
    assert '--fixture-file "$interop_fixture"' in gate
    assert '--output "$repo_root/$evidence_dir"' in gate
    assert REMOTE_ACCEPTANCE.is_file(), (
        "emulator gate invokes device_remote_acceptance.py but the exact source tree "
        "does not package that helper"
    )

    remote = REMOTE_ACCEPTANCE.read_text(encoding="utf-8")
    assert "SecurityAdminClient" in remote
    assert 'device.tap("Remote")' in remote
    assert "signal_turn_acceptance(fixture)" in remote
    assert 'device.await_contains("stock server response"' in remote
    assert '"remote_turn_completed": True' in remote


def test_stock_fixture_traces_every_canonical_runtime_event() -> None:
    fixture = STOCK_FIXTURE.read_text(encoding="utf-8")

    expected = (
        "TurnStarted",
        "AssistantStarted",
        "AssistantComplete",
        "AgentCompleted",
    )
    for event_type in expected:
        assert f"events.{event_type}(" in fixture
        assert f'_trace("runtime.event", "published:{event_type}")' in fixture

    assert fixture.count('self.bus.publish(\n                events.') == len(expected)
    assert fixture.count('_trace("runtime.event", "published:') == len(expected)


def test_fixture_readiness_deadline_excludes_nix_environment_build() -> None:
    for path in (Path("scripts/test-android.sh"), EMULATOR_GATE):
        gate = path.read_text()
        prepare = gate.index('nix develop "$repo_root" -c true')
        launch = gate.index('nix develop "$repo_root" -c env')
        deadline = gate.index('for _ in $(seq 1 1200)')
        assert prepare < launch < deadline
        assert 'sleep 0.05' in gate[deadline:]
