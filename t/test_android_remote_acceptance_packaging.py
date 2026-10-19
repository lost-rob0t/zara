import ast
from pathlib import Path

import pytest


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


@pytest.mark.parametrize("response_exists", [True, False])
def test_remote_flow_reaches_retained_history_response_without_accepting_absence(response_exists):
    tree = ast.parse(REMOTE_ACCEPTANCE.read_text(encoding="utf-8"))
    exercise = next(node for node in tree.body
                    if isinstance(node, ast.FunctionDef) and node.name == "exercise_remote_connection")

    class RetainedHistoryDevice:
        def __init__(self):
            self.revealed = False
            self.calls = []

        def reveal_contains(self, fragment):
            self.calls.append(("reveal", fragment))
            if fragment == "stock server response":
                if not response_exists:
                    raise AssertionError("Missing remote response")
                self.revealed = True

        def await_contains(self, fragment, **kwargs):
            self.calls.append(("await", fragment))
            if fragment == "stock server response" and not self.revealed:
                raise AssertionError("Remote response is outside the retained history viewport")

        def __getattr__(self, name):
            return lambda *args, **kwargs: self.calls.append((name, *args))

    fixture = {"endpoint": "tcp://127.0.0.1:1234", "server_public": "fixture-public"}
    namespace = {
        "Device": RetainedHistoryDevice, "APP_PACKAGE": "ai.zara.app",
        "endpoint_port": lambda _: 1234,
        "require_reverse_mapping": lambda *_: "tcp:1234 tcp:1234",
        "open_menu": lambda *_: None, "type_printable_ascii": lambda *_: None,
        "exercise_chat_error": lambda *_: {"clipboard_exact_match": True},
        "find_curve_public_key": lambda *_: "client-public",
        "enroll_live_server": lambda *_: None,
        "visible_device_text": lambda *_: ["fixture-public"],
        "signal_turn_acceptance": lambda *_: None,
    }
    exec(compile(ast.Module(body=[exercise], type_ignores=[]), str(REMOTE_ACCEPTANCE), "exec"), namespace)
    device = RetainedHistoryDevice()
    if not response_exists:
        with pytest.raises(AssertionError, match="Missing remote response"):
            namespace["exercise_remote_connection"](device, fixture)
    else:
        result = namespace["exercise_remote_connection"](device, fixture)
        assert result["remote_turn_completed"] is True
        assert device.calls.index(("reveal", "stock server response")) < device.calls.index(
            ("await", "stock server response"))
