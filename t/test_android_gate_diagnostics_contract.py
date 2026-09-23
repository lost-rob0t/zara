import ast
from pathlib import Path


def test_android_gate_retains_stock_server_log_on_gradle_failure() -> None:
    source = Path("scripts/test-android.sh").read_text(encoding="utf-8")

    assert 'cp "$interop_log" "$diagnostics_dir/stock-zara-server.log"' in source
    assert 'tail -n 240 "$gradle_log" > "$diagnostics_dir/gradle-failure-tail.log"' in source


def test_stock_server_trace_is_bounded_and_literal_only() -> None:
    source = Path("android/integration/stock_zara_server_fixture.py").read_text(
        encoding="utf-8"
    )
    tree = ast.parse(source)
    trace_calls = [
        node
        for node in ast.walk(tree)
        if isinstance(node, ast.Call)
        and isinstance(node.func, ast.Name)
        and node.func.id == "_trace"
    ]

    observed: set[tuple[str, str]] = set()
    for call in trace_calls:
        assert len(call.args) == 2
        assert all(
            isinstance(argument, ast.Constant) and isinstance(argument.value, str)
            for argument in call.args
        )
        observed.add((call.args[0].value, call.args[1].value))

    assert {
        ("server", "ready"),
        ("transport.probe", "ready"),
        ("principal", "opened"),
        ("subscription", "opened"),
        ("turn.submit", "received"),
        ("turn.accepted", "callback-enter"),
        ("turn.accepted", "callback-returned"),
        ("turn.accepted", "barrier-armed"),
        ("turn.accepted", "client-observed"),
        ("runtime.events", "publishing"),
    } <= observed
    assert (
        'print(f"STOCK_INTEROP phase={phase} outcome={outcome}", '
        "file=sys.stderr, flush=True)"
    ) in source


def test_stock_fixture_is_published_only_after_authenticated_transport_probe() -> None:
    source = Path("android/integration/stock_zara_server_fixture.py").read_text(
        encoding="utf-8"
    )

    server_start = source.index("        server.start()")
    probe_call = source.index("            _wait_for_transport_ready(")
    fixture_write = source.index("            _write_fixture(", probe_call)
    ready_publish = source.index('            print("READY", flush=True)', fixture_write)

    assert server_start < probe_call < fixture_write < ready_publish
    assert "probe.start().result(timeout=5.0)" in source
    assert "CurveClientConfig(" in source
    assert "capabilities={Capability.SESSION_BASIC}" in source
