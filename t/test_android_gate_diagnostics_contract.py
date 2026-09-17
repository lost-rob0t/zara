from pathlib import Path


def test_android_gate_retains_stock_server_log_on_gradle_failure() -> None:
    source = Path("scripts/test-android.sh").read_text(encoding="utf-8")

    assert 'cp "$interop_log" "$diagnostics_dir/stock-zara-server.log"' in source
    assert 'tail -n 240 "$gradle_log" > "$diagnostics_dir/gradle-failure-tail.log"' in source
