from __future__ import annotations

from pathlib import Path
import sys

from verification.zara_verify_runner import execute_gate, parse_junit


def test_executor_rejects_retained_log_tampering(tmp_path):
    command = (
        'import os,time\n'
        'from pathlib import Path\n'
        'print("observed", flush=True)\n'
        'time.sleep(0.1)\n'
        'Path(os.environ["ARTIFACT_DIR"], "output.log").write_text("tampered\\n")\n'
    )
    result = execute_gate(
        'fixture', [sys.executable, '-c', command], tmp_path, tmp_path / 'evidence', 3, 1024
    )
    assert result['state'] == 'error'
    assert result['reason'] == 'artifact_tampered'
    assert Path(result['artifact']).read_text() == 'tampered\n'


def test_junit_rejects_symlinked_evidence(tmp_path):
    target = tmp_path / 'outside.xml'
    target.write_text('<testsuite><testcase name="forged"/></testsuite>')
    junit = tmp_path / 'junit.xml'
    junit.symlink_to(target)
    assert parse_junit(junit)['state'] == 'missing'
