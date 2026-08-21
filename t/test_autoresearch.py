from pathlib import Path
import sys

import pytest

from zara.autoresearch import (
    ResearchContract,
    ResearchLedger,
    metric_improved,
    parse_metric,
    run_evaluator,
)


def test_parse_metric_uses_last_match():
    output = "score: 4.0\nscore: 3.25\n"
    assert parse_metric(output, r"score: ([0-9.]+)") == 3.25


def test_parse_metric_requires_numeric_capture():
    with pytest.raises(ValueError, match="not numeric"):
        parse_metric("score: nope", r"score: (\w+)")


def test_metric_improved_respects_direction():
    assert metric_improved(0.9, 1.0, "minimize")
    assert not metric_improved(1.1, 1.0, "minimize")
    assert metric_improved(1.1, 1.0, "maximize")
    assert not metric_improved(0.9, 1.0, "maximize")


def test_contract_rejects_escape_from_repo(tmp_path: Path):
    repo = tmp_path / "repo"
    repo.mkdir()
    (repo / "train.py").write_text("print('ok')\n", encoding="utf-8")
    outside = tmp_path / "outside.py"
    outside.write_text("print('no')\n", encoding="utf-8")

    contract = ResearchContract(
        goal="improve score",
        command=(sys.executable, "train.py"),
        metric_pattern=r"score: ([0-9.]+)",
        direction="maximize",
        files=("../outside.py",),
    )

    with pytest.raises(ValueError, match="escapes repo root"):
        contract.validate(repo)


def test_contract_requires_metric_capture_group(tmp_path: Path):
    repo = tmp_path / "repo"
    repo.mkdir()
    (repo / "train.py").write_text("print('ok')\n", encoding="utf-8")
    contract = ResearchContract(
        goal="improve score",
        command=(sys.executable, "train.py"),
        metric_pattern=r"score: [0-9.]+",
        direction="maximize",
        files=("train.py",),
    )

    with pytest.raises(ValueError, match="capture group"):
        contract.validate(repo)


def test_run_evaluator_parses_metric_without_shell(tmp_path: Path):
    script = tmp_path / "experiment.py"
    script.write_text("print('metric: 7.5')\n", encoding="utf-8")
    contract = ResearchContract(
        goal="maximize metric",
        command=(sys.executable, "experiment.py"),
        metric_pattern=r"metric: ([0-9.]+)",
        direction="maximize",
        files=("experiment.py",),
        timeout_seconds=5,
    )
    contract.validate(tmp_path)

    evaluation = run_evaluator(tmp_path, contract)

    assert evaluation.metric == 7.5
    assert evaluation.returncode == 0
    assert evaluation.elapsed_seconds >= 0


def test_ledger_history_exposes_only_numeric_decisions(tmp_path: Path):
    ledger = ResearchLedger(tmp_path / "results.jsonl")
    ledger.append(
        {
            "iteration": 1,
            "metric": 1.25,
            "accepted": True,
            "status": "accepted",
            "hypothesis": "untrusted prose should not be replayed",
        }
    )

    assert ledger.numeric_history() == [
        {
            "iteration": 1,
            "metric": 1.25,
            "accepted": True,
            "status": "accepted",
        }
    ]
