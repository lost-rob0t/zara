import json
from pathlib import Path

import pytest

from zara import bench


def _write_cases(path: Path, rows):
    path.write_text(
        "\n".join(json.dumps(row) for row in rows) + "\n",
        encoding="utf-8",
    )


def test_load_cases_requires_heldout(tmp_path):
    corpus = tmp_path / "cases.jsonl"
    _write_cases(corpus, [{"id": "one", "prompt": "hello", "split": "train"}])
    with pytest.raises(ValueError, match="heldout"):
        bench.load_cases(corpus)


def test_score_case_combines_positive_and_forbidden_checks():
    case = bench.BenchmarkCase(
        case_id="policy",
        prompt="x",
        must_include=("direct",),
        must_not_include=("bypass",),
    )
    score, policy_failure = bench.score_case(
        case,
        "Use the direct provider path.",
    )
    assert score == 1.0
    assert policy_failure is False

    score, policy_failure = bench.score_case(case, "direct bypass")
    assert score == 0.5
    assert policy_failure is True


def test_openai_compatible_base_endpoint_is_normalized():
    assert (
        bench.normalize_openai_endpoint("https://llm.starintel.actor/v1")
        == "https://llm.starintel.actor/v1/chat/completions"
    )
    assert (
        bench.normalize_openai_endpoint(
            "https://proxy.example/v1/chat/completions"
        )
        == "https://proxy.example/v1/chat/completions"
    )


def test_generic_config_key_supports_starintel_gateway(monkeypatch):
    monkeypatch.delenv("OPENROUTER_API_KEY", raising=False)
    monkeypatch.delenv("ZARA_LLM_API_KEY", raising=False)
    assert (
        bench.resolve_api_key(
            "openrouter",
            {"api_key": "gateway-key"},
        )
        == "gateway-key"
    )


def test_parser_defaults_to_ten_workers():
    args = bench.build_parser().parse_args(["bench", "cases.jsonl"])
    assert args.workers == 10
    assert bench.DEFAULT_GLM_MODEL == "z-ai/glm-5.3-flash"


def test_validate_prompt_resource_does_not_require_prolog():
    bench.validate_resource("prompt", "Be precise.")


def test_zara_config_exposes_generic_llm_api_key(monkeypatch, tmp_path):
    from zara.config import ZaraConfig

    monkeypatch.delenv("ZARA_LLM_API_KEY", raising=False)
    monkeypatch.delenv("OPENROUTER_API_KEY", raising=False)
    config_path = tmp_path / "config.toml"
    config_path.write_text(
        '[llm]\nprovider = "openrouter"\napi_key = "gateway-key"\n',
        encoding="utf-8",
    )
    config = ZaraConfig(str(config_path))
    llm = config.get_llm_config()
    assert llm["api_key"] == "gateway-key"
    assert llm["openrouter_api_key"] == "gateway-key"


def test_generic_llm_env_key_overrides_provider_config(monkeypatch, tmp_path):
    from zara.config import ZaraConfig

    monkeypatch.setenv("ZARA_LLM_API_KEY", "env-key")
    monkeypatch.delenv("OPENROUTER_API_KEY", raising=False)
    config_path = tmp_path / "config.toml"
    config_path.write_text(
        '[llm]\nprovider = "openrouter"\n'
        'openrouter_api_key = "provider-config-key"\n',
        encoding="utf-8",
    )
    config = ZaraConfig(str(config_path))
    assert config.get_llm_config()["openrouter_api_key"] == "env-key"


def test_mutation_evidence_excludes_heldout_prompts(monkeypatch, tmp_path):
    cases = [
        bench.BenchmarkCase("train-one", "TRAIN_SECRET", split="train"),
        bench.BenchmarkCase("heldout-one", "HELDOUT_SECRET", split="heldout"),
    ]
    resource = tmp_path / "prompt.txt"
    resource.write_text("base prompt", encoding="utf-8")
    seen = {}

    def fake_rollouts(cases, **kwargs):
        prompt = kwargs["system_prompt"]
        if "candidate prompt" in prompt:
            train_score, heldout_score = 1.0, 1.0
        else:
            train_score, heldout_score = 0.0, 0.5
        return [
            {
                "case_id": "train-one",
                "split": "train",
                "prompt": "TRAIN_SECRET",
                "success": train_score == 1.0,
                "score": train_score,
                "latency_ms": 1.0,
                "text": "train response",
                "error_type": "",
                "attempts": 1,
                "policy_failure": False,
            },
            {
                "case_id": "heldout-one",
                "split": "heldout",
                "prompt": "HELDOUT_SECRET",
                "success": heldout_score == 1.0,
                "score": heldout_score,
                "latency_ms": 1.0,
                "text": "heldout response",
                "error_type": "",
                "attempts": 1,
                "policy_failure": False,
            },
        ]

    async def fake_mutation(**kwargs):
        seen["failures"] = kwargs["failures"]
        return "candidate prompt"

    monkeypatch.setattr(bench, "run_rollouts", fake_rollouts)
    monkeypatch.setattr(bench, "propose_mutation", fake_mutation)

    lineage = bench.evolve(
        cases=cases,
        resource_path=resource,
        resource_kind="prompt",
        generations=1,
        provider="openrouter",
        model=bench.DEFAULT_GLM_MODEL,
        endpoint="https://example.test/v1/chat/completions",
        api_key="test-key",
        workers=10,
        timeout=1.0,
        output_dir=tmp_path / "out",
    )

    assert [row["prompt"] for row in seen["failures"]] == ["TRAIN_SECRET"]
    assert all(row["prompt"] != "HELDOUT_SECRET" for row in seen["failures"])
    assert lineage[-1].accepted is True
    assert lineage[-1].parent_id == "gen-000"


def test_run_rollouts_starts_and_stops_exact_worker_count(monkeypatch):
    started = []
    stopped = []

    class FakeFuture:
        def __init__(self, message):
            self.message = message

        def get(self, timeout):
            return {"case_id": self.message["case"]["case_id"], "timeout": timeout}

    class FakeRef:
        def ask(self, message, block):
            assert block is False
            return FakeFuture(message)

        def stop(self, block):
            assert block is True
            stopped.append(self)

    def fake_start(**kwargs):
        ref = FakeRef()
        started.append((ref, kwargs))
        return ref

    monkeypatch.setattr(bench.RolloutWorker, "start", fake_start)
    rows = bench.run_rollouts(
        [bench.BenchmarkCase("one", "hello", split="heldout")],
        system_prompt="test",
        provider="openrouter",
        model=bench.DEFAULT_GLM_MODEL,
        endpoint="https://example.test/v1/chat/completions",
        api_key="test-key",
        workers=10,
        timeout=1.0,
    )

    assert len(rows) == 1
    assert len(started) == 10
    assert len(stopped) == 10
