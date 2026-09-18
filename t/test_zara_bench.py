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
