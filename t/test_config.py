import tomllib

import pytest

from zara.config import ConfigError, DEFAULT_CONFIG_TOML, ZaraConfig
from zara.tts.engine import TTSEngine


def test_default_config_is_valid_toml():
    parsed = tomllib.loads(DEFAULT_CONFIG_TOML)

    assert parsed["tts"]["provider"] == "qwen3"
    assert parsed["llm"]["endpoint"] == ""
    assert list(parsed).count("noaa") == 1
    assert parsed["tools"]["file_tools"] is False
    assert parsed["tools"]["memory_list"] is True
    assert parsed["tools"]["forget"] is True
    assert parsed["tool_approval"] == {
        "required_tools": [],
        "timeout_seconds": 300.0,
        "max_pending": 8,
    }
    assert parsed["file_tools"]["readable_roots"] == ["."]
    assert parsed["file_tools"]["writable_roots"] == ["."]
    assert parsed["wake"]["audio_queue_chunks"] == 32
    assert parsed["wake"]["first_speech_timeout"] == 5.0
    assert parsed["wake"]["max_utterance_duration"] == 30.0
    assert "silence_threshold" not in parsed["wake"]
    assert parsed["stt"]["trailing_silence_ms"] == 320
    assert parsed["stt"]["pre_speech_buffer_chunks"] == 10
    assert len(parsed["wake"]["acknowledgement"]["phrases"]) >= 15
    assert parsed["plugins"]["lifecycle_timeout"] == 5.0
    assert parsed["plugins"]["event_queue_size"] == 256
    assert parsed["plugins"]["max_managed_workers"] == 8


def test_plugin_config_is_isolated_by_plugin_name(tmp_path):
    config_path = tmp_path / "config.toml"
    config_path.write_text(
        '[tts]\nprovider = "qwen3"\n\n'
        '[plugins]\nevent_queue_size = 32\n\n'
        '[plugins.example-service]\nenabled = true\nport = 1234\n\n'
        '[plugins.other-service]\nsecret = "private"\n'
    )

    config = ZaraConfig(str(config_path))

    assert config.get_plugin_runtime_config() == {
        "lifecycle_timeout": 5.0,
        "event_queue_size": 32,
        "max_managed_workers": 8,
    }
    assert config.get_plugin_config("example-service") == {
        "enabled": True,
        "port": 1234,
    }
    assert "secret" not in config.get_plugin_config("example-service")


@pytest.mark.parametrize(
    "setting",
    [
        "lifecycle_timeout = 0",
        "event_queue_size = 0",
        "event_queue_size = 4097",
        "max_managed_workers = 0",
        "max_managed_workers = 65",
    ],
)
def test_plugin_runtime_bounds_are_validated(tmp_path, setting):
    config_path = tmp_path / "config.toml"
    config_path.write_text(
        f'[tts]\nprovider = "qwen3"\n\n[plugins]\n{setting}\n'
    )

    with pytest.raises(ConfigError, match="plugins"):
        ZaraConfig(str(config_path))


def test_first_run_creates_parseable_config(monkeypatch, tmp_path):
    monkeypatch.setenv("XDG_CONFIG_HOME", str(tmp_path))

    config = ZaraConfig()

    with config.config_file.open("rb") as config_file:
        parsed = tomllib.load(config_file)
    assert parsed["tts"]["provider"] == "qwen3"
    assert config.get("wake", "threshold") == 0.5


def test_reload_reads_user_override(tmp_path):
    config_path = tmp_path / "config.toml"
    config_path.write_text('[tts]\nprovider = "qwen3"\n\n[wake]\nthreshold = 0.4\n')
    config = ZaraConfig(str(config_path))

    config_path.write_text('[tts]\nprovider = "qwen3"\n\n[wake]\nthreshold = 0.8\n')
    config.reload()

    assert config.get("wake", "threshold") == 0.8


def test_invalid_toml_reports_path_and_does_not_fall_back(tmp_path):
    config_path = tmp_path / "broken.toml"
    config_path.write_text("[tts\nprovider = 'qwen3'\n")

    with pytest.raises(ConfigError, match=r"broken\.toml") as error:
        ZaraConfig(str(config_path))

    assert "Failed to load config" in str(error.value)


@pytest.mark.parametrize("provider", ["qwen", "qwen3"])
def test_qwen_provider_names_initialize_as_qwen3(tmp_path, provider):
    config_path = tmp_path / "config.toml"
    config_path.write_text(f'[tts]\nprovider = "{provider}"\n')
    config = ZaraConfig(str(config_path))

    engine = TTSEngine(provider=config.get("tts", "provider"), config=config._config)

    assert config.get("tts", "provider") == "qwen3"
    assert engine.provider == "qwen3"
    assert engine.qwen3_url == "http://localhost:7860"


def test_provider_specific_fields_are_validated(tmp_path):
    config_path = tmp_path / "config.toml"
    config_path.write_text('[tts]\nprovider = "11labs"\n')

    with pytest.raises(ConfigError, match="tts.elevenlabs_api_key"):
        ZaraConfig(str(config_path))


@pytest.mark.parametrize(
    "setting",
    [
        'provider = "unknown"',
        "connect_timeout = 0",
        "read_timeout = false",
        "total_timeout = -1",
        "max_retries = -1",
        "history_limit = 0",
    ],
)
def test_llm_bounds_are_validated(tmp_path, setting):
    config_path = tmp_path / "config.toml"
    config_path.write_text(f'[tts]\nprovider = "qwen3"\n\n[llm]\n{setting}\n')

    with pytest.raises(ConfigError, match="LLM|llm"):
        ZaraConfig(str(config_path))


@pytest.mark.parametrize("provider", ["anthropic", "openai", "openrouter"])
def test_remote_provider_does_not_inherit_ollama_default_endpoint(
    monkeypatch, tmp_path, provider
):
    monkeypatch.delenv("ZARA_LLM_PROVIDER", raising=False)
    monkeypatch.delenv("ZARA_LLM_ENDPOINT", raising=False)
    config_path = tmp_path / "config.toml"
    config_path.write_text(
        '[tts]\nprovider = "qwen3"\n\n'
        f'[llm]\nprovider = "{provider}"\n'
        'endpoint = "http://localhost:11434/api/chat"\n'
    )

    config = ZaraConfig(str(config_path))

    assert config.get_llm_config()["endpoint"] is None


@pytest.mark.parametrize("provider", ["anthropic", "openai", "openrouter"])
def test_remote_provider_preserves_explicit_custom_endpoint(monkeypatch, tmp_path, provider):
    monkeypatch.delenv("ZARA_LLM_PROVIDER", raising=False)
    monkeypatch.delenv("ZARA_LLM_ENDPOINT", raising=False)
    config_path = tmp_path / "config.toml"
    config_path.write_text(
        '[tts]\nprovider = "qwen3"\n\n'
        f'[llm]\nprovider = "{provider}"\nendpoint = "http://proxy.test/v1/chat"\n'
    )

    config = ZaraConfig(str(config_path))

    assert config.get_llm_config()["endpoint"] == "http://proxy.test/v1/chat"


def test_openrouter_api_key_resolves_config_then_environment(monkeypatch, tmp_path):
    monkeypatch.delenv("OPENROUTER_API_KEY", raising=False)
    config_path = tmp_path / "config.toml"
    config_path.write_text(
        '[tts]\nprovider = "qwen3"\n\n'
        '[llm]\nprovider = "openrouter"\nopenrouter_api_key = "config-key"\n'
    )
    config = ZaraConfig(str(config_path))

    assert config.get_llm_config()["openrouter_api_key"] == "config-key"

    monkeypatch.setenv("OPENROUTER_API_KEY", "env-key")

    assert config.get_llm_config()["openrouter_api_key"] == "env-key"


def test_openrouter_api_key_defaults_to_none(monkeypatch, tmp_path):
    monkeypatch.delenv("OPENROUTER_API_KEY", raising=False)
    config_path = tmp_path / "config.toml"
    config_path.write_text('[tts]\nprovider = "qwen3"\n\n[llm]\nprovider = "openrouter"\n')
    config = ZaraConfig(str(config_path))

    assert config.get_llm_config()["openrouter_api_key"] is None


@pytest.mark.parametrize(
    "setting",
    [
        'file_tools = "yes"',
        "file_tools = 1",
    ],
)
def test_file_tool_toggle_must_be_boolean(tmp_path, setting):
    config_path = tmp_path / "config.toml"
    config_path.write_text(f'[tts]\nprovider = "qwen3"\n\n[tools]\n{setting}\n')

    with pytest.raises(ConfigError, match="tools.file_tools"):
        ZaraConfig(str(config_path))


@pytest.mark.parametrize(
    "setting",
    [
        'readable_roots = "foo"',
        "writable_roots = []",
        "max_bytes = 0",
    ],
)
def test_file_tool_policy_config_is_validated(tmp_path, setting):
    config_path = tmp_path / "config.toml"
    config_path.write_text(
        f'[tts]\nprovider = "qwen3"\n\n[file_tools]\n{setting}\n'
    )

    with pytest.raises(ConfigError, match="file_tools"):
        ZaraConfig(str(config_path))


@pytest.mark.parametrize(
    "setting",
    [
        'required_tools = "calculator"',
        'required_tools = [""]',
        'required_tools = ["bad name"]',
        'required_tools = ["calculator", "calculator"]',
        "timeout_seconds = 0",
        "timeout_seconds = false",
        "max_pending = 0",
        "max_pending = 65",
    ],
)
def test_tool_approval_policy_is_bounded_and_validated(tmp_path, setting):
    config_path = tmp_path / "config.toml"
    config_path.write_text(
        f'[tts]\nprovider = "qwen3"\n\n[tool_approval]\n{setting}\n'
    )

    with pytest.raises(ConfigError, match="tool_approval"):
        ZaraConfig(str(config_path))
