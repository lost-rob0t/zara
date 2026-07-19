import tomllib

import pytest

from zara.config import ConfigError, DEFAULT_CONFIG_TOML, ZaraConfig
from zara.tts.engine import TTSEngine


def test_default_config_is_valid_toml():
    parsed = tomllib.loads(DEFAULT_CONFIG_TOML)

    assert parsed["tts"]["provider"] == "qwen3"
    assert list(parsed).count("noaa") == 1
    assert parsed["tools"]["file_tools"] is False
    assert parsed["file_tools"]["readable_roots"] == ["."]
    assert parsed["file_tools"]["writable_roots"] == ["."]
    assert parsed["wake"]["audio_queue_chunks"] == 32
    assert parsed["wake"]["first_speech_timeout"] == 5.0
    assert parsed["wake"]["max_utterance_duration"] == 30.0


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
