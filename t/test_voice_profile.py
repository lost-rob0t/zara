"""Voice profile switching: normal (local llamacpp) <-> game (edge)."""

import tomllib

import pytest

from zara.config import ZaraConfig
from zara.voice_profile import (
    VOICE_MODE_PROVIDERS,
    current_voice_mode,
    register_voice_reset,
    set_voice_mode,
)


@pytest.fixture
def config(tmp_path):
    config_path = tmp_path / "config.toml"
    config_path.write_text('[tts]\nprovider = "llamacpp"\n')
    return ZaraConfig(str(config_path))


@pytest.fixture(autouse=True)
def clear_resets():
    import zara.voice_profile as voice_profile

    voice_profile._RESET_CALLBACKS.clear()
    yield
    voice_profile._RESET_CALLBACKS.clear()


def test_voice_mode_provider_map():
    assert VOICE_MODE_PROVIDERS == {"normal": "llamacpp", "game": "edge"}


def test_current_voice_mode_derives_from_provider(config):
    assert current_voice_mode(config) == "normal"

    config.set_value("tts", "provider", "edge")
    assert current_voice_mode(config) == "game"


def test_current_voice_mode_unknown_provider_reads_as_normal(tmp_path):
    config_path = tmp_path / "config.toml"
    config_path.write_text('[tts]\nprovider = "local"\n')
    config = ZaraConfig(str(config_path))

    assert current_voice_mode(config) == "normal"


def test_set_voice_mode_game_flips_provider_persists_and_notifies(config):
    fired = []
    register_voice_reset(lambda: fired.append("bridge"))

    response = set_voice_mode("game", config=config)

    assert config.get("tts", "provider") == "edge"
    persisted = tomllib.loads(config.config_file.read_text())
    assert persisted["tts"]["provider"] == "edge"
    assert fired == ["bridge"]
    assert "Game mode" in response


def test_set_voice_mode_normal_restores_llamacpp(config):
    config.set_value("tts", "provider", "edge")

    response = set_voice_mode("normal", config=config)

    assert config.get("tts", "provider") == "llamacpp"
    assert "Normal mode" in response


def test_set_voice_mode_same_mode_still_persists_and_notifies(config):
    fired = []
    register_voice_reset(lambda: fired.append("reset"))

    set_voice_mode("normal", config=config)

    assert fired == ["reset"]


def test_set_voice_mode_rejects_unknown_mode(config):
    with pytest.raises(ValueError, match="unknown voice mode"):
        set_voice_mode("turbo", config=config)


def test_set_voice_mode_swallows_callback_failures(config):
    def broken():
        raise RuntimeError("bridge is gone")

    fired = []
    register_voice_reset(broken)
    register_voice_reset(lambda: fired.append("announcer"))

    response = set_voice_mode("game", config=config)

    assert fired == ["announcer"]
    assert config.get("tts", "provider") == "edge"
    assert "Game mode" in response


def test_python_skills_expose_voice_mode_handlers(monkeypatch, tmp_path):
    monkeypatch.setenv("XDG_CONFIG_HOME", str(tmp_path))
    from zara import config as config_module

    monkeypatch.setattr(config_module, "_global_config", None)
    from zara.python_skills import python_skills

    assert "set_game_mode" in python_skills.list_skills()
    assert "set_normal_mode" in python_skills.list_skills()

    response = python_skills.execute("set_game_mode", [])

    assert "Game mode" in response
    persisted = tomllib.loads(
        (tmp_path / "zarathushtra" / "config.toml").read_text()
    )
    assert persisted["tts"]["provider"] == "edge"

    python_skills.execute("set_normal_mode", [])
    persisted = tomllib.loads(
        (tmp_path / "zarathushtra" / "config.toml").read_text()
    )
    assert persisted["tts"]["provider"] == "llamacpp"
