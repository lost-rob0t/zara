"""zara voice-mode CLI: quick normal<->game voice profile switch."""

import tomllib

import pytest

from zara.config import ZaraConfig
from zara.voice_cli import main


@pytest.fixture
def config(tmp_path):
    config_path = tmp_path / "config.toml"
    config_path.write_text('[tts]\nprovider = "llamacpp"\n')
    return ZaraConfig(str(config_path))


def test_voice_mode_game_flips_provider(config, capsys):
    exit_code = main(["game"], config=config)

    assert exit_code == 0
    persisted = tomllib.loads(config.config_file.read_text())
    assert persisted["tts"]["provider"] == "edge"
    assert "Game mode" in capsys.readouterr().out


def test_voice_mode_normal_restores_llamacpp(config, capsys):
    main(["game"], config=config)

    exit_code = main(["normal"], config=config)

    assert exit_code == 0
    persisted = tomllib.loads(config.config_file.read_text())
    assert persisted["tts"]["provider"] == "llamacpp"
    assert "Normal mode" in capsys.readouterr().out


def test_voice_mode_without_argument_prints_current(config, capsys):
    exit_code = main([], config=config)

    assert exit_code == 0
    output = capsys.readouterr().out
    assert "normal" in output
    assert "llamacpp" in output


def test_voice_mode_status_reports_game(config, capsys):
    main(["game"], config=config)

    exit_code = main(["status"], config=config)

    assert exit_code == 0
    output = capsys.readouterr().out
    assert "game" in output
    assert "edge" in output


def test_voice_mode_rejects_unknown_mode(config):
    with pytest.raises(SystemExit):
        main(["turbo"], config=config)
