import pathlib
import sys
import types
from unittest.mock import MagicMock, patch

import pytest

import zara.__main__ as cli
import zara.wake


def _fake_config(*, model="small", device="cpu", threads=4):
    config = MagicMock()
    config.get_section.side_effect = lambda name: {
        "wake": {},
        "stt": {
            "model": model,
            "device": device,
            "threads": threads,
        },
        "tts": {"provider": "edge"},
        "memory": {},
        "dictate": {},
    }.get(name, {})
    config.get_llm_config.return_value = {"provider": "ollama"}
    config.get_latency_config.return_value = {"enabled": False}
    return config


def test_wake_constructs_whisper_with_cpu_threads_and_one_worker():
    config = _fake_config(threads=3)
    memory = MagicMock()
    memory.start_session.return_value = "test-session"
    main_pl = pathlib.Path(__file__).resolve().parent.parent / "main.pl"

    with (
        patch("zara.wake.get_config", return_value=config),
        patch("zara.wake.resolve_input_sample_rate", return_value=(16000.0, None)),
        patch("zara.wake.build_memory_manager", return_value=memory),
        patch("zara.wake.PrologEngine"),
        patch("zara.wake.faster_whisper.WhisperModel") as whisper_model,
        patch.object(zara.wake.WakeWordListener, "log") as log,
    ):
        zara.wake.WakeWordListener(
            model="small",
            device="cpu",
            prolog_main_path=main_pl,
            enable_tts=False,
        )

    whisper_model.assert_called_once_with(
        "small",
        device="cpu",
        compute_type="int8",
        cpu_threads=3,
        num_workers=1,
    )
    messages = [call.args[0] for call in log.call_args_list]
    assert any("Loading Whisper small" in message for message in messages)
    assert any("Whisper model ready: small on cpu" in message for message in messages)
    assert any("Silero VAD configured" in message for message in messages)


def test_wake_accepts_common_zara_transcription_variant():
    listener = zara.wake.WakeWordListener.__new__(zara.wake.WakeWordListener)

    assert listener._wake_command("Zara") == ""
    assert listener._wake_command("Hey Zara") == ""
    assert listener._wake_command("Sara") == ""
    assert listener._wake_command("Sara open Firefox") == "open Firefox"


def _fake_wake_modules(resolved_model):
    wake_main = MagicMock(return_value=0)
    fake_wake = types.ModuleType("zara.wake")
    fake_wake.main = wake_main

    resolve_model = MagicMock(return_value=resolved_model)
    fake_loader = types.ModuleType("zara.whisper_loader")
    fake_loader.resolve_whisper_model_files = resolve_model
    return wake_main, resolve_model, fake_wake, fake_loader


def test_wake_cli_defaults_model_and_device_from_stt_config():
    config = _fake_config(model="base.en", device="cpu")
    wake_main, resolve_model, fake_wake, fake_loader = _fake_wake_modules(
        "/cache/faster-whisper-base.en"
    )

    with (
        patch.object(cli, "init_config", return_value=config),
        patch.object(sys, "argv", ["zara", "--wake"]),
        patch.dict(
            sys.modules,
            {
                "zara.wake": fake_wake,
                "zara.whisper_loader": fake_loader,
            },
        ),
        pytest.raises(SystemExit) as exited,
    ):
        cli.main()

    assert exited.value.code == 0
    resolve_model.assert_called_once_with("base.en")
    wake_main.assert_called_once_with(
        model="/cache/faster-whisper-base.en",
        device="cpu",
        with_pets=False,
    )


def test_wake_cli_explicit_model_override_wins():
    config = _fake_config(model="small", device="cpu")
    wake_main, resolve_model, fake_wake, fake_loader = _fake_wake_modules(
        "/cache/faster-whisper-base.en"
    )

    with (
        patch.object(cli, "init_config", return_value=config),
        patch.object(
            sys,
            "argv",
            ["zara", "--wake", "--model", "base.en", "--device", "cpu", "--pets"],
        ),
        patch.dict(
            sys.modules,
            {
                "zara.wake": fake_wake,
                "zara.whisper_loader": fake_loader,
            },
        ),
        pytest.raises(SystemExit) as exited,
    ):
        cli.main()

    assert exited.value.code == 0
    resolve_model.assert_called_once_with("base.en")
    wake_main.assert_called_once_with(
        model="/cache/faster-whisper-base.en",
        device="cpu",
        with_pets=True,
    )
