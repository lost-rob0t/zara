import pathlib
import sys
import types
from concurrent.futures import ThreadPoolExecutor
from types import SimpleNamespace
from unittest.mock import MagicMock, patch

import numpy as np
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

    with (
        patch("zara.wake.get_config", return_value=config),
        patch("zara.wake.resolve_input_sample_rate", return_value=(16000.0, None)),
        patch("zara.wake.faster_whisper.WhisperModel") as whisper_model,
        patch.object(zara.wake.WakeWordListener, "log") as log,
        patch.object(zara.wake.WakeWordListener, "_init_ack_player"),
        patch("zara.wake.AcknowledgementPlayer"),
    ):
        zara.wake.WakeWordListener(
            model="small",
            device="cpu",
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
    assert any("Wake spotting configured" in message for message in messages)


def test_wake_accepts_common_zara_transcription_variant():
    listener = zara.wake.WakeWordListener.__new__(zara.wake.WakeWordListener)

    assert listener._wake_command("Zara") == ""
    assert listener._wake_command("Hey Zara") == ""
    assert listener._wake_command("Sara") == ""
    assert listener._wake_command("Sara open Firefox") == "open Firefox"


def test_stt_audio_conditioning_removes_dc_and_nonfinite_values():
    listener = zara.wake.WakeWordListener.__new__(zara.wake.WakeWordListener)
    audio = np.array([np.nan, np.inf, 0.75, 0.25, -0.25], dtype=np.float32)

    conditioned = listener._condition_stt_audio(audio)

    assert conditioned.dtype == np.float32
    assert np.all(np.isfinite(conditioned))
    assert np.max(np.abs(conditioned)) <= 1.0
    assert abs(float(np.mean(conditioned))) < 1e-5


@pytest.mark.asyncio
async def test_final_transcript_uses_quality_beam_without_second_vad():
    listener = zara.wake.WakeWordListener.__new__(zara.wake.WakeWordListener)
    listener.input_sample_rate = 16000.0
    listener.target_sample_rate = 16000.0
    listener.stt_beam_size = 5
    listener.wake_beam_size = 2
    listener.stt_language = "en"
    listener.executor = ThreadPoolExecutor(max_workers=1)
    listener.model = MagicMock()
    listener.model.transcribe.return_value = (
        [SimpleNamespace(text=" forget all of my memory")],
        None,
    )

    try:
        text = await listener.transcribe_async(
            np.array([[0.1], [0.2], [-0.1], [-0.2]], dtype=np.float32)
        )
    finally:
        listener.executor.shutdown(wait=True)

    assert text == "forget all of my memory"
    kwargs = listener.model.transcribe.call_args.kwargs
    assert kwargs["beam_size"] == 5
    assert kwargs["vad_filter"] is False
    assert kwargs["language"] == "en"
    assert kwargs["condition_on_previous_text"] is False
    assert kwargs["initial_prompt"] is None


@pytest.mark.asyncio
async def test_wake_transcript_keeps_fast_beam_and_zara_prompt():
    listener = zara.wake.WakeWordListener.__new__(zara.wake.WakeWordListener)
    listener.input_sample_rate = 16000.0
    listener.target_sample_rate = 16000.0
    listener.stt_beam_size = 5
    listener.wake_beam_size = 2
    listener.stt_language = "en"
    listener.executor = ThreadPoolExecutor(max_workers=1)
    listener.model = MagicMock()
    listener.model.transcribe.return_value = (
        [SimpleNamespace(text=" Zara")],
        None,
    )

    try:
        text = await listener.transcribe_async(
            np.array([[0.1], [0.2], [-0.1], [-0.2]], dtype=np.float32),
            wake_mode=True,
        )
    finally:
        listener.executor.shutdown(wait=True)

    assert text == "Zara"
    kwargs = listener.model.transcribe.call_args.kwargs
    assert kwargs["beam_size"] == 2
    assert kwargs["vad_filter"] is False
    assert kwargs["condition_on_previous_text"] is False
    assert "Zara" in kwargs["initial_prompt"]


def test_no_speech_logs_saturated_laptop_input():
    listener = zara.wake.WakeWordListener.__new__(zara.wake.WakeWordListener)
    listener._clock = lambda: 31.0
    listener._last_audio_warning = 0.0
    listener.log = MagicMock()

    listener._log_no_speech(1.0, 0.011)

    message = listener.log.call_args.args[0]
    assert "saturated" in message.lower()
    assert "laptop microphone" in message.lower()


@pytest.mark.parametrize("alias", ["rocm", "hip", "amd"])
def test_stt_gpu_aliases_use_ctranslate_cuda_device(alias):
    assert cli.normalize_stt_device(alias) == "cuda"


def test_stt_native_devices_are_preserved():
    assert cli.normalize_stt_device("cpu") == "cpu"
    assert cli.normalize_stt_device("cuda") == "cuda"


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


def test_wake_cli_rocm_alias_uses_ctranslate_cuda_device():
    config = _fake_config(model="base.en", device="cpu")
    wake_main, resolve_model, fake_wake, fake_loader = _fake_wake_modules(
        "/cache/faster-whisper-base.en"
    )

    with (
        patch.object(cli, "init_config", return_value=config),
        patch.object(sys, "argv", ["zara", "--wake", "--device", "rocm"]),
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
        device="cuda",
        with_pets=False,
    )


def test_wake_cli_gpu_initialization_failure_falls_back_to_cpu(capsys):
    config = _fake_config(model="base.en", device="cpu")
    wake_main, resolve_model, fake_wake, fake_loader = _fake_wake_modules(
        "/cache/faster-whisper-base.en"
    )
    wake_main.side_effect = [
        ValueError("HIP error: no binary for GPU gfx1012"),
        0,
    ]

    with (
        patch.object(cli, "init_config", return_value=config),
        patch.object(sys, "argv", ["zara", "--wake", "--device", "rocm"]),
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
    assert wake_main.call_count == 2
    assert wake_main.call_args_list[0].kwargs == {
        "model": "/cache/faster-whisper-base.en",
        "device": "cuda",
        "with_pets": False,
    }
    assert wake_main.call_args_list[1].kwargs == {
        "model": "/cache/faster-whisper-base.en",
        "device": "cpu",
        "with_pets": False,
    }
    assert "falling back to CPU" in capsys.readouterr().err
