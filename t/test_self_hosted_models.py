from __future__ import annotations

from pathlib import Path

import pytest

from zara.llm import LLMClient
from zara.self_hosted_models import (
    HardwareDevice,
    HardwareProfile,
    LlamaCppSettings,
    ManagedLlamaCppRuntime,
    ModelCandidate,
    assess_model_fit,
    build_llama_cpp_command,
    discover_gguf_models,
    parse_nvidia_smi,
)


def test_gguf_catalog_is_bounded_sorted_and_never_guesses_quantization(tmp_path):
    root = tmp_path / "models"
    root.mkdir()
    first = root / "alpha.gguf"
    second = root / "nested" / "zeta.Q4_K_M.gguf"
    second.parent.mkdir()
    first.write_bytes(b"a" * 17)
    second.write_bytes(b"b" * 23)
    (root / "ignore.bin").write_bytes(b"no")

    models = discover_gguf_models(root)

    assert [model.path for model in models] == [first.resolve(), second.resolve()]
    assert [model.size_bytes for model in models] == [17, 23]
    assert all(model.format == "gguf" for model in models)
    assert all(model.quantization is None for model in models)


def test_llama_cpp_command_exposes_current_offload_controls(tmp_path):
    model = tmp_path / "model.gguf"
    model.write_bytes(b"gguf")
    settings = LlamaCppSettings(
        model_path=model,
        binary="llama-server",
        host="127.0.0.1",
        port=11435,
        gpu_layers="all",
        split_mode="tensor",
        devices=("CUDA0", "CUDA1"),
        tensor_split=(3.0, 1.0),
        main_gpu=0,
        fit=False,
        fit_target_mib=(768, 512),
        context_size=8192,
        parallel=2,
    )

    command = build_llama_cpp_command(settings)

    assert command[:7] == [
        "llama-server",
        "-m",
        str(model.resolve()),
        "--host",
        "127.0.0.1",
        "--port",
        "11435",
    ]
    assert command[command.index("--n-gpu-layers") + 1] == "all"
    assert command[command.index("--split-mode") + 1] == "tensor"
    assert command[command.index("--device") + 1] == "CUDA0,CUDA1"
    assert command[command.index("--tensor-split") + 1] == "3,1"
    assert command[command.index("--main-gpu") + 1] == "0"
    assert command[command.index("--fit") + 1] == "off"
    assert command[command.index("--fit-target") + 1] == "768,512"
    assert command[command.index("--ctx-size") + 1] == "8192"
    assert command[command.index("--parallel") + 1] == "2"


def test_cpu_offload_forces_zero_gpu_layers(tmp_path):
    model = tmp_path / "cpu.gguf"
    model.write_bytes(b"gguf")
    settings = LlamaCppSettings(model_path=model, offload_mode="cpu", gpu_layers="all")

    command = build_llama_cpp_command(settings)

    assert command[command.index("--n-gpu-layers") + 1] == "0"
    assert "--device" not in command
    assert "--tensor-split" not in command


def test_nvidia_hardware_probe_parser_keeps_memory_explicit():
    devices = parse_nvidia_smi(
        "0, NVIDIA RTX 4090, 24564, 20111\n1, NVIDIA RTX 2060, 6144, 5120\n"
    )

    assert devices == (
        HardwareDevice("nvidia:0", "gpu", "NVIDIA RTX 4090", "cuda", 24564 << 20, 20111 << 20),
        HardwareDevice("nvidia:1", "gpu", "NVIDIA RTX 2060", "cuda", 6144 << 20, 5120 << 20),
    )


def test_fit_assessment_is_explicitly_only_a_memory_floor(tmp_path):
    model_path = tmp_path / "model.gguf"
    model_path.write_bytes(b"x" * 64)
    model = ModelCandidate("model", model_path.resolve(), "gguf", None, 64)
    profile = HardwareProfile(
        system="Linux",
        machine="x86_64",
        cpu_count=32,
        ram_total_bytes=256,
        ram_available_bytes=128,
        devices=(),
    )

    assessment = assess_model_fit(model, profile)

    assert assessment.verdict == "possible"
    assert "runtime overhead" in assessment.reason.lower()


def test_managed_runtime_owns_process_lifecycle_without_shell(tmp_path):
    model = tmp_path / "model.gguf"
    model.write_bytes(b"gguf")
    settings = LlamaCppSettings(model_path=model, startup_timeout=1.0)
    calls = []
    readiness = iter((False, True))

    class Process:
        returncode = None
        terminated = False

        def poll(self):
            return self.returncode

        def terminate(self):
            self.terminated = True
            self.returncode = 0

        def wait(self, timeout=None):
            return self.returncode

        def kill(self):
            self.returncode = -9

    process = Process()

    def process_factory(argv, **kwargs):
        calls.append((argv, kwargs))
        return process

    runtime = ManagedLlamaCppRuntime(
        settings,
        process_factory=process_factory,
        readiness_probe=lambda _host, _port: next(readiness),
        sleeper=lambda _seconds: None,
    )

    runtime.start()
    runtime.stop()

    assert calls[0][0][0] == "llama-server"
    assert "shell" not in calls[0][1]
    assert process.terminated is True


def test_llama_cpp_direct_client_is_openai_compatible_without_api_key():
    client = LLMClient(provider="llama_cpp")

    headers, payload = client.serialize_request("hello", system_prompt="system")

    assert client.model == "local"
    assert client.endpoint == "http://127.0.0.1:11435/v1/chat/completions"
    assert "Authorization" not in headers
    assert payload["messages"][0] == {"role": "system", "content": "system"}


def test_invalid_llama_cpp_split_configuration_fails_closed(tmp_path):
    model = tmp_path / "model.gguf"
    model.write_bytes(b"gguf")

    with pytest.raises(ValueError):
        LlamaCppSettings(
            model_path=model,
            split_mode="tensor",
            tensor_split=(1.0,),
            devices=("CUDA0", "CUDA1"),
        )
