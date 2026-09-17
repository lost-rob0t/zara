"""Self-hosted model discovery, hardware inspection, and llama.cpp lifecycle."""

from __future__ import annotations

import csv
import io
import os
import platform
import shutil
import socket
import subprocess
import time
from dataclasses import dataclass, field
from pathlib import Path
from typing import Callable, Mapping, Optional, Sequence


MIB = 1 << 20
MAX_DISCOVERED_MODELS = 256


@dataclass(frozen=True)
class HardwareDevice:
    device_id: str
    kind: str
    name: str
    backend: str
    memory_total_bytes: Optional[int] = None
    memory_available_bytes: Optional[int] = None


@dataclass(frozen=True)
class HardwareProfile:
    system: str
    machine: str
    cpu_count: Optional[int]
    ram_total_bytes: Optional[int]
    ram_available_bytes: Optional[int]
    devices: tuple[HardwareDevice, ...] = field(default_factory=tuple)
    runtime_devices: tuple[str, ...] = field(default_factory=tuple)

    def summary(self) -> str:
        parts = [f"{self.system or 'Unknown OS'} {self.machine or 'unknown'}"]
        if self.cpu_count is not None:
            parts.append(f"{self.cpu_count} logical CPUs")
        if self.ram_total_bytes is not None:
            total = format_bytes(self.ram_total_bytes)
            if self.ram_available_bytes is None:
                parts.append(f"RAM {total}")
            else:
                parts.append(f"RAM {format_bytes(self.ram_available_bytes)} available / {total}")
        for device in self.devices:
            memory = ""
            if device.memory_total_bytes is not None:
                memory = f" ({format_bytes(device.memory_total_bytes)})"
            parts.append(f"{device.name} [{device.backend}]{memory}")
        if self.runtime_devices:
            parts.append("llama.cpp: " + "; ".join(self.runtime_devices))
        return " • ".join(parts)


@dataclass(frozen=True)
class ModelCandidate:
    model_id: str
    path: Path
    format: str
    quantization: Optional[str]
    size_bytes: Optional[int]


@dataclass(frozen=True)
class FitAssessment:
    verdict: str
    reason: str


@dataclass(frozen=True)
class LlamaCppSettings:
    model_path: Path
    binary: str = "llama-server"
    host: str = "127.0.0.1"
    port: int = 11435
    offload_mode: str = "auto"
    gpu_layers: str = "auto"
    split_mode: str = "layer"
    devices: tuple[str, ...] = field(default_factory=tuple)
    tensor_split: tuple[float, ...] = field(default_factory=tuple)
    main_gpu: int = 0
    fit: bool = True
    fit_target_mib: tuple[int, ...] = (1024,)
    context_size: int = 4096
    parallel: int = 1
    startup_timeout: float = 15.0

    def __post_init__(self) -> None:
        if not isinstance(self.model_path, Path):
            object.__setattr__(self, "model_path", Path(self.model_path))
        if not self.binary:
            raise ValueError("llama.cpp binary must not be empty")
        if not self.host:
            raise ValueError("llama.cpp host must not be empty")
        if not 1 <= self.port <= 65535:
            raise ValueError("llama.cpp port must be between 1 and 65535")
        if self.offload_mode not in {"auto", "cpu", "single", "multi"}:
            raise ValueError("invalid llama.cpp offload mode")
        if self.split_mode not in {"none", "layer", "row", "tensor"}:
            raise ValueError("invalid llama.cpp split mode")
        if self.gpu_layers not in {"auto", "all"}:
            try:
                if int(self.gpu_layers) < 0:
                    raise ValueError
            except (TypeError, ValueError) as error:
                raise ValueError("gpu_layers must be auto, all, or a non-negative integer") from error
        if self.main_gpu < 0:
            raise ValueError("main_gpu cannot be negative")
        if self.context_size < 128:
            raise ValueError("context_size must be at least 128")
        if self.parallel < 1:
            raise ValueError("parallel must be positive")
        if self.startup_timeout <= 0:
            raise ValueError("startup_timeout must be positive")
        if any(not device.strip() for device in self.devices):
            raise ValueError("device ids must not be empty")
        if any(value <= 0 for value in self.tensor_split):
            raise ValueError("tensor_split proportions must be positive")
        if self.tensor_split and self.devices and len(self.tensor_split) != len(self.devices):
            raise ValueError("tensor_split must provide one proportion per selected device")
        if any(value < 0 for value in self.fit_target_mib):
            raise ValueError("fit_target_mib cannot be negative")
        if len(self.fit_target_mib) > 1 and self.devices and len(self.fit_target_mib) != len(self.devices):
            raise ValueError("fit_target_mib must be one value or one value per selected device")

    @classmethod
    def from_mapping(cls, values: Mapping[str, object]) -> "LlamaCppSettings":
        model_text = str(values.get("model_path", "")).strip()
        if not model_text:
            raise ValueError("local_models.model_path is required for managed llama.cpp")
        devices = _split_strings(values.get("devices", ""))
        tensor_split = tuple(float(value) for value in _split_strings(values.get("tensor_split", "")))
        fit_target = tuple(int(value) for value in _split_strings(values.get("fit_target_mib", "1024")))
        if not fit_target:
            fit_target = (1024,)
        return cls(
            model_path=Path(model_text).expanduser(),
            binary=str(values.get("binary", "llama-server")).strip(),
            host=str(values.get("host", "127.0.0.1")).strip(),
            port=int(values.get("port", 11435)),
            offload_mode=str(values.get("offload_mode", "auto")).strip().lower(),
            gpu_layers=str(values.get("gpu_layers", "auto")).strip().lower(),
            split_mode=str(values.get("split_mode", "layer")).strip().lower(),
            devices=devices,
            tensor_split=tensor_split,
            main_gpu=int(values.get("main_gpu", 0)),
            fit=bool(values.get("fit", True)),
            fit_target_mib=fit_target,
            context_size=int(values.get("context_size", 4096)),
            parallel=int(values.get("parallel", 1)),
            startup_timeout=float(values.get("startup_timeout", 15.0)),
        )


def _split_strings(value: object) -> tuple[str, ...]:
    if value is None:
        return ()
    if isinstance(value, str):
        return tuple(part.strip() for part in value.split(",") if part.strip())
    if isinstance(value, Sequence):
        return tuple(str(part).strip() for part in value if str(part).strip())
    return (str(value).strip(),) if str(value).strip() else ()


def _render_numbers(values: Sequence[float | int]) -> str:
    rendered = []
    for value in values:
        numeric = float(value)
        rendered.append(str(int(numeric)) if numeric.is_integer() else str(numeric))
    return ",".join(rendered)


def build_llama_cpp_command(settings: LlamaCppSettings) -> list[str]:
    model = settings.model_path.expanduser().resolve()
    command = [
        settings.binary,
        "-m",
        str(model),
        "--host",
        settings.host,
        "--port",
        str(settings.port),
    ]
    gpu_layers = "0" if settings.offload_mode == "cpu" else settings.gpu_layers
    command.extend(["--n-gpu-layers", gpu_layers])
    if settings.offload_mode != "cpu":
        command.extend(["--split-mode", settings.split_mode])
        if settings.devices:
            command.extend(["--device", ",".join(settings.devices)])
        if settings.tensor_split:
            command.extend(["--tensor-split", _render_numbers(settings.tensor_split)])
        command.extend(["--main-gpu", str(settings.main_gpu)])
    command.extend(["--fit", "on" if settings.fit else "off"])
    if settings.fit_target_mib:
        command.extend(["--fit-target", _render_numbers(settings.fit_target_mib)])
    command.extend(["--ctx-size", str(settings.context_size)])
    command.extend(["--parallel", str(settings.parallel)])
    return command


def discover_gguf_models(root: Path | str, limit: int = MAX_DISCOVERED_MODELS) -> list[ModelCandidate]:
    root_path = Path(root).expanduser()
    if limit < 1 or not root_path.is_dir():
        return []
    root_resolved = root_path.resolve()
    candidates: list[ModelCandidate] = []
    for path in sorted(root_path.rglob("*.gguf"), key=lambda item: str(item).lower()):
        if len(candidates) >= limit or path.is_symlink() or not path.is_file():
            continue
        resolved = path.resolve()
        try:
            relative = resolved.relative_to(root_resolved)
        except ValueError:
            continue
        candidates.append(
            ModelCandidate(
                model_id=relative.with_suffix("").as_posix(),
                path=resolved,
                format="gguf",
                quantization=None,
                size_bytes=resolved.stat().st_size,
            )
        )
    return candidates


def assess_model_fit(model: ModelCandidate, profile: HardwareProfile) -> FitAssessment:
    if model.size_bytes is None:
        return FitAssessment("unknown", "Model size is unknown; no memory fit estimate is available.")
    available_values = []
    if profile.ram_available_bytes is not None:
        available_values.append(profile.ram_available_bytes)
    available_values.extend(
        device.memory_available_bytes
        for device in profile.devices
        if device.memory_available_bytes is not None
    )
    if not available_values:
        return FitAssessment("unknown", "Available RAM/accelerator memory is unknown.")
    available = sum(available_values)
    if available < model.size_bytes:
        return FitAssessment(
            "insufficient",
            "The model file alone exceeds reported available memory; runtime overhead and KV cache need more.",
        )
    return FitAssessment(
        "possible",
        "The model file fits reported available memory; runtime overhead and KV cache are not included.",
    )


def parse_nvidia_smi(output: str) -> tuple[HardwareDevice, ...]:
    devices = []
    for row in csv.reader(io.StringIO(output)):
        if len(row) != 4:
            continue
        index, name, total_mib, free_mib = (item.strip() for item in row)
        try:
            total = int(total_mib) * MIB
            available = int(free_mib) * MIB
        except ValueError:
            continue
        devices.append(
            HardwareDevice(
                device_id=f"nvidia:{index}",
                kind="gpu",
                name=name,
                backend="cuda",
                memory_total_bytes=total,
                memory_available_bytes=available,
            )
        )
    return tuple(devices)


def _linux_memory() -> tuple[Optional[int], Optional[int]]:
    meminfo = Path("/proc/meminfo")
    if meminfo.is_file():
        values: dict[str, int] = {}
        try:
            for line in meminfo.read_text(encoding="utf-8").splitlines():
                name, separator, rest = line.partition(":")
                if not separator:
                    continue
                pieces = rest.strip().split()
                if not pieces:
                    continue
                values[name] = int(pieces[0]) * 1024
        except (OSError, ValueError):
            values = {}
        if "MemTotal" in values:
            return values["MemTotal"], values.get("MemAvailable")
    try:
        page_size = os.sysconf("SC_PAGE_SIZE")
        total_pages = os.sysconf("SC_PHYS_PAGES")
        available_pages = os.sysconf("SC_AVPHYS_PAGES")
        return page_size * total_pages, page_size * available_pages
    except (AttributeError, OSError, ValueError):
        return None, None


def _nvidia_devices() -> tuple[HardwareDevice, ...]:
    binary = shutil.which("nvidia-smi")
    if binary is None:
        return ()
    try:
        result = subprocess.run(
            [
                binary,
                "--query-gpu=index,name,memory.total,memory.free",
                "--format=csv,noheader,nounits",
            ],
            check=False,
            capture_output=True,
            text=True,
            timeout=2.0,
        )
    except (OSError, subprocess.TimeoutExpired):
        return ()
    if result.returncode != 0:
        return ()
    return parse_nvidia_smi(result.stdout)


def _llama_runtime_devices(binary: str) -> tuple[str, ...]:
    resolved = shutil.which(binary) if os.path.sep not in binary else binary
    if not resolved:
        return ()
    try:
        result = subprocess.run(
            [resolved, "--list-devices"],
            check=False,
            capture_output=True,
            text=True,
            timeout=2.0,
        )
    except (OSError, subprocess.TimeoutExpired):
        return ()
    if result.returncode != 0:
        return ()
    lines = []
    for line in (result.stdout + "\n" + result.stderr).splitlines():
        text = line.strip()
        if not text or text.lower().startswith("available devices"):
            continue
        if len(text) <= 256:
            lines.append(text)
        if len(lines) >= 32:
            break
    return tuple(lines)


def probe_hardware(llama_cpp_binary: str = "llama-server") -> HardwareProfile:
    total, available = _linux_memory()
    return HardwareProfile(
        system=platform.system(),
        machine=platform.machine(),
        cpu_count=os.cpu_count(),
        ram_total_bytes=total,
        ram_available_bytes=available,
        devices=_nvidia_devices(),
        runtime_devices=_llama_runtime_devices(llama_cpp_binary),
    )


def format_bytes(value: int) -> str:
    amount = float(value)
    for suffix in ("B", "KiB", "MiB", "GiB", "TiB"):
        if amount < 1024.0 or suffix == "TiB":
            return f"{amount:.1f} {suffix}" if suffix != "B" else f"{int(amount)} B"
        amount /= 1024.0
    return f"{amount:.1f} TiB"


def _port_ready(host: str, port: int) -> bool:
    connect_host = "127.0.0.1" if host in {"0.0.0.0", "::"} else host
    try:
        with socket.create_connection((connect_host, port), timeout=0.2):
            return True
    except OSError:
        return False


class ManagedLlamaCppRuntime:
    def __init__(
        self,
        settings: LlamaCppSettings,
        *,
        process_factory: Callable[..., subprocess.Popen] = subprocess.Popen,
        readiness_probe: Callable[[str, int], bool] = _port_ready,
        monotonic: Callable[[], float] = time.monotonic,
        sleeper: Callable[[float], None] = time.sleep,
    ) -> None:
        self.settings = settings
        self._process_factory = process_factory
        self._readiness_probe = readiness_probe
        self._monotonic = monotonic
        self._sleeper = sleeper
        self._process: Optional[subprocess.Popen] = None

    @property
    def running(self) -> bool:
        return self._process is not None and self._process.poll() is None

    def start(self) -> None:
        if self.running:
            return
        model = self.settings.model_path.expanduser().resolve()
        if not model.is_file() or model.suffix.lower() != ".gguf":
            raise ValueError(f"Managed llama.cpp model is not a readable GGUF file: {model}")
        if self._readiness_probe(self.settings.host, self.settings.port):
            raise RuntimeError(f"llama.cpp port {self.settings.host}:{self.settings.port} is already in use")
        command = build_llama_cpp_command(self.settings)
        try:
            self._process = self._process_factory(
                command,
                stdin=subprocess.DEVNULL,
                stdout=subprocess.DEVNULL,
                stderr=subprocess.DEVNULL,
                start_new_session=os.name != "nt",
            )
        except OSError as error:
            self._process = None
            raise RuntimeError(f"Could not start llama.cpp: {error}") from error
        deadline = self._monotonic() + self.settings.startup_timeout
        while True:
            if self._readiness_probe(self.settings.host, self.settings.port):
                return
            returncode = self._process.poll()
            if returncode is not None:
                self._process = None
                raise RuntimeError(f"llama.cpp exited during startup with status {returncode}")
            if self._monotonic() >= deadline:
                self.stop()
                raise TimeoutError("llama.cpp did not become ready before startup timeout")
            self._sleeper(0.05)

    def stop(self) -> None:
        process = self._process
        self._process = None
        if process is None or process.poll() is not None:
            return
        process.terminate()
        try:
            process.wait(timeout=3.0)
        except subprocess.TimeoutExpired:
            process.kill()
            process.wait(timeout=1.0)


__all__ = [
    "FitAssessment",
    "HardwareDevice",
    "HardwareProfile",
    "LlamaCppSettings",
    "ManagedLlamaCppRuntime",
    "ModelCandidate",
    "assess_model_fit",
    "build_llama_cpp_command",
    "discover_gguf_models",
    "format_bytes",
    "parse_nvidia_smi",
    "probe_hardware",
]
