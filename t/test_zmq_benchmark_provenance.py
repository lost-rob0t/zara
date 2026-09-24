from __future__ import annotations

import importlib.util
import sys
from pathlib import Path


def _benchmark_module():
    path = Path(__file__).resolve().parents[1] / "scripts" / "benchmark-zmq-e2e.py"
    spec = importlib.util.spec_from_file_location("benchmark_zmq_e2e", path)
    assert spec is not None and spec.loader is not None
    module = importlib.util.module_from_spec(spec)
    sys.modules[spec.name] = module
    spec.loader.exec_module(module)
    return module


def test_zmq_audio_metric_provenance_is_explicit():
    module = _benchmark_module()
    provenance = module._metric_provenance(
        source_sha="a" * 40,
        iterations=50,
    )
    assert provenance == {
        "source_sha": "a" * 40,
        "endpoint": "inproc",
        "workload": "audio_input_chunk_ack",
        "payload_bytes": module.AUDIO_INPUT_FRAME_BYTES,
        "warmup_iterations": 50,
        "measured_iterations": 50,
        "voice_ingress": False,
        "classification": "local_loopback_protocol_rtt",
        "device_result": False,
        "wan_result": False,
        "audio_end_to_end_result": False,
    }
