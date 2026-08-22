from pathlib import Path

from zara.server import default_zmq_endpoint


def test_default_zmq_endpoint_is_owner_private_ipc_under_runtime_directory(tmp_path):
    runtime_dir = tmp_path / "runtime"

    endpoint = default_zmq_endpoint(runtime_dir)

    assert endpoint == f"ipc://{runtime_dir / 'zara-server.sock'}"
    assert not endpoint.startswith("tcp://")


def test_default_zmq_endpoint_accepts_string_runtime_directory(tmp_path):
    runtime_dir = tmp_path / "runtime"

    endpoint = default_zmq_endpoint(str(runtime_dir))

    assert endpoint == f"ipc://{Path(runtime_dir) / 'zara-server.sock'}"
