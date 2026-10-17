import http.client
import json

from zara.agent.tools.file_tools import FileTooling, FileToolingConfig
from zara.runtime.host_read_callback import HostReadCallbackServer
from zara.runtime.turn_context import TurnCapabilityLease


def request(callback, payload, token="test-secret"):
    connection = http.client.HTTPConnection("127.0.0.1", callback.port, timeout=3)
    try:
        connection.request(
            "POST",
            "/zara-host-tool/v1/read",
            body=json.dumps(payload),
            headers={"Authorization": f"Bearer {token}", "Content-Type": "application/json"},
        )
        response = connection.getresponse()
        return response.status, json.loads(response.read())
    finally:
        connection.close()


def payload(**changes):
    base = {
        "protocol": "ZARA-HOST-TOOL/1",
        "request_id": "turn-1",
        "tool_call_id": "call-1",
        "generation": 7,
        "name": "file_read",
        "args": {"path": "note.txt"},
    }
    return {**base, **changes}


def test_read_callback_only_serves_registered_active_turn(tmp_path):
    (tmp_path / "note.txt").write_text("hello\n")
    tooling = FileTooling(FileToolingConfig(base_dir=tmp_path))
    callback = HostReadCallbackServer(tooling, token="test-secret", generation=7)
    callback.start()
    try:
        lease = TurnCapabilityLease("turn-1")
        callback.register_turn("turn-1", lease)
        status, body = request(callback, payload())
        assert status == 200
        assert body == {
            "protocol": "ZARA-HOST-TOOL/1",
            "request_id": "turn-1",
            "tool_call_id": "call-1",
            "generation": 7,
            "status": "ok",
            "value": {"content": "hello"},
        }
        lease.invalidate()
        assert request(callback, payload())[1]["status"] == "denied"
    finally:
        callback.stop()


def test_read_callback_rejects_wrong_authority_and_outside_repo(tmp_path):
    (tmp_path / "note.txt").write_text("hello\n")
    outside = tmp_path.parent / "outside.txt"
    outside.write_text("private\n")
    tooling = FileTooling(FileToolingConfig(base_dir=tmp_path))
    callback = HostReadCallbackServer(tooling, token="test-secret", generation=7)
    callback.start()
    try:
        callback.register_turn("turn-1", TurnCapabilityLease("turn-1"))
        assert request(callback, payload(), token="wrong")[1]["status"] == "denied"
        assert request(callback, payload(generation=8))[1]["status"] == "denied"
        assert request(callback, payload(name="write_file"))[1]["status"] == "denied"
        assert request(callback, payload(args={"path": str(outside)}))[1]["status"] == "denied"
    finally:
        callback.stop()


def test_read_callback_rejects_stale_registration_and_malformed_arguments(tmp_path):
    (tmp_path / "note.txt").write_text("hello")
    callback = HostReadCallbackServer(
        FileTooling(FileToolingConfig(base_dir=tmp_path)), token="test-secret", generation=7
    )
    callback.start()
    try:
        stale = TurnCapabilityLease("turn-1")
        stale.invalidate()
        try:
            callback.register_turn("turn-1", stale)
        except RuntimeError:
            pass
        else:
            raise AssertionError("stale turn was registered")
        assert request(callback, payload())[1]["status"] == "denied"
        callback.register_turn("turn-1", TurnCapabilityLease("turn-1"))
        assert request(callback, payload(args={}))[1]["status"] == "denied"
        assert request(callback, payload(args={"path": "note.txt", "max_lines": 0}))[1]["status"] == "denied"
        callback.unregister_turn("turn-1")
        assert request(callback, payload())[1]["status"] == "denied"
    finally:
        callback.stop()
