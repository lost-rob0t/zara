from __future__ import annotations

import json
import subprocess
from urllib.request import Request, urlopen

from langchain_core.tools import StructuredTool

from zara.runtime.host_effect_callback import HostEffectCallbackServer
from zara.runtime.turn_context import TurnCapabilityLease
from zara.tasks.effect_journal import RepositoryEffectJournal


def test_approved_effect_dispatches_once_and_replays(tmp_path):
    subprocess.run(["git", "init", "-q", str(tmp_path)], check=True)
    calls = []
    tool = StructuredTool.from_function(
        lambda tasks: calls.append(tasks) or "task-123",
        name="code_tasks", description="Start tasks",
    )
    server = HostEffectCallbackServer(
        {"code_tasks": tool}, RepositoryEffectJournal(tmp_path),
        principal_id="user:test", token="abcdefghijklmnop", generation=7,
    )
    server.start()
    lease = TurnCapabilityLease("turn-1")
    approval = {
        "id": "approval-1", "name": "code_tasks", "effect": "process",
        "args": {"tasks": [{"task": "fix bug", "project": "work"}]},
        "attempt_id": "attempt-1", "effect_fingerprint": "effect-1",
        "idempotency_key": "key-1", "fingerprint": "authority-1",
    }
    server.register_approval("turn-1", approval, lease)
    request = {
        "protocol": "ZARA-HOST-EFFECT/1", "request_id": "turn-1",
        "tool_call_id": "attempt-1", "generation": 7,
        "attempt_id": "attempt-1", "effect_fingerprint": "effect-1",
        "idempotency_key": "key-1", "name": "code_tasks", "args": approval["args"],
    }

    def post(payload, token="abcdefghijklmnop"):
        with urlopen(Request(server.endpoint, data=json.dumps(payload).encode(),
                             headers={"Authorization": f"Bearer {token}", "Content-Type": "application/json"}), timeout=3) as response:
            return json.load(response)

    try:
        assert post(request)["value"] == {"result": "task-123"}
        assert post(request)["value"] == {"result": "task-123"}
        assert len(calls) == 1
        assert post({**request, "idempotency_key": "wrong"})["status"] == "denied"
        assert post(request, token="wrong")["status"] == "denied"
        lease.invalidate()
        assert post(request)["status"] == "denied"
    finally:
        server.stop()
