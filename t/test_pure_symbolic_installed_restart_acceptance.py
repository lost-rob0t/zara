"""Installed-wheel restart acceptance for Desktop pure-symbolic conversation.

This exercises the shipped runtime and canonical ConversationStore in two fresh
Python processes.  No provider credential or network access is available in
either process.  A clarification written by process one must therefore be the
only state process two can use to resolve the follow-up.
"""

from __future__ import annotations

import os
from pathlib import Path
import subprocess
import sys
import zipfile

import pytest


ROOT = Path(__file__).resolve().parent.parent
PROVIDER_ENV = (
    "ANTHROPIC_API_KEY",
    "AZURE_OPENAI_API_KEY",
    "AZURE_OPENAI_ENDPOINT",
    "COHERE_API_KEY",
    "DEEPSEEK_API_KEY",
    "GEMINI_API_KEY",
    "GOOGLE_API_KEY",
    "GROQ_API_KEY",
    "HF_TOKEN",
    "HUGGINGFACEHUB_API_TOKEN",
    "MISTRAL_API_KEY",
    "OLLAMA_HOST",
    "OPENAI_API_KEY",
    "OPENAI_BASE_URL",
    "OPENROUTER_API_KEY",
    "TOGETHER_API_KEY",
    "ZAI_API_KEY",
)


@pytest.fixture(scope="module")
def installed_symbolic_wheel(tmp_path_factory: pytest.TempPathFactory) -> Path:
    dist_dir = tmp_path_factory.mktemp("installed-symbolic-restart-wheel")
    result = subprocess.run(
        [sys.executable, "setup.py", "bdist_wheel", "--dist-dir", str(dist_dir)],
        cwd=ROOT,
        capture_output=True,
        text=True,
        timeout=120,
    )
    assert result.returncode == 0, (
        "wheel build failed\nstdout:\n{}\nstderr:\n{}".format(
            result.stdout,
            result.stderr,
        )
    )
    wheels = list(dist_dir.glob("*.whl"))
    assert len(wheels) == 1, f"expected exactly one wheel, got {wheels}"
    return wheels[0]


def _installed_env(wheel: Path, tmp_path: Path) -> tuple[dict[str, str], Path]:
    wheel_root = tmp_path / "wheel"
    wheel_root.mkdir()
    with zipfile.ZipFile(wheel) as archive:
        archive.extractall(wheel_root)

    data_prefixes = list(wheel_root.glob("*.data/data"))
    assert len(data_prefixes) == 1, (
        "wheel must expose one install data prefix, got " + repr(data_prefixes)
    )

    home = tmp_path / "home"
    home.mkdir()
    env = os.environ.copy()
    for key in PROVIDER_ENV:
        env.pop(key, None)
    env.update(
        {
            "HOME": str(home),
            "XDG_CONFIG_HOME": str(home / ".config"),
            "PYTHONNOUSERSITE": "1",
            "PYTHONPATH": str(wheel_root),
            "ZARA_TEST_WHEEL_PREFIX": str(data_prefixes[0]),
            "ZARA_TEST_WHEEL_ROOT": str(wheel_root),
            "ZARA_TEST_CONVERSATION_DB": str(tmp_path / "conversation.db"),
        }
    )
    return env, wheel_root


def _run_installed_phase(*, env: dict[str, str], cwd: Path, phase: str) -> None:
    script = r'''
import asyncio
import os
import socket
import sys
from pathlib import Path

PROVIDER_ENV = (
    "ANTHROPIC_API_KEY",
    "AZURE_OPENAI_API_KEY",
    "AZURE_OPENAI_ENDPOINT",
    "COHERE_API_KEY",
    "DEEPSEEK_API_KEY",
    "GEMINI_API_KEY",
    "GOOGLE_API_KEY",
    "GROQ_API_KEY",
    "HF_TOKEN",
    "HUGGINGFACEHUB_API_TOKEN",
    "MISTRAL_API_KEY",
    "OLLAMA_HOST",
    "OPENAI_API_KEY",
    "OPENAI_BASE_URL",
    "OPENROUTER_API_KEY",
    "TOGETHER_API_KEY",
    "ZAI_API_KEY",
)
for key in PROVIDER_ENV:
    assert key not in os.environ, f"provider environment leaked into acceptance: {key}"

wheel_root = Path(os.environ["ZARA_TEST_WHEEL_ROOT"]).resolve()
sys.prefix = os.environ["ZARA_TEST_WHEEL_PREFIX"]

_real_socket = socket.socket


def _socket(family=socket.AF_INET, *args, **kwargs):
    if family in (socket.AF_INET, socket.AF_INET6):
        raise AssertionError("pure-symbolic installed acceptance attempted network access")
    return _real_socket(family, *args, **kwargs)


def _connection(*_args, **_kwargs):
    raise AssertionError("pure-symbolic installed acceptance attempted a network connection")


socket.socket = _socket
socket.create_connection = _connection

import zara

package_path = Path(zara.__file__).resolve()
assert package_path.is_relative_to(wheel_root), (
    f"loaded Zara outside candidate wheel: {package_path}"
)

from zara.database import DatabaseManager
from zara.desktop.conversation import ConversationStore
from zara.desktop.conversation.symbolic_runtime import PureSymbolicProjectionAdapter
from zara.runtime.pure_symbolic_backend import PureSymbolicRuntimeBackend

DB_PATH = Path(os.environ["ZARA_TEST_CONVERSATION_DB"])
CONVERSATION_ID = "installed-desktop-restart"


def assert_zero(metadata):
    assert metadata["route"] == "pure_symbolic"
    assert metadata["providers_enabled"] is False
    assert metadata["renderer"] == "symbolic-dcg/v1"
    for key in (
        "max_provider_calls",
        "max_model_calls",
        "provider_calls",
        "model_calls",
    ):
        value = metadata[key]
        assert type(value) is int and value == 0, f"{key}={value!r}"


async def phase_one():
    database = DatabaseManager(DB_PATH)
    store = ConversationStore(database)
    conversation = store.create_conversation(
        "Installed symbolic restart",
        conversation_id=CONVERSATION_ID,
    )
    backend = PureSymbolicRuntimeBackend(
        projection_adapter=PureSymbolicProjectionAdapter(store),
    )
    await backend.start()
    try:
        result = await backend.submit_turn(
            "timer",
            turn_id="installed-turn-1",
            conversation_id=conversation.id,
        )
        assert result.response == "How long should I set the timer for?"
        assert result.metadata["response_act"].startswith("clarify(")
        assert_zero(result.metadata)
        backend.commit_turn_result(
            result,
            turn_id="installed-turn-1",
            conversation_id=conversation.id,
        )
    finally:
        await backend.stop()
        database.close()


async def phase_two():
    database = DatabaseManager(DB_PATH)
    store = ConversationStore(database)
    backend = PureSymbolicRuntimeBackend(
        projection_adapter=PureSymbolicProjectionAdapter(store),
    )
    await backend.start()
    try:
        result = await backend.submit_turn(
            "5 minutes",
            turn_id="installed-turn-2",
            conversation_id=CONVERSATION_ID,
        )
        assert result.response == (
            "That action needs capability-checked execution before I can report success."
        )
        assert result.metadata["response_act"].startswith("dispatch_required(")
        assert_zero(result.metadata)
        backend.commit_turn_result(
            result,
            turn_id="installed-turn-2",
            conversation_id=CONVERSATION_ID,
        )

        projection = store.load_symbolic_projection(CONVERSATION_ID)
        assert projection is not None
        projection.assert_pure_symbolic()
        assert projection.projection_generation == 2
        assert projection.runtime_generation == 2
        assert projection.dialogue_act == "dispatch_required"
        assert "completed_frame" in projection.dialogue_state["prolog_context_term"]
        assert projection.providers_enabled is False
        assert projection.max_model_calls == 0
        assert projection.provider_calls == 0
        assert projection.model_calls == 0
    finally:
        await backend.stop()
        database.close()


phase = os.environ["ZARA_TEST_PHASE"]
if phase == "one":
    asyncio.run(phase_one())
elif phase == "two":
    asyncio.run(phase_two())
else:
    raise AssertionError(f"unknown phase: {phase!r}")
'''

    phase_env = env.copy()
    phase_env["ZARA_TEST_PHASE"] = phase
    result = subprocess.run(
        [sys.executable, "-P", "-c", script],
        cwd=cwd,
        env=phase_env,
        capture_output=True,
        text=True,
        timeout=45,
    )
    assert result.returncode == 0, (
        f"installed pure-symbolic restart phase {phase} failed\n"
        f"stdout:\n{result.stdout}\nstderr:\n{result.stderr}"
    )


def test_installed_desktop_clarification_survives_process_recreation_with_zero_calls(
    installed_symbolic_wheel: Path,
    tmp_path: Path,
) -> None:
    env, wheel_root = _installed_env(installed_symbolic_wheel, tmp_path)

    _run_installed_phase(env=env, cwd=tmp_path, phase="one")
    _run_installed_phase(env=env, cwd=tmp_path, phase="two")

    assert (tmp_path / "conversation.db").is_file()
    assert wheel_root.is_dir()
