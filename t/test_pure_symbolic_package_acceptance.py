"""Installed-layout acceptance for Zara's pure-symbolic wheel resources.

This is packaging evidence, not a second runtime.  The test builds the real
wheel, runs the canonical :class:`PureSymbolicRuntimeBackend` from that wheel's
installed data layout, removes provider credentials, and syscall-fences network
sockets.  The resulting turn must still prove the exact-zero provider/model
contract.
"""

from __future__ import annotations

import os
import pathlib
import subprocess
import sys
import zipfile

import pytest


ROOT = pathlib.Path(__file__).resolve().parent.parent
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
def symbolic_wheel(tmp_path_factory: pytest.TempPathFactory) -> pathlib.Path:
    dist_dir = tmp_path_factory.mktemp("symbolic-wheel")
    result = subprocess.run(
        [sys.executable, "setup.py", "bdist_wheel", "--dist-dir", str(dist_dir)],
        cwd=ROOT,
        capture_output=True,
        text=True,
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


def test_wheel_contains_canonical_symbolic_store_and_runtime_resources(symbolic_wheel):
    with zipfile.ZipFile(symbolic_wheel) as archive:
        names = archive.namelist()

    assert "zara/conversation_schema.sql" in names
    assert "zara/runtime/pure_symbolic_backend.py" in names
    assert "zara/desktop/conversation/symbolic_projection.py" in names
    assert any(
        name.endswith("share/zarathushtra/modules/symbolic_dialogue_turn.pl")
        for name in names
    )
    assert any(
        "/share/zarathushtra/contracts/zara-expert-v1/" in f"/{name}"
        for name in names
    )


def test_wheel_executes_pure_symbolic_turn_without_credentials_or_network(
    symbolic_wheel,
    tmp_path,
):
    wheel_root = tmp_path / "wheel"
    wheel_root.mkdir()
    with zipfile.ZipFile(symbolic_wheel) as archive:
        archive.extractall(wheel_root)

    data_prefixes = list(wheel_root.glob("*.data/data"))
    assert len(data_prefixes) == 1, (
        "wheel must expose one install data prefix, got " + repr(data_prefixes)
    )
    prefix = data_prefixes[0]
    share = prefix / "share" / "zarathushtra"
    assert (share / "main.pl").is_file()
    assert (share / "modules" / "symbolic_dialogue_turn.pl").is_file()

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
            "ZARA_TEST_WHEEL_PREFIX": str(prefix),
            "ZARA_TEST_WHEEL_ROOT": str(wheel_root),
        }
    )

    script = r'''
import asyncio
import os
import socket
import sys
from pathlib import Path

for key in (
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
):
    assert key not in os.environ, f"provider environment leaked into smoke test: {key}"

wheel_root = Path(os.environ["ZARA_TEST_WHEEL_ROOT"]).resolve()
sys.prefix = os.environ["ZARA_TEST_WHEEL_PREFIX"]

_real_socket = socket.socket


def _socket(family=socket.AF_INET, *args, **kwargs):
    if family in (socket.AF_INET, socket.AF_INET6):
        raise AssertionError("pure-symbolic package attempted a network socket")
    return _real_socket(family, *args, **kwargs)


def _connection(*_args, **_kwargs):
    raise AssertionError("pure-symbolic package attempted a network connection")


socket.socket = _socket
socket.create_connection = _connection

import zara

package_path = Path(zara.__file__).resolve()
assert package_path.is_relative_to(wheel_root), (
    f"loaded Zara from source/site-packages instead of candidate wheel: {package_path}"
)

from zara.conversation_schema import CONVERSATION_SCHEMA_VERSION, conversation_schema_sql
from zara.runtime.pure_symbolic_backend import PureSymbolicRuntimeBackend

assert CONVERSATION_SCHEMA_VERSION == 4
schema = conversation_schema_sql()
assert "CREATE TABLE IF NOT EXISTS desktop_symbolic_projections" in schema
assert "max_model_calls INTEGER NOT NULL DEFAULT 1" in schema


async def main():
    backend = PureSymbolicRuntimeBackend()
    await backend.start()
    try:
        result = await backend.submit_turn(
            "hello",
            turn_id="package-smoke-turn",
            conversation_id="package-smoke-conversation",
        )
    finally:
        await backend.stop()

    metadata = result.metadata
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
    assert isinstance(result.response, str) and result.response.strip()


asyncio.run(main())
'''

    result = subprocess.run(
        [sys.executable, "-P", "-c", script],
        cwd=tmp_path,
        env=env,
        capture_output=True,
        text=True,
    )
    assert result.returncode == 0, (
        "candidate wheel pure-symbolic smoke failed\nstdout:\n{}\nstderr:\n{}".format(
            result.stdout,
            result.stderr,
        )
    )
