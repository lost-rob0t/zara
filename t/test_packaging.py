"""Verifies the Python packaging contract from ZARA-019.

These tests build the actual wheel produced by ``setup.py`` and inspect its
contents so they exercise the real packaging pipeline instead of re-asserting
metadata strings. They cover three claims:

* ``main.pl``, ``kb/``, and ``modules/`` are shipped inside the wheel as
  ``data_files`` under ``share/zarathushtra/`` (the same layout the Nix
  packages install to).
* All five console scripts (``zara``, ``zara-wake``, ``zara-console``,
  ``zara-dictate``, ``zara-agent``) are declared in the wheel entry points.
* The runtime dependency list in the wheel METADATA covers the import surface
  used by the runtime modules.
"""

from __future__ import annotations

import io
import pathlib
import subprocess
import sys
import zipfile

import pytest


ROOT = pathlib.Path(__file__).resolve().parent.parent


def _build_wheel(tmp_path: pathlib.Path) -> pathlib.Path:
    dist_dir = tmp_path / "dist"
    dist_dir.mkdir()
    result = subprocess.run(
        [sys.executable, "setup.py", "bdist_wheel", "--dist-dir", str(dist_dir)],
        cwd=ROOT,
        capture_output=True,
        text=True,
    )
    assert result.returncode == 0, (
        "wheel build failed\nstdout:\n{}\nstderr:\n{}".format(result.stdout, result.stderr)
    )
    wheels = list(dist_dir.glob("*.whl"))
    assert wheels, "no wheel produced"
    assert len(wheels) == 1, f"expected one wheel, got {wheels}"
    return wheels[0]


@pytest.fixture(scope="module")
def wheel_path(tmp_path_factory):
    tmp = tmp_path_factory.mktemp("wheel-build")
    return _build_wheel(tmp)


@pytest.fixture(scope="module")
def wheel_names(wheel_path):
    with zipfile.ZipFile(wheel_path) as zf:
        return zf.namelist()


@pytest.fixture(scope="module")
def wheel_metadata(wheel_path):
    with zipfile.ZipFile(wheel_path) as zf:
        metadata_name = next(n for n in zf.namelist() if n.endswith("METADATA"))
        return zf.read(metadata_name).decode("utf-8")


@pytest.fixture(scope="module")
def wheel_entry_points(wheel_path):
    with zipfile.ZipFile(wheel_path) as zf:
        name = next(n for n in zf.namelist() if n.endswith("entry_points.txt"))
        return zf.read(name).decode("utf-8")


def test_wheel_includes_main_pl(wheel_names):
    matches = [n for n in wheel_names if n.endswith("share/zarathushtra/main.pl")]
    assert matches, "main.pl missing from wheel data_files"


def test_wheel_includes_kb_resources(wheel_names):
    kb_pl_files = list((ROOT / "kb").glob("*.pl"))
    assert kb_pl_files, "no kb/*.pl files found in repo to compare against"
    for kb_file in kb_pl_files:
        rel = kb_file.relative_to(ROOT).as_posix()
        suffix = f"share/zarathushtra/{rel}"
        matches = [n for n in wheel_names if n.endswith(suffix)]
        assert matches, f"{rel} missing from wheel"


def test_wheel_includes_modules_resources(wheel_names):
    mod_pl_files = list((ROOT / "modules").glob("*.pl"))
    assert mod_pl_files, "no modules/*.pl files found in repo to compare against"
    for mod_file in mod_pl_files:
        rel = mod_file.relative_to(ROOT).as_posix()
        suffix = f"share/zarathushtra/{rel}"
        matches = [n for n in wheel_names if n.endswith(suffix)]
        assert matches, f"{rel} missing from wheel"


def test_wheel_declares_all_console_scripts(wheel_entry_points):
    expected = {
        "zara = zara.__main__:main",
        "zara-wake = zara.wake:main",
        "zara-console = zara.console:main",
        "zara-dictate = zara.dictate:main",
        "zara-agent = zara.agent_cli:main",
    }
    missing = [line for line in expected if line not in wheel_entry_points]
    assert not missing, (
        "missing console scripts in wheel entry_points.txt:\n  "
        + "\n  ".join(missing)
        + "\n--- entry_points.txt ---\n"
        + wheel_entry_points
    )


def test_wheel_declares_runtime_dependencies(wheel_metadata):
    required_deps = [
        "sounddevice",
        "numpy",
        "faster-whisper",
        "pyswip",
        "edge-tts",
        "elevenlabs",
        "pynput",
        "aiohttp",
        "soundfile",
        "PyYAML",
        "pydantic",
        "httpx",
        "orgparse",
        "langchain",
        "langchain-core",
        "langchain-community",
        "langgraph",
        "langchain-anthropic",
        "anthropic",
        "langchain-openai",
        "openai",
        "langchain-ollama",
        "ollama",
        "chromadb",
        "sentence-transformers",
        "pytest",
        "pytest-asyncio",
    ]

    def declared(dep: str) -> bool:
        prefix = f"Requires-Dist: {dep}"
        prefix_lower = prefix.lower()
        return any(
            line.startswith(prefix) or line.lower().startswith(prefix_lower)
            for line in wheel_metadata.splitlines()
        )

    missing = [dep for dep in required_deps if not declared(dep)]
    assert not missing, (
        "missing dependencies in wheel METADATA:\n  "
        + "\n  ".join(missing)
        + "\n--- METADATA Requires-Dist ---\n"
        + "\n".join(
            line for line in wheel_metadata.splitlines()
            if line.startswith("Requires-Dist")
        )
    )


def test_entrypoint_functions_exist():
    """Each entrypoint must resolve to a real, callable function."""
    from zara import agent_cli, console, dictate, wake
    from zara import __main__ as zara_main

    assert callable(zara_main.main)
    assert callable(wake.main)
    assert callable(console.main)
    assert callable(dictate.main)
    assert callable(agent_cli.main)


def test_find_main_pl_locates_project_root_main():
    """``find_main_pl`` should locate ``main.pl`` at the project root in dev mode."""
    from zara.console import find_main_pl

    located = find_main_pl()
    assert located is not None, "find_main_pl() returned None in dev checkout"
    assert located.name == "main.pl"
    assert located.exists()
