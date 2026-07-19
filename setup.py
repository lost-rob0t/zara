#!/usr/bin/env python3
"""
Zarathustra Voice Assistant — Python packaging metadata.

Nix is the authoritative install surface (see flake.nix and wiki/install.org).
The wheel/sdist produced by this file is a best-effort companion that installs
the Python modules and console scripts plus the Prolog resources under
``<sys.prefix>/share/zarathushtra/``. The runtime looks for ``main.pl`` in the
project root, the Nix store, ``<sys.prefix>/share/zarathushtra/``, and
``/usr/share/zarathushtra`` so the same code path works for ``pip install`` and
``nix build``.
"""

from setuptools import setup, find_packages
from pathlib import Path
import os

ROOT = Path(__file__).resolve().parent

readme = ROOT / "README.org"
long_description = readme.read_text() if readme.exists() else ""


def _prolog_data_files():
    """Yield ``(target, [relpaths])`` pairs that preserve directory structure.

    Included as ``data_files`` so wheels ship the Prolog resources next to the
    Python install in the same layout the Nix packages use. Files are grouped
    by their parent directory so ``kb/intents.pl`` lands at
    ``share/zarathushtra/kb/intents.pl`` (preserving the ``kb/`` directory)
    rather than being flattened into ``share/zarathushtra/intents.pl``. The
    Prolog ``:- use_module('kb/config').`` directives in ``main.pl`` rely on
    this directory being preserved.
    """
    import collections
    import pathlib

    base = pathlib.PurePosixPath("share/zarathushtra")
    sources: list[str] = []

    main_pl = ROOT / "main.pl"
    if main_pl.exists():
        sources.append("main.pl")

    for sub in ("kb", "modules", "assets"):
        sub_root = ROOT / sub
        if not sub_root.is_dir():
            continue
        for path in sub_root.rglob("*"):
            if path.is_file():
                sources.append(str(path.relative_to(ROOT).as_posix()))

    if not sources:
        return []

    grouped: dict[str, list[str]] = collections.defaultdict(list)
    for rel in sources:
        target_dir = base / pathlib.PurePosixPath(rel).parent
        grouped[str(target_dir)].append(rel)

    return list(grouped.items())


setup(
    name="zara",
    version="2.0.0",
    author="nsaspy",
    description="Hybrid Python/Prolog voice assistant",
    long_description=long_description,
    long_description_content_type="text/markdown",
    url="https://github.com/lost-rob0t/zara",
    packages=find_packages(include=["zara", "zara.*"]),
    classifiers=[
        "Programming Language :: Python :: 3",
        "Programming Language :: Prolog",
        "License :: OSI Approved :: MIT License",
        "Operating System :: POSIX :: Linux",
    ],
    python_requires=">=3.11",
    install_requires=[
        "sounddevice>=0.4.6",
        "numpy>=1.24.0",
        "faster-whisper>=0.10.0",
        "pyswip>=0.3.1",
        "edge-tts>=6.1.0",
        "elevenlabs>=0.2.0",
        "pynput>=1.7.6",
        "aiohttp>=3.9",
        "soundfile>=0.12",
        "PyYAML>=6.0",
        "pydantic>=2.0",
        "httpx>=0.25",
        "tomli>=2.0; python_version < '3.11'",
        "orgparse>=0.0.10",
        "langchain>=0.2",
        "langchain-core>=0.2",
        "langchain-community>=0.2",
        "langgraph>=0.2",
        "langchain-anthropic>=0.1",
        "langchain-openai>=0.1",
        "langchain-ollama>=0.1",
        "anthropic>=0.18.0",
        "openai>=1.0",
        "ollama>=0.1",
        "chromadb>=0.5",
        "sentence-transformers>=2.5",
        "pytest>=7.0",
        "pytest-asyncio>=0.23",
    ],
    extras_require={
        "dev": ["pytest", "pytest-asyncio"],
    },
    entry_points={
        "console_scripts": [
            "zara=zara.__main__:main",
            "zara-wake=zara.wake:main",
            "zara-console=zara.console:main",
            "zara-dictate=zara.dictate:main",
            "zara-agent=zara.agent_cli:main",
        ],
    },
    include_package_data=True,
    package_data={
        "zara": ["py.typed"],
    },
    data_files=_prolog_data_files(),
)
