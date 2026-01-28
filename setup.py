#!/usr/bin/env python3
"""
Zarathustra Voice Assistant
Setup script for pip installation
"""

from setuptools import setup, find_packages
from pathlib import Path

# Read README
readme = Path(__file__).parent / "README.org"
long_description = readme.read_text() if readme.exists() else ""

setup(
    name="zara",
    version="2.0.0",
    author="nsaspy",
    description="Hybrid Python/Prolog voice assistant",
    long_description=long_description,
    long_description_content_type="text/markdown",
    url="https://github.com/lost-rob0t/zara",
    packages=find_packages(),
    classifiers=[
        "Programming Language :: Python :: 3",
        "Programming Language :: Prolog",
        "License :: OSI Approved :: MIT License",
        "Operating System :: POSIX :: Linux",
    ],
    python_requires=">=3.9",
    install_requires=[
        "sounddevice>=0.4.6",
        "numpy>=1.24.0",
        "faster-whisper>=0.10.0",
        "anthropic>=0.18.0",
        "pyswip>=0.2.10",
        "edge-tts>=6.1.0",
        "pynput>=1.7.6",
    ],
    extras_require={
        "elevenlabs": ["elevenlabs>=0.2.0"],
        "dev": ["pytest", "black", "mypy"],
    },
    entry_points={
        "console_scripts": [
            "zara=zara.__main__:main",
            "zara-console=zara.console:main",
            "zara-dictate=zara.dictate:main",
        ],
    },
    include_package_data=True,
    package_data={
        "": ["*.pl", "kb/*.pl", "modules/*.pl"],
    },
)
