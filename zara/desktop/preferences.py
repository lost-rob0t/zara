"""Validated, comment-preserving writes to Zara's canonical TOML config."""

from __future__ import annotations

import json
import math
import os
import re
import tempfile
from collections.abc import Callable, Mapping
from pathlib import Path
from typing import Any

from zara.config import ConfigError, ZaraConfig
from zara.desktop.theme import THEME_REGISTRY


class SettingsValidationError(ValueError):
    """A settings edit could not safely become canonical configuration."""


def _positive_integer(value: Any) -> bool:
    return isinstance(value, int) and not isinstance(value, bool) and value > 0


def _non_negative_integer(value: Any) -> bool:
    return isinstance(value, int) and not isinstance(value, bool) and value >= 0


def _positive_number(value: Any) -> bool:
    return (
        isinstance(value, (int, float))
        and not isinstance(value, bool)
        and math.isfinite(float(value))
        and float(value) > 0
    )


def _number_between(low: float, high: float) -> Callable[[Any], bool]:
    return lambda value: (
        isinstance(value, (int, float))
        and not isinstance(value, bool)
        and math.isfinite(float(value))
        and low <= float(value) <= high
    )


def _choice(*values: str) -> Callable[[Any], bool]:
    allowed = set(values)
    return lambda value: isinstance(value, str) and value in allowed


def _string(value: Any) -> bool:
    return isinstance(value, str)


def _boolean(value: Any) -> bool:
    return isinstance(value, bool)


SETTING_VALIDATORS: Mapping[str, Callable[[Any], bool]] = {
    "desktop.theme": _choice(*THEME_REGISTRY),
    "llm.provider": _choice("ollama", "openai", "anthropic", "openrouter"),
    "llm.model": _string,
    "llm.endpoint": _string,
    "llm.connect_timeout": _positive_number,
    "llm.read_timeout": _positive_number,
    "llm.total_timeout": _positive_number,
    "llm.max_retries": _non_negative_integer,
    "llm.history_limit": _positive_integer,
    "agent.conversation_timeout": _positive_number,
    "agent.post_tts_silence_seconds": _positive_number,
    "agent.max_steps": _positive_integer,
    "agent.system_prompt": _string,
    "wake.threshold": _number_between(0.0, 1.0),
    "wake.silence_duration": _positive_number,
    "wake.first_speech_timeout": _positive_number,
    "wake.acknowledgement.enabled": _boolean,
    "wake.acknowledgement.voice": _string,
    "wake.acknowledgement.volume": _number_between(0.0, 1.0),
    "stt.provider": _string,
    "stt.model": _string,
    "stt.device": _string,
    "tts.provider": _choice("local", "11labs", "edge", "qwen3"),
    "tools.calculator": _boolean,
    "tools.get_current_time": _boolean,
    "tools.query_prolog": _boolean,
    "tools.remember": _boolean,
    "tools.recall": _boolean,
    "tools.file_tools": _boolean,
    "memory.enabled": _boolean,
    "latency.enabled": _boolean,
    "database.path": _string,
    "prolog.main_file": _string,
    "prolog.load_on_startup": _boolean,
    "plugins.lifecycle_timeout": _positive_number,
    "plugins.event_queue_size": _positive_integer,
    "plugins.max_managed_workers": _positive_integer,
}


def _toml_literal(value: Any) -> str:
    if isinstance(value, bool):
        return "true" if value else "false"
    if isinstance(value, str):
        return json.dumps(value, ensure_ascii=False)
    if isinstance(value, int) and not isinstance(value, bool):
        return str(value)
    if isinstance(value, float) and math.isfinite(value):
        return repr(value)
    raise SettingsValidationError(f"unsupported setting value {value!r}")


def _replace_value(text: str, dotted_key: str, value: Any) -> str:
    section, key = dotted_key.rsplit(".", 1)
    lines = text.splitlines(keepends=True)
    header_re = re.compile(rf"^\s*\[{re.escape(section)}\]\s*(?:#.*)?$")
    any_header_re = re.compile(r"^\s*\[[^]]+\]\s*(?:#.*)?$")
    key_re = re.compile(rf"^(\s*{re.escape(key)}\s*=\s*)(.*?)(\s+#.*)?$")
    section_start = None
    section_end = len(lines)

    for index, line in enumerate(lines):
        stripped = line.rstrip("\r\n")
        if section_start is None and header_re.match(stripped):
            section_start = index + 1
            continue
        if section_start is not None and any_header_re.match(stripped):
            section_end = index
            break

    rendered = _toml_literal(value)
    if section_start is None:
        separator = "" if not text or text.endswith("\n\n") else "\n"
        return f"{text}{separator}[{section}]\n{key} = {rendered}\n"

    for index in range(section_start, section_end):
        ending = "\n" if lines[index].endswith("\n") else ""
        match = key_re.match(lines[index].rstrip("\r\n"))
        if match:
            comment = match.group(3) or ""
            lines[index] = f"{match.group(1)}{rendered}{comment}{ending}"
            return "".join(lines)

    lines.insert(section_end, f"{key} = {rendered}\n")
    return "".join(lines)


class SettingsDocument:
    """Persist supported settings without replacing the user's TOML document."""

    def __init__(self, config: ZaraConfig) -> None:
        self.config = config

    def update(self, values: Mapping[str, Any]) -> None:
        for dotted_key, value in values.items():
            validator = SETTING_VALIDATORS.get(dotted_key)
            if validator is None:
                raise SettingsValidationError(f"unsupported setting {dotted_key}")
            if not validator(value):
                raise SettingsValidationError(f"invalid value for {dotted_key}")

        path = self.config.config_file
        text = path.read_text(encoding="utf-8")
        for dotted_key, value in values.items():
            text = _replace_value(text, dotted_key, value)

        self.replace_source(text)

    def replace_source(self, text: str) -> None:
        """Validate and atomically replace the complete canonical TOML source."""
        if not isinstance(text, str):
            raise SettingsValidationError("config.toml must be UTF-8 text")

        path = self.config.config_file
        path.parent.mkdir(parents=True, exist_ok=True)
        temporary_path: Path | None = None
        try:
            with tempfile.NamedTemporaryFile(
                "w",
                encoding="utf-8",
                dir=path.parent,
                prefix=f".{path.name}.",
                suffix=".tmp",
                delete=False,
            ) as temporary:
                temporary.write(text)
                temporary.flush()
                os.fsync(temporary.fileno())
                temporary_path = Path(temporary.name)
            try:
                ZaraConfig(str(temporary_path))
            except ConfigError as error:
                raise SettingsValidationError(str(error)) from error
            if path.exists():
                os.chmod(temporary_path, path.stat().st_mode)
            os.replace(temporary_path, path)
            temporary_path = None
            self.config.reload()
        finally:
            if temporary_path is not None:
                temporary_path.unlink(missing_ok=True)


__all__ = [
    "SETTING_VALIDATORS",
    "SettingsDocument",
    "SettingsValidationError",
]
