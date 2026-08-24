"""Safe source editing and guided facts for Zara's real Prolog config."""

from __future__ import annotations

import json
import os
import re
import subprocess
import tempfile
import uuid
from collections.abc import Callable, Mapping
from dataclasses import dataclass
from pathlib import Path
from typing import Any

from PySide6.QtCore import QRegularExpression, Qt
from PySide6.QtGui import QColor, QSyntaxHighlighter, QTextCharFormat, QTextDocument, QTextFormat

from zara.desktop.theme import resolve_theme


FACT_TYPES = (
    "app_mapping",
    "direct_app",
    "search_engine",
    "dictation_command",
    "timer_sound",
    "alarm_sound",
    "llm_provider",
    "llm_model",
    "llm_endpoint",
    "todo_destination",
    "todo_context_mode",
    "verb_intent",
)

_BEGIN = "% BEGIN ZARA MANAGED FACTS"
_END = "% END ZARA MANAGED FACTS"
_METADATA = "% zara-fact: "
_ATOM_RE = re.compile(r"^[a-z][a-z0-9_]*$")
_SHELLS = {"sh", "bash", "dash", "zsh", "ksh", "fish"}


class PrologStudioError(ValueError):
    """A source or fact edit violated Zara's Prolog boundary."""


@dataclass(frozen=True)
class ManagedFact:
    id: str
    kind: str
    fields: dict[str, Any]


@dataclass(frozen=True)
class PrologSource:
    id: str
    label: str
    path: Path
    writable: bool


def _atom(value: Any, label: str) -> str:
    text = str(value).strip().lower().replace(" ", "_")
    if not _ATOM_RE.fullmatch(text):
        raise PrologStudioError(f"{label} must use lowercase letters, numbers, and underscores")
    return text


def _string(value: Any, label: str) -> str:
    if not isinstance(value, str) or not value.strip() or "\x00" in value:
        raise PrologStudioError(f"{label} must be a non-empty string")
    return value.strip()


def _prolog_string(value: str) -> str:
    escaped = value.replace("\\", "\\\\").replace('"', '\\"')
    escaped = escaped.replace("\n", "\\n").replace("\r", "\\r")
    return f'"{escaped}"'


def _argv(value: Any, label: str = "command") -> list[str]:
    if not isinstance(value, list) or not value:
        raise PrologStudioError(f"{label} must contain at least one argument")
    if any(not isinstance(item, str) or not item or "\x00" in item for item in value):
        raise PrologStudioError(f"{label} arguments must be non-empty text")
    executable = Path(value[0]).name
    if value[0].startswith("-") or executable in _SHELLS:
        raise PrologStudioError(f"{label} executable is not allowed")
    return list(value)


def _normalized_fields(kind: str, fields: Mapping[str, Any]) -> dict[str, Any]:
    if kind not in FACT_TYPES:
        raise PrologStudioError(f"unsupported fact type {kind!r}")
    if kind == "app_mapping":
        return {"name": _atom(fields.get("name"), "app name"), "argv": _argv(fields.get("argv"))}
    if kind == "direct_app":
        return {"name": _atom(fields.get("name"), "app name")}
    if kind == "search_engine":
        return {"template": _string(fields.get("template"), "search template")}
    if kind == "dictation_command":
        return {"argv": _argv(fields.get("argv"), "dictation command")}
    if kind in {"timer_sound", "alarm_sound"}:
        return {"value": _string(fields.get("value"), "sound setting")}
    if kind == "llm_provider":
        value = _atom(fields.get("value"), "LLM provider")
        if value not in {"ollama", "openai", "anthropic"}:
            raise PrologStudioError("LLM provider must be ollama, openai, or anthropic")
        return {"value": value}
    if kind in {"llm_model", "llm_endpoint", "todo_destination"}:
        return {"value": _string(fields.get("value"), kind.replace("_", " "))}
    if kind == "todo_context_mode":
        value = _atom(fields.get("value"), "TODO context mode")
        if value not in {"infer", "infer_with_llm", "llm_only"}:
            raise PrologStudioError("TODO context mode is not supported")
        return {"value": value}
    phrase = _atom(fields.get("phrase"), "intent phrase")
    intent = _atom(fields.get("intent"), "intent action")
    arity = fields.get("arity")
    if arity != "rest" and (
        not isinstance(arity, int) or isinstance(arity, bool) or arity < 0
    ):
        raise PrologStudioError("intent arity must be rest or a non-negative integer")
    return {"phrase": phrase, "intent": intent, "arity": arity}


def _render_fact(fact: ManagedFact) -> str:
    fields = _normalized_fields(fact.kind, fact.fields)
    kind = fact.kind
    if kind == "app_mapping":
        argv = ", ".join(_prolog_string(item) for item in fields["argv"])
        return f"app_mapping({fields['name']}, [{argv}])."
    if kind == "direct_app":
        return f"direct_app({fields['name']})."
    if kind == "search_engine":
        return f"search_engine({_prolog_string(fields['template'])})."
    if kind == "dictation_command":
        argv = ", ".join(_prolog_string(item) for item in fields["argv"])
        return f"dictation_command([{argv}])."
    if kind in {"timer_sound", "alarm_sound"}:
        value = fields["value"]
        rendered = "disabled" if value == "disabled" else _prolog_string(value)
        return f"{kind}({rendered})."
    if kind in {"llm_provider", "todo_context_mode"}:
        return f"{kind}({fields['value']})."
    if kind in {"llm_model", "llm_endpoint", "todo_destination"}:
        return f"{kind}({_prolog_string(fields['value'])})."
    return f"verb_intent({fields['phrase']}, {fields['intent']}, {fields['arity']})."


def _atomic_write(path: Path, text: str, validator: Callable[[Path], None] | None = None) -> None:
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
        if validator is not None:
            validator(temporary_path)
        if path.exists():
            os.chmod(temporary_path, path.stat().st_mode)
        os.replace(temporary_path, path)
        temporary_path = None
    finally:
        if temporary_path is not None:
            temporary_path.unlink(missing_ok=True)


class ManagedFactStore:
    """Own only Zara's marked fact block inside the actual user config."""

    def __init__(self, path: Path) -> None:
        self.path = Path(path)

    def list(self) -> list[ManagedFact]:
        if not self.path.exists():
            return []
        text = self.path.read_text(encoding="utf-8")
        managed = self._managed_text(text)
        if managed is None:
            return []
        facts = []
        for line in managed.splitlines():
            if not line.startswith(_METADATA):
                continue
            try:
                payload = json.loads(line[len(_METADATA) :])
                fact = ManagedFact(
                    id=str(payload["id"]),
                    kind=str(payload["kind"]),
                    fields=dict(payload["fields"]),
                )
                _normalized_fields(fact.kind, fact.fields)
            except (KeyError, TypeError, ValueError, json.JSONDecodeError) as error:
                raise PrologStudioError("managed fact metadata is invalid") from error
            facts.append(fact)
        return facts

    def add(self, kind: str, fields: Mapping[str, Any]) -> ManagedFact:
        fact = ManagedFact(uuid.uuid4().hex, kind, _normalized_fields(kind, fields))
        facts = self.list()
        facts.append(fact)
        self._write(facts)
        return fact

    def update(self, fact_id: str, kind: str, fields: Mapping[str, Any]) -> ManagedFact:
        replacement = ManagedFact(fact_id, kind, _normalized_fields(kind, fields))
        facts = self.list()
        for index, fact in enumerate(facts):
            if fact.id == fact_id:
                facts[index] = replacement
                self._write(facts)
                return replacement
        raise PrologStudioError("managed fact was not found")

    def delete(self, fact_id: str) -> None:
        facts = self.list()
        retained = [fact for fact in facts if fact.id != fact_id]
        if len(retained) == len(facts):
            raise PrologStudioError("managed fact was not found")
        self._write(retained)

    @staticmethod
    def _managed_text(text: str) -> str | None:
        start = text.find(_BEGIN)
        end = text.find(_END)
        if start < 0 and end < 0:
            return None
        if start < 0 or end < start:
            raise PrologStudioError("managed fact block markers are incomplete")
        return text[start + len(_BEGIN) : end]

    def _write(self, facts: list[ManagedFact]) -> None:
        original = self.path.read_text(encoding="utf-8") if self.path.exists() else ""
        start = original.find(_BEGIN)
        end = original.find(_END)
        if (start < 0) != (end < 0) or 0 <= end < start:
            raise PrologStudioError("managed fact block markers are incomplete")

        rendered_lines = [_BEGIN]
        for fact in facts:
            payload = json.dumps(
                {"id": fact.id, "kind": fact.kind, "fields": fact.fields},
                ensure_ascii=False,
                separators=(",", ":"),
            )
            rendered_lines.extend([f"{_METADATA}{payload}", _render_fact(fact)])
        rendered_lines.append(_END)
        block = "\n".join(rendered_lines)

        if start < 0:
            separator = "" if not original or original.endswith("\n\n") else "\n"
            text = f"{original}{separator}{block}\n"
        else:
            suffix_start = end + len(_END)
            text = f"{original[:start]}{block}{original[suffix_start:]}"
        _atomic_write(self.path, text)


class PrologSourceRepository:
    """Expose only approved Zara Prolog sources by stable identifiers."""

    def __init__(self, root: Path, user_config: Path, *, max_bytes: int = 512_000) -> None:
        self.root = Path(root).resolve()
        self.user_config = Path(user_config).resolve()
        self.max_bytes = max_bytes

    def list(self) -> list[PrologSource]:
        entries = [self._entry("user-config", "User config.pl", self.user_config)]
        candidates = [self.root / "main.pl"]
        candidates.extend(sorted((self.root / "kb").glob("*.pl")))
        candidates.extend(sorted((self.root / "modules").glob("*.pl")))
        for path in candidates:
            if not path.is_file() or path.is_symlink():
                continue
            resolved = path.resolve()
            if not resolved.is_relative_to(self.root):
                continue
            relative = resolved.relative_to(self.root).as_posix()
            entries.append(self._entry(relative, relative, resolved))
        return entries

    def read(self, source_id: str) -> str:
        source = self._source(source_id)
        try:
            size = source.path.stat().st_size
        except OSError as error:
            raise PrologStudioError(f"unable to inspect {source.label}: {error}") from error
        if size > self.max_bytes:
            raise PrologStudioError(f"source exceeds the {self.max_bytes}-byte maximum")
        try:
            return source.path.read_text(encoding="utf-8")
        except UnicodeDecodeError as error:
            raise PrologStudioError("source is not valid UTF-8") from error
        except OSError as error:
            raise PrologStudioError(f"unable to read {source.label}: {error}") from error

    def write(
        self,
        source_id: str,
        text: str,
        *,
        validator: Callable[[Path], None] | None = None,
    ) -> None:
        source = self._source(source_id)
        if not source.writable:
            raise PrologStudioError(f"{source.label} is read-only")
        if not isinstance(text, str):
            raise PrologStudioError("source must be UTF-8 text")
        if len(text.encode("utf-8")) > self.max_bytes:
            raise PrologStudioError(f"source exceeds the {self.max_bytes}-byte maximum")
        _atomic_write(source.path, text, validator or validate_prolog_file)

    @staticmethod
    def _entry(source_id: str, label: str, path: Path) -> PrologSource:
        writable = path.exists() and os.access(path, os.W_OK)
        return PrologSource(source_id, label, path, writable)

    def _source(self, source_id: str) -> PrologSource:
        for source in self.list():
            if source.id == source_id:
                return source
        raise PrologStudioError("source is not an approved Prolog file")


def validate_prolog_file(path: Path) -> None:
    """Parse every term in a fresh SWI-Prolog process without consulting it."""
    goal = (
        "current_prolog_flag(argv,[Path]),open(Path,read,Stream),repeat,"
        "catch(read_term(Stream,Term,[]),Error,(print_message(error,Error),halt(2))),"
        "(Term==end_of_file->close(Stream),halt(0);fail)"
    )
    result = subprocess.run(
        ["swipl", "-q", "-f", "none", "-g", goal, "--", str(path)],
        capture_output=True,
        text=True,
        timeout=10,
        check=False,
    )
    if result.returncode != 0:
        detail = (result.stderr or result.stdout).strip()
        raise PrologStudioError(detail or "Prolog syntax validation failed")


class PrologHighlighter(QSyntaxHighlighter):
    """Readable semantic highlighting for Prolog source and user facts."""

    CATEGORY_PROPERTY = int(QTextFormat.Property.UserProperty) + 1

    def __init__(self, document: QTextDocument, theme_key: str = "signal-cabin") -> None:
        super().__init__(document)
        self._patterns = [
            ("string", QRegularExpression(r'"(?:\\.|[^"\\])*"'), False),
            ("variable", QRegularExpression(r"\b(?:[A-Z_][A-Za-z0-9_]*)\b"), False),
            ("number", QRegularExpression(r"\b\d+(?:\.\d+)?\b"), False),
            ("predicate", QRegularExpression(r"\b[a-z][A-Za-z0-9_]*(?=\s*\()"), True),
            ("directive", QRegularExpression(r"^\s*:-.*$"), True),
            ("comment", QRegularExpression(r"%[^\n]*"), False),
        ]
        self.set_theme(theme_key)

    def set_theme(self, theme_key: str) -> None:
        colors = resolve_theme(theme_key).colors
        light = colors["ground"].upper() == "#FFFFFF"
        if light:
            self._colors = {
                "string": "#8A5A00",
                "variable": "#0969DA",
                "number": "#8250DF",
                "predicate": "#116329",
                "directive": "#C2410C",
                "comment": colors["text_muted"],
            }
        else:
            self._colors = {
                "string": colors["active"],
                "variable": colors["primary"],
                "number": colors["primary_hover"],
                "predicate": colors["text"],
                "directive": colors["danger"],
                "comment": colors["text_muted"],
            }
        self.rehighlight()

    def highlightBlock(self, text: str) -> None:  # noqa: N802 - Qt API
        for category, pattern, bold in self._patterns:
            match = pattern.globalMatch(text)
            while match.hasNext():
                result = match.next()
                text_format = QTextCharFormat()
                text_format.setForeground(QColor(self._colors[category]))
                if bold:
                    text_format.setFontWeight(700)
                text_format.setProperty(self.CATEGORY_PROPERTY, category)
                self.setFormat(result.capturedStart(), result.capturedLength(), text_format)


__all__ = [
    "FACT_TYPES",
    "ManagedFact",
    "ManagedFactStore",
    "PrologHighlighter",
    "PrologSource",
    "PrologSourceRepository",
    "PrologStudioError",
    "validate_prolog_file",
]
