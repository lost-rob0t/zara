"""Resolved Org browser configuration and trusted Python customization.

Org files remain canonical. This module controls browser policy only. Native
clients that do not own SWI-Prolog consume the canonical runtime's disposable,
owner-local override snapshot instead of starting a second Prolog engine.
"""

from __future__ import annotations

import importlib.util
import json
import os
import uuid
from dataclasses import dataclass, replace
from pathlib import Path
from types import ModuleType
from typing import Any, Callable, Iterable, Optional, Sequence


_DEFAULT_HELP_SOURCES = (
    "README.org",
    "docs/README.org",
    "wiki/android.org",
    "wiki/customization.org",
    "wiki/agent-mode.org",
)
_DEFAULT_HEADING_SCALES = (1.45, 1.30, 1.18, 1.10, 1.04)
_ALLOWED_HOOKS = frozenset(
    {
        "filter_node",
        "sort_nodes",
        "render_document",
        "filter_help_sources",
        "memory_tags",
        "memory_text",
    }
)
_USER_OWNER = "user:org_browser.py"
_MAX_SNAPSHOT_BYTES = 256_000


class OrgBrowserConfigError(ValueError):
    pass


class OrgBrowserPythonLoadError(RuntimeError):
    pass


@dataclass(frozen=True)
class OrgBrowserConfig:
    enabled: bool = True
    roots: tuple[str, ...] = ()
    default_project: Optional[str] = None
    memory_sync: bool = True
    base_font_pt: float = 12.0
    max_files: int = 2000
    max_file_bytes: int = 2_000_000
    search_limit: int = 200
    show_backlinks: bool = True
    show_properties: bool = True
    recent_chat_limit: int = 5
    help_sources: tuple[str, ...] = _DEFAULT_HELP_SOURCES
    heading_scales: tuple[float, ...] = _DEFAULT_HEADING_SCALES
    python_config_enabled: bool = True

    @classmethod
    def from_mapping(cls, mapping: Optional[dict[str, Any]] = None) -> "OrgBrowserConfig":
        config = cls()
        values = dict(mapping or {})
        roots = values.pop("roots", None)
        help_sources = values.pop("help_sources", None)
        heading_scales = values.pop("heading_scales", None)
        for key, value in values.items():
            config = config.with_setting(key, value)
        if roots is not None:
            config = replace(config, roots=_string_tuple(roots, "roots"))
        if help_sources is not None:
            config = replace(
                config,
                help_sources=_string_tuple(help_sources, "help_sources"),
            )
        if heading_scales is not None:
            scales = tuple(_positive_float(value, "heading_scales") for value in heading_scales)
            if not scales:
                raise OrgBrowserConfigError("heading_scales must not be empty")
            config = replace(config, heading_scales=scales)
        return config

    def with_setting(self, key: str, value: Any) -> "OrgBrowserConfig":
        key = str(key).strip()
        if key == "enabled":
            return replace(self, enabled=_bool_value(value, key))
        if key == "default_project":
            return replace(self, default_project=_optional_text(value))
        if key == "memory_sync":
            return replace(self, memory_sync=_bool_value(value, key))
        if key == "base_font_pt":
            return replace(self, base_font_pt=_bounded_float(value, key, 1.0, 96.0))
        if key == "max_files":
            return replace(self, max_files=_bounded_int(value, key, 1, 100_000))
        if key == "max_file_bytes":
            return replace(self, max_file_bytes=_bounded_int(value, key, 1, 100_000_000))
        if key == "search_limit":
            return replace(self, search_limit=_bounded_int(value, key, 1, 10_000))
        if key == "show_backlinks":
            return replace(self, show_backlinks=_bool_value(value, key))
        if key == "show_properties":
            return replace(self, show_properties=_bool_value(value, key))
        if key == "recent_chat_limit":
            return replace(self, recent_chat_limit=_bounded_int(value, key, 0, 100))
        if key == "python_config_enabled":
            return replace(self, python_config_enabled=_bool_value(value, key))
        raise KeyError(f"unknown Org browser setting: {key}")

    def with_root(self, path: str) -> "OrgBrowserConfig":
        text = _required_text(path, "root")
        if text in self.roots:
            return self
        return replace(self, roots=(*self.roots, text))

    def without_roots(self) -> "OrgBrowserConfig":
        return replace(self, roots=())

    def with_help_source(self, path: str) -> "OrgBrowserConfig":
        text = _required_text(path, "help source")
        if text in self.help_sources:
            return self
        return replace(self, help_sources=(*self.help_sources, text))

    def without_help_sources(self) -> "OrgBrowserConfig":
        return replace(self, help_sources=())

    def with_heading_scale(self, level: int, scale: float) -> "OrgBrowserConfig":
        level_value = _bounded_int(level, "heading level", 1, 32)
        scale_value = _bounded_float(scale, "heading scale", 0.25, 4.0)
        rows = list(self.heading_scales)
        while len(rows) < level_value:
            rows.append(1.0)
        rows[level_value - 1] = scale_value
        return replace(self, heading_scales=tuple(rows))

    def heading_scale(self, level: int) -> float:
        level_value = max(1, int(level))
        if level_value <= len(self.heading_scales):
            return self.heading_scales[level_value - 1]
        return 1.0


@dataclass(frozen=True)
class OrgBrowserHookDiagnostic:
    registration_id: int
    stage: str
    owner: str
    priority: int
    sequence: int


@dataclass(frozen=True)
class _HookRegistration:
    registration_id: int
    stage: str
    owner: str
    priority: int
    sequence: int
    callback: Callable[..., Any]


class OrgBrowserHookRegistry:
    """Ordered synchronous transforms for native Org browser projections."""

    def __init__(self) -> None:
        self._next_registration_id = 1
        self._next_sequence = 1
        self._registrations: dict[int, _HookRegistration] = {}

    def register(
        self,
        stage: str,
        owner: str,
        priority: int,
        callback: Callable[..., Any],
    ) -> int:
        if stage not in _ALLOWED_HOOKS:
            raise OrgBrowserConfigError(f"unknown Org browser hook: {stage}")
        if not isinstance(owner, str) or not owner.strip() or len(owner) > 128:
            raise OrgBrowserConfigError("hook owner must be a non-empty bounded string")
        if isinstance(priority, bool) or not isinstance(priority, int) or abs(priority) > 100_000:
            raise OrgBrowserConfigError("hook priority must be a bounded integer")
        if not callable(callback):
            raise OrgBrowserConfigError("hook callback must be callable")
        registration_id = self._next_registration_id
        sequence = self._next_sequence
        self._next_registration_id += 1
        self._next_sequence += 1
        self._registrations[registration_id] = _HookRegistration(
            registration_id,
            stage,
            owner.strip(),
            priority,
            sequence,
            callback,
        )
        return registration_id

    def clear_owner(self, owner: str) -> int:
        ids = [
            row.registration_id
            for row in self._registrations.values()
            if row.owner == owner
        ]
        for registration_id in ids:
            self._registrations.pop(registration_id, None)
        return len(ids)

    def diagnostics(self) -> tuple[OrgBrowserHookDiagnostic, ...]:
        return tuple(
            OrgBrowserHookDiagnostic(
                row.registration_id,
                row.stage,
                row.owner,
                row.priority,
                row.sequence,
            )
            for row in self._snapshot()
        )

    def filter_nodes(self, nodes: Sequence[Any], config: OrgBrowserConfig) -> tuple[Any, ...]:
        rows = []
        callbacks = self._stage("filter_node")
        for node in nodes:
            keep = True
            for registration in callbacks:
                result = registration.callback(node, config)
                if result is False:
                    keep = False
                    break
                if result not in (None, True):
                    raise OrgBrowserConfigError("filter_node hooks must return bool or None")
            if keep:
                rows.append(node)
        return tuple(rows)

    def sort_nodes(self, nodes: Sequence[Any], config: OrgBrowserConfig) -> tuple[Any, ...]:
        value: tuple[Any, ...] = tuple(nodes)
        for registration in self._stage("sort_nodes"):
            result = registration.callback(value, config)
            if result is not None:
                value = tuple(result)
        return value

    def render_document(self, document: Any, rendered: str, config: OrgBrowserConfig) -> str:
        value = str(rendered)
        for registration in self._stage("render_document"):
            result = registration.callback(document, value, config)
            if result is not None:
                value = str(result)
        return value

    def help_sources(self, paths: Sequence[str], config: OrgBrowserConfig) -> tuple[str, ...]:
        value = tuple(str(path) for path in paths)
        for registration in self._stage("filter_help_sources"):
            result = registration.callback(value, config)
            if result is not None:
                value = tuple(str(path) for path in result)
        return tuple(dict.fromkeys(path for path in value if path.strip()))

    def memory_tags(
        self,
        node: Any,
        tags: Sequence[str],
        config: OrgBrowserConfig,
    ) -> tuple[str, ...]:
        value = tuple(str(tag) for tag in tags if str(tag).strip())
        for registration in self._stage("memory_tags"):
            result = registration.callback(node, value, config)
            if result is not None:
                value = tuple(str(tag) for tag in result if str(tag).strip())
        return tuple(dict.fromkeys(value))

    def memory_text(self, node: Any, text: str, config: OrgBrowserConfig) -> str:
        value = str(text)
        for registration in self._stage("memory_text"):
            result = registration.callback(node, value, config)
            if result is not None:
                value = str(result)
        return value

    def _snapshot(self) -> list[_HookRegistration]:
        return sorted(
            self._registrations.values(),
            key=lambda row: (row.priority, row.sequence),
        )

    def _stage(self, stage: str) -> tuple[_HookRegistration, ...]:
        return tuple(row for row in self._snapshot() if row.stage == stage)


@dataclass(frozen=True)
class OrgBrowserRuntime:
    config: OrgBrowserConfig
    hooks: OrgBrowserHookRegistry


class _OrgBrowserFacade:
    def __init__(self, config: OrgBrowserConfig, hooks: OrgBrowserHookRegistry) -> None:
        self.config = config
        self.hooks = hooks

    def set(self, key: str, value: Any) -> None:
        self.config = self.config.with_setting(key, value)

    def clear_roots(self) -> None:
        self.config = self.config.without_roots()

    def add_root(self, path: str) -> None:
        self.config = self.config.with_root(path)

    def clear_help_sources(self) -> None:
        self.config = self.config.without_help_sources()

    def help_source(self, path: str) -> None:
        self.config = self.config.with_help_source(path)

    def heading_scale(self, level: int, scale: float) -> None:
        self.config = self.config.with_heading_scale(level, scale)

    def filter_node(self, callback: Callable[..., Any], priority: int = 0) -> int:
        return self.hooks.register("filter_node", _USER_OWNER, priority, callback)

    def sort_nodes(self, callback: Callable[..., Any], priority: int = 0) -> int:
        return self.hooks.register("sort_nodes", _USER_OWNER, priority, callback)

    def render_document(self, callback: Callable[..., Any], priority: int = 0) -> int:
        return self.hooks.register("render_document", _USER_OWNER, priority, callback)

    def filter_help_sources(self, callback: Callable[..., Any], priority: int = 0) -> int:
        return self.hooks.register("filter_help_sources", _USER_OWNER, priority, callback)

    def memory_tags(self, callback: Callable[..., Any], priority: int = 0) -> int:
        return self.hooks.register("memory_tags", _USER_OWNER, priority, callback)

    def memory_text(self, callback: Callable[..., Any], priority: int = 0) -> int:
        return self.hooks.register("memory_text", _USER_OWNER, priority, callback)


class OrgBrowserPythonLoader:
    def __init__(self, *, config_dir: Path) -> None:
        self.config_dir = Path(config_dir)

    @property
    def path(self) -> Path:
        return self.config_dir / "org_browser.py"

    def apply(
        self,
        config: OrgBrowserConfig,
        hooks: OrgBrowserHookRegistry,
    ) -> OrgBrowserConfig:
        if not self.path.is_file():
            return config
        try:
            module = self._load_module()
            configure = getattr(module, "configure", None)
            if not callable(configure):
                raise TypeError("configure must be callable")
            facade = _OrgBrowserFacade(config, hooks)
            configure(facade)
            return facade.config
        except Exception as error:
            raise OrgBrowserPythonLoadError(
                f"failed to load org_browser.py ({type(error).__name__})"
            ) from error

    def _load_module(self) -> ModuleType:
        module_name = f"_zara_org_browser_{uuid.uuid4().hex}"
        spec = importlib.util.spec_from_file_location(module_name, self.path)
        if spec is None or spec.loader is None:
            raise ImportError("unable to create org_browser.py module spec")
        module = importlib.util.module_from_spec(spec)
        spec.loader.exec_module(module)
        return module


def build_org_browser_runtime(
    zara_config: Any,
    *,
    prolog_engine: Any = None,
) -> OrgBrowserRuntime:
    get_section = getattr(zara_config, "get_section", None)
    org_mapping = get_section("org") if callable(get_section) else {}
    config = OrgBrowserConfig.from_mapping(org_mapping)
    if prolog_engine is not None:
        config = _apply_prolog_overrides(config, prolog_engine)
    else:
        config = _apply_prolog_snapshot(config)

    hooks = OrgBrowserHookRegistry()
    hooks_mapping = get_section("hooks") if callable(get_section) else {}
    hooks_enabled = bool((hooks_mapping or {}).get("enabled", False))
    config_dir = getattr(zara_config, "config_dir", None)
    if hooks_enabled and config.python_config_enabled and config_dir is not None:
        config = OrgBrowserPythonLoader(config_dir=Path(config_dir)).apply(config, hooks)
    return OrgBrowserRuntime(config=config, hooks=hooks)


def _apply_prolog_overrides(config: OrgBrowserConfig, engine: Any) -> OrgBrowserConfig:
    settings = engine.query_all("kb_config:org_browser_setting(Key, Value)", max_solutions=100)
    seen: set[str] = set()
    for row in settings:
        key = str(row.get("Key", "")).strip()
        if not key or key in seen:
            continue
        config = config.with_setting(key, row.get("Value"))
        seen.add(key)

    roots = engine.query_all("kb_config:org_browser_root(Path)", max_solutions=10_000)
    roots_overridden = bool(roots) or _query_succeeds(engine, "kb_config:org_browser_roots_overridden")
    if roots_overridden:
        config = config.without_roots()
        for row in roots:
            config = config.with_root(str(row.get("Path", "")))

    scales = engine.query_all(
        "kb_config:org_browser_heading_scale(Level, Scale)",
        max_solutions=64,
    )
    seen_levels: set[int] = set()
    for row in scales:
        level = int(row.get("Level"))
        if level in seen_levels:
            continue
        config = config.with_heading_scale(level, float(row.get("Scale")))
        seen_levels.add(level)

    help_sources = engine.query_all(
        "kb_config:org_browser_help_source(Path)",
        max_solutions=1000,
    )
    help_overridden = bool(help_sources) or _query_succeeds(
        engine,
        "kb_config:org_browser_help_sources_overridden",
    )
    if help_overridden:
        config = config.without_help_sources()
        for row in help_sources:
            config = config.with_help_source(str(row.get("Path", "")))
    return config


def _query_succeeds(engine: Any, goal: str) -> bool:
    try:
        return bool(engine.query_all(goal, max_solutions=1))
    except Exception:
        return False


def _apply_prolog_snapshot(config: OrgBrowserConfig) -> OrgBrowserConfig:
    path = _prolog_snapshot_path()
    if path is None or not path.is_file():
        return config
    try:
        stat = path.stat()
        if stat.st_size <= 0 or stat.st_size > _MAX_SNAPSHOT_BYTES:
            return config
        getuid = getattr(os, "getuid", None)
        if callable(getuid) and stat.st_uid != getuid():
            return config
        payload = json.loads(path.read_text(encoding="utf-8"))
        if not isinstance(payload, dict) or payload.get("version") != 1:
            return config
        settings = payload.get("settings", {})
        if isinstance(settings, dict):
            for key, value in settings.items():
                config = config.with_setting(str(key), value)

        roots = payload.get("roots", [])
        if payload.get("roots_override") is True:
            config = config.without_roots()
            if isinstance(roots, list):
                for root in roots:
                    config = config.with_root(str(root))

        scales = payload.get("heading_scales", [])
        if isinstance(scales, list):
            for row in scales:
                if not isinstance(row, dict):
                    continue
                config = config.with_heading_scale(int(row["level"]), float(row["scale"]))

        help_sources = payload.get("help_sources", [])
        if payload.get("help_sources_override") is True:
            config = config.without_help_sources()
            if isinstance(help_sources, list):
                for source in help_sources:
                    config = config.with_help_source(str(source))
        return config
    except (OSError, ValueError, TypeError, KeyError, json.JSONDecodeError):
        return config


def _prolog_snapshot_path() -> Optional[Path]:
    explicit = os.getenv("ZARA_ORG_BROWSER_PROLOG_SNAPSHOT", "").strip()
    if explicit:
        return Path(explicit).expanduser()
    runtime_dir = os.getenv("XDG_RUNTIME_DIR", "").strip()
    if not runtime_dir:
        return None
    return Path(runtime_dir) / "zarathushtra" / "org-browser-prolog.json"


def _optional_text(value: Any) -> Optional[str]:
    if value is None:
        return None
    text = str(value).strip()
    return text or None


def _required_text(value: Any, name: str) -> str:
    text = _optional_text(value)
    if text is None:
        raise OrgBrowserConfigError(f"{name} must be a non-empty string")
    return text


def _string_tuple(value: Any, name: str) -> tuple[str, ...]:
    if isinstance(value, str):
        rows: Iterable[Any] = (value,)
    elif isinstance(value, Iterable):
        rows = value
    else:
        raise OrgBrowserConfigError(f"{name} must be a string or list of strings")
    return tuple(dict.fromkeys(_required_text(item, name) for item in rows))


def _bool_value(value: Any, name: str) -> bool:
    if isinstance(value, bool):
        return value
    if isinstance(value, (int, float)) and value in {0, 1}:
        return bool(value)
    normalized = str(value).strip().casefold()
    if normalized in {"true", "yes", "on", "1"}:
        return True
    if normalized in {"false", "no", "off", "0"}:
        return False
    raise OrgBrowserConfigError(f"{name} must be boolean")


def _bounded_int(value: Any, name: str, minimum: int, maximum: int) -> int:
    if isinstance(value, bool):
        raise OrgBrowserConfigError(f"{name} must be an integer")
    try:
        result = int(value)
    except (TypeError, ValueError) as error:
        raise OrgBrowserConfigError(f"{name} must be an integer") from error
    if result < minimum or result > maximum:
        raise OrgBrowserConfigError(f"{name} outside supported range")
    return result


def _positive_float(value: Any, name: str) -> float:
    return _bounded_float(value, name, 0.000001, 1_000_000.0)


def _bounded_float(value: Any, name: str, minimum: float, maximum: float) -> float:
    if isinstance(value, bool):
        raise OrgBrowserConfigError(f"{name} must be numeric")
    try:
        result = float(value)
    except (TypeError, ValueError) as error:
        raise OrgBrowserConfigError(f"{name} must be numeric") from error
    if result < minimum or result > maximum:
        raise OrgBrowserConfigError(f"{name} outside supported range")
    return result


__all__ = [
    "OrgBrowserConfig",
    "OrgBrowserConfigError",
    "OrgBrowserHookDiagnostic",
    "OrgBrowserHookRegistry",
    "OrgBrowserPythonLoadError",
    "OrgBrowserPythonLoader",
    "OrgBrowserRuntime",
    "build_org_browser_runtime",
]
