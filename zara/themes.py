"""Semantic theme registry shared by Zara surfaces.

Themes are data, not UI authority. Native Desktop, first-party web surfaces and
other clients can project the same bounded semantic tokens into their own UI
toolkits without importing each other.
"""

from __future__ import annotations

from dataclasses import dataclass
from importlib import metadata as importlib_metadata
import re
from types import MappingProxyType
from typing import Any, Callable, Mapping


@dataclass(frozen=True)
class ThemeDefinition:
    key: str
    label: str
    description: str
    colors: Mapping[str, str]


SEMANTIC_COLOR_KEYS = (
    "ground",
    "panel_deep",
    "panel",
    "panel_lift",
    "line",
    "line_strong",
    "text",
    "text_muted",
    "primary",
    "primary_hover",
    "primary_deep",
    "on_primary",
    "active",
    "danger",
    "danger_deep",
)
_THEME_KEY_PATTERN = re.compile(r"^[a-z0-9][a-z0-9._-]{0,63}$")
_HEX_COLOR_PATTERN = re.compile(r"^#[0-9A-Fa-f]{6}(?:[0-9A-Fa-f]{2})?$")


def _theme(
    key: str,
    label: str,
    description: str,
    *,
    ground: str,
    panel_deep: str,
    panel: str,
    panel_lift: str,
    line: str,
    line_strong: str,
    text: str,
    text_muted: str,
    primary: str,
    primary_hover: str,
    primary_deep: str,
    on_primary: str,
    active: str,
    danger: str,
    danger_deep: str,
) -> ThemeDefinition:
    colors = {
        "ground": ground,
        "panel_deep": panel_deep,
        "panel": panel,
        "panel_lift": panel_lift,
        "line": line,
        "line_strong": line_strong,
        "text": text,
        "text_muted": text_muted,
        "primary": primary,
        "primary_hover": primary_hover,
        "primary_deep": primary_deep,
        "on_primary": on_primary,
        "active": active,
        "danger": danger,
        "danger_deep": danger_deep,
    }
    return ThemeDefinition(key, label, description, MappingProxyType(colors))


_THEME_REGISTRY: dict[str, ThemeDefinition] = {
    "signal-cabin": _theme(
        "signal-cabin",
        "Signal Cabin",
        "Charcoal enamel with warm ivory and precise route signals.",
        ground="#0A1012",
        panel_deep="#0D1518",
        panel="#111A1E",
        panel_lift="#172226",
        line="#2A393E",
        line_strong="#3C5358",
        text="#F2E9D8",
        text_muted="#A8B7B3",
        primary="#61D095",
        primary_hover="#7ADDA8",
        primary_deep="#17382B",
        on_primary="#0A1012",
        active="#E7B84B",
        danger="#E6544D",
        danger_deep="#562727",
    ),
    "dotfiles-outrun": _theme(
        "dotfiles-outrun",
        "Dotfiles Outrun",
        "The Doom Electric Outrun palette from the user's Qtile desktop.",
        ground="#170C32",
        panel_deep="#1B153A",
        panel="#202146",
        panel_lift="#2A2056",
        line="#56325F",
        line_strong="#92406E",
        text="#F3F4F5",
        text_muted="#D7B9D0",
        primary="#2DE2E6",
        primary_hover="#72F5F7",
        primary_deep="#173F51",
        on_primary="#170C32",
        active="#FBA922",
        danger="#DD546E",
        danger_deep="#4D1F3C",
    ),
    "nord": _theme(
        "nord",
        "Nord",
        "Polar-night surfaces with frost-blue controls.",
        ground="#2E3440",
        panel_deep="#282E39",
        panel="#3B4252",
        panel_lift="#434C5E",
        line="#4C566A",
        line_strong="#5E81AC",
        text="#ECEFF4",
        text_muted="#D8DEE9",
        primary="#88C0D0",
        primary_hover="#8FBCBB",
        primary_deep="#3B5368",
        on_primary="#20242C",
        active="#EBCB8B",
        danger="#BF616A",
        danger_deep="#4C3038",
    ),
    "dracula": _theme(
        "dracula",
        "Dracula",
        "Ink-dark violet surfaces with bright terminal accents.",
        ground="#282A36",
        panel_deep="#21222C",
        panel="#343746",
        panel_lift="#44475A",
        line="#525568",
        line_strong="#6272A4",
        text="#F8F8F2",
        text_muted="#C8C8D0",
        primary="#50FA7B",
        primary_hover="#69FF94",
        primary_deep="#24452E",
        on_primary="#20222B",
        active="#F1FA8C",
        danger="#FF5555",
        danger_deep="#5A2A34",
    ),
    "chatgpt-neutral": _theme(
        "chatgpt-neutral",
        "ChatGPT Neutral",
        "A calm neutral workspace inspired by modern conversational tools.",
        ground="#FFFFFF",
        panel_deep="#F7F7F8",
        panel="#ECECF1",
        panel_lift="#FFFFFF",
        line="#D9D9E3",
        line_strong="#B4B4C0",
        text="#202123",
        text_muted="#5F6368",
        primary="#10A37F",
        primary_hover="#0E8F70",
        primary_deep="#D1F4EA",
        on_primary="#0D0D0D",
        active="#9A6700",
        danger="#C92A2A",
        danger_deep="#FDE8E8",
    ),
}
THEME_REGISTRY: Mapping[str, ThemeDefinition] = MappingProxyType(_THEME_REGISTRY)
_BUILTIN_THEME_KEYS = frozenset(_THEME_REGISTRY)
_LOADED_THEME_PACKAGES: set[str] = set()
_CONFIG_THEME_KEYS: set[str] = set()
_ACTIVE_THEME_KEY = "signal-cabin"


def _validate_theme_key(key: str) -> str:
    if not isinstance(key, str) or _THEME_KEY_PATTERN.fullmatch(key) is None:
        raise ValueError(
            "theme key must be 1 to 64 lowercase letters, digits, '.', '_' or '-'"
        )
    return key


def _validate_color(name: str, value: object) -> str:
    if not isinstance(value, str) or _HEX_COLOR_PATTERN.fullmatch(value) is None:
        raise ValueError(f"theme color {name!r} must be #RRGGBB or #RRGGBBAA")
    return value.upper()


def resolve_theme(theme_key: str | None) -> ThemeDefinition:
    """Return a known theme, falling back to Signal Cabin."""
    return THEME_REGISTRY.get(theme_key or "", THEME_REGISTRY["signal-cabin"])


def active_theme_key() -> str:
    return _ACTIVE_THEME_KEY


def set_active_theme(theme_key: str | None) -> ThemeDefinition:
    """Select the process-wide semantic theme after configuration is loaded."""
    global _ACTIVE_THEME_KEY
    theme = resolve_theme(theme_key)
    _ACTIVE_THEME_KEY = theme.key
    return theme


def register_theme(
    definition: ThemeDefinition,
    *,
    replace: bool = False,
) -> ThemeDefinition:
    """Register one complete semantic theme."""
    if not isinstance(definition, ThemeDefinition):
        raise TypeError("definition must be a ThemeDefinition")
    key = _validate_theme_key(definition.key)
    colors = dict(definition.colors)
    missing = [name for name in SEMANTIC_COLOR_KEYS if name not in colors]
    unknown = sorted(set(colors) - set(SEMANTIC_COLOR_KEYS))
    if missing:
        raise ValueError("theme is missing semantic colors: " + ", ".join(missing))
    if unknown:
        raise ValueError("theme has unknown semantic colors: " + ", ".join(unknown))
    normalized = ThemeDefinition(
        key=key,
        label=str(definition.label or key),
        description=str(definition.description or ""),
        colors=MappingProxyType(
            {name: _validate_color(name, colors[name]) for name in SEMANTIC_COLOR_KEYS}
        ),
    )
    existing = _THEME_REGISTRY.get(key)
    if existing is not None and not replace:
        if existing == normalized:
            return existing
        raise ValueError(f"theme {key!r} is already registered")
    _THEME_REGISTRY[key] = normalized
    return normalized


def unregister_theme(key: str) -> bool:
    """Remove one non-built-in theme and its ownership markers."""
    _validate_theme_key(key)
    if key in _BUILTIN_THEME_KEYS:
        raise ValueError("built-in themes cannot be unregistered")
    removed = _THEME_REGISTRY.pop(key, None) is not None
    _LOADED_THEME_PACKAGES.discard(key)
    _CONFIG_THEME_KEYS.discard(key)
    global _ACTIVE_THEME_KEY
    if _ACTIVE_THEME_KEY == key:
        _ACTIVE_THEME_KEY = "signal-cabin"
    return removed


def theme_from_mapping(key: str, spec: Mapping[str, Any]) -> ThemeDefinition:
    """Build a complete theme by inheriting a registered semantic theme."""
    _validate_theme_key(key)
    if not isinstance(spec, Mapping):
        raise TypeError("theme configuration must be a mapping")

    metadata_keys = {"base", "label", "description", "colors"}
    unknown_top_level = sorted(set(spec) - metadata_keys - set(SEMANTIC_COLOR_KEYS))
    if unknown_top_level:
        raise ValueError(
            "unknown semantic color or theme field: " + ", ".join(unknown_top_level)
        )

    base_key = spec.get("base", "signal-cabin")
    if not isinstance(base_key, str) or base_key not in THEME_REGISTRY:
        raise ValueError(f"unknown base theme {base_key!r}")
    base = THEME_REGISTRY[base_key]
    colors = dict(base.colors)

    nested_colors = spec.get("colors", {})
    if not isinstance(nested_colors, Mapping):
        raise ValueError("theme colors must be a TOML table or mapping")
    unknown_nested = sorted(set(nested_colors) - set(SEMANTIC_COLOR_KEYS))
    if unknown_nested:
        raise ValueError("unknown semantic color: " + ", ".join(unknown_nested))
    for name, value in nested_colors.items():
        colors[name] = _validate_color(name, value)
    for name in SEMANTIC_COLOR_KEYS:
        if name in spec:
            colors[name] = _validate_color(name, spec[name])

    return ThemeDefinition(
        key=key,
        label=str(spec.get("label", key)),
        description=str(spec.get("description", base.description)),
        colors=MappingProxyType(colors),
    )


def load_theme_packages(
    entry_points_provider: Callable[[], object] | None = None,
) -> None:
    """Load themes declared by installed packages through the zara.themes group."""
    provider = entry_points_provider or importlib_metadata.entry_points
    discovered = provider()
    if hasattr(discovered, "select"):
        candidates = list(discovered.select(group="zara.themes"))
    elif isinstance(discovered, Mapping):
        candidates = list(discovered.get("zara.themes", ()))
    else:
        candidates = list(discovered)

    for entry_point in candidates:
        name = _validate_theme_key(str(entry_point.name))
        if name in _LOADED_THEME_PACKAGES:
            continue
        candidate = entry_point.load()
        if callable(candidate):
            candidate = candidate()
        if isinstance(candidate, ThemeDefinition):
            if candidate.key != name:
                raise ValueError(
                    f"theme package entry {name!r} returned key {candidate.key!r}"
                )
            definition = candidate
        elif isinstance(candidate, Mapping):
            definition = theme_from_mapping(name, candidate)
        else:
            raise TypeError(
                f"theme package {name!r} must expose ThemeDefinition, mapping, or factory"
            )
        register_theme(definition)
        _LOADED_THEME_PACKAGES.add(name)


def _build_config_themes(config: Mapping[str, Any]) -> dict[str, ThemeDefinition]:
    definitions: dict[str, ThemeDefinition] = {}
    for key, spec in config.items():
        if not isinstance(key, str) or not isinstance(spec, Mapping):
            raise ValueError("each [themes.<name>] entry must be a TOML table")
        if key in _BUILTIN_THEME_KEYS or key in _LOADED_THEME_PACKAGES:
            raise ValueError(f"configured theme {key!r} conflicts with a non-config theme")
        definitions[key] = theme_from_mapping(key, spec)
    return definitions


def configure_theme_registry(
    config: Mapping[str, Any] | None = None,
    *,
    load_packages: bool = True,
    active_theme: str | None = None,
) -> ThemeDefinition:
    """Load package/config themes and select the effective semantic theme."""
    if load_packages:
        load_theme_packages()
    if config is not None:
        if not isinstance(config, Mapping):
            raise ValueError("[themes] must be a TOML table")
        definitions = _build_config_themes(config)
        incoming_keys = set(definitions)
        conflicting_keys = sorted(
            key
            for key in incoming_keys
            if key in _THEME_REGISTRY and key not in _CONFIG_THEME_KEYS
        )
        if conflicting_keys:
            raise ValueError(f"theme {conflicting_keys[0]!r} is already registered")
        removed_keys = _CONFIG_THEME_KEYS - incoming_keys
        for key in removed_keys:
            _THEME_REGISTRY.pop(key, None)
        for key, definition in definitions.items():
            register_theme(definition, replace=key in _CONFIG_THEME_KEYS)
        _CONFIG_THEME_KEYS.clear()
        _CONFIG_THEME_KEYS.update(incoming_keys)
        global _ACTIVE_THEME_KEY
        if _ACTIVE_THEME_KEY in removed_keys:
            _ACTIVE_THEME_KEY = "signal-cabin"
    return set_active_theme(active_theme if active_theme is not None else _ACTIVE_THEME_KEY)


def theme_css_variables(theme_key: str | None = None) -> dict[str, str]:
    """Project semantic colors into stable CSS custom properties."""
    theme = resolve_theme(theme_key or _ACTIVE_THEME_KEY)
    return {
        f"--zara-{name.replace('_', '-')}": value
        for name, value in theme.colors.items()
    }


__all__ = [
    "SEMANTIC_COLOR_KEYS",
    "THEME_REGISTRY",
    "ThemeDefinition",
    "active_theme_key",
    "configure_theme_registry",
    "load_theme_packages",
    "register_theme",
    "resolve_theme",
    "set_active_theme",
    "theme_css_variables",
    "theme_from_mapping",
    "unregister_theme",
]
