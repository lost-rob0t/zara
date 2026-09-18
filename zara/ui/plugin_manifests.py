from __future__ import annotations

import json
from pathlib import Path
from typing import Callable, Iterable

from .extensions import (
    UiContribution,
    UiContributionKind,
    UiExtensionRegistry,
    UiPlatform,
    UiSlot,
)


UI_MANIFEST_API_VERSION = "1"
MAX_UI_MANIFEST_BYTES = 256 * 1024


class UiManifestLoadError(RuntimeError):
    pass


def _iter_manifests(paths: Iterable[Path | str]):
    seen: set[Path] = set()
    for raw_path in paths:
        root = Path(raw_path).expanduser()
        if not root.is_dir():
            continue
        resolved_root = root.resolve()
        candidates = list(root.glob("*.ui.json")) + list(root.glob("*/ui.json"))
        for candidate in sorted(candidates):
            resolved = candidate.resolve()
            try:
                resolved.relative_to(resolved_root)
            except ValueError as error:
                raise ValueError("UI manifest path escapes configured plugin root") from error
            if resolved in seen:
                continue
            seen.add(resolved)
            yield resolved


def _manifest_plugin_identity(path: Path) -> str:
    if path.name == "ui.json":
        identity = path.parent.name
    elif path.name.endswith(".ui.json"):
        identity = path.name[: -len(".ui.json")]
    else:
        raise ValueError("unsupported UI manifest filename")
    if not identity:
        raise ValueError("UI manifest path must identify a plugin")
    return identity


class UiManifestLoader:
    """Project declarative plugin UI manifests without importing plugin code."""

    def __init__(
        self,
        paths: Iterable[Path | str],
        *,
        registry: UiExtensionRegistry,
        enabled_provider: Callable[[str], bool] | None = None,
    ) -> None:
        self.paths = tuple(paths)
        self.registry = registry
        self.enabled_provider = enabled_provider or (lambda _name: True)
        self._owners: set[str] = set()

    def load(self) -> int:
        return self.reload()

    def reload(self) -> int:
        staged: dict[str, tuple[UiContribution, ...]] = {}
        try:
            for path in _iter_manifests(self.paths):
                plugin_name, contributions = self._read(path)
                if not self.enabled_provider(plugin_name):
                    continue
                owner = f"plugin:{plugin_name}"
                if owner in staged:
                    raise ValueError(f"duplicate UI manifest owner {owner!r}")
                check = UiExtensionRegistry()
                check.replace_owner(owner, contributions)
                staged[owner] = check.snapshot()
        except Exception as error:
            raise UiManifestLoadError(
                f"failed to load plugin UI manifests ({type(error).__name__})"
            ) from error

        for owner in self._owners - staged.keys():
            self.registry.clear_owner(owner)
        for owner, contributions in staged.items():
            self.registry.replace_owner(owner, contributions)
        self._owners = set(staged)
        return sum(len(items) for items in staged.values())

    @staticmethod
    def _read(path: Path) -> tuple[str, tuple[UiContribution, ...]]:
        if path.stat().st_size > MAX_UI_MANIFEST_BYTES:
            raise ValueError(f"UI manifest is too large: {path}")
        document = json.loads(path.read_text(encoding="utf-8"))
        if not isinstance(document, dict):
            raise ValueError("UI manifest root must be an object")
        if document.get("api_version") != UI_MANIFEST_API_VERSION:
            raise ValueError("unsupported UI manifest api_version")
        plugin_name = document.get("plugin")
        if not isinstance(plugin_name, str) or not plugin_name:
            raise ValueError("UI manifest plugin must be a non-empty string")
        if plugin_name != _manifest_plugin_identity(path):
            raise ValueError("UI manifest plugin identity does not match its path")
        raw_items = document.get("contributions", [])
        if not isinstance(raw_items, list):
            raise ValueError("UI manifest contributions must be a list")
        return plugin_name, tuple(UiManifestLoader._decode_item(item) for item in raw_items)

    @staticmethod
    def _decode_item(item) -> UiContribution:
        if not isinstance(item, dict):
            raise ValueError("UI manifest contribution must be an object")
        platforms = item.get("platforms", ["desktop", "android"])
        if not isinstance(platforms, list) or any(not isinstance(value, str) for value in platforms):
            raise ValueError("UI manifest platforms must be a string list")
        return UiContribution(
            id=item.get("id", ""),
            slot=UiSlot(item.get("slot", "")),
            kind=UiContributionKind(item.get("kind", "")),
            label=item.get("label", ""),
            action=item.get("action", ""),
            priority=item.get("priority", 100),
            platforms=tuple(UiPlatform(value) for value in platforms),
        )


__all__ = [
    "MAX_UI_MANIFEST_BYTES",
    "UI_MANIFEST_API_VERSION",
    "UiManifestLoadError",
    "UiManifestLoader",
]
