from __future__ import annotations

import importlib.util
import re
import uuid
from dataclasses import dataclass, replace
from enum import Enum
from pathlib import Path
from types import ModuleType
from typing import Callable, Iterable, Sequence


class UiPlatform(str, Enum):
    DESKTOP = "desktop"
    ANDROID = "android"


class UiSlot(str, Enum):
    DRAWER = "drawer"
    CHAT_TOP = "chat.top"
    CHAT_BOTTOM = "chat.bottom"
    SETTINGS = "settings"
    PLUGINS = "plugins"


class UiContributionKind(str, Enum):
    SURFACE = "surface"
    SECTION = "section"
    TEXT = "text"
    BUTTON = "button"
    TOGGLE = "toggle"
    STATUS = "status"


PORTABLE_UI_ACTION_PREFIXES = ("route:", "prompt:", "submit:", "plugin:")
_ACTION_KINDS = {
    UiContributionKind.SURFACE,
    UiContributionKind.BUTTON,
    UiContributionKind.TOGGLE,
}
_ID = re.compile(r"[a-zA-Z0-9][a-zA-Z0-9._-]{0,63}")
_OWNER = re.compile(r"[a-zA-Z0-9][a-zA-Z0-9._:-]{0,127}")
_MAX_LABEL = 160
_MAX_ACTION = 1024
_MAX_CONTRIBUTIONS = 256
_USER_INIT_OWNER = "user:init.py"


@dataclass(frozen=True)
class UiContribution:
    id: str
    slot: UiSlot
    kind: UiContributionKind
    label: str
    action: str = ""
    priority: int = 100
    platforms: tuple[UiPlatform, ...] = (UiPlatform.DESKTOP, UiPlatform.ANDROID)
    owner: str = "unbound"

    def __post_init__(self) -> None:
        if not _ID.fullmatch(self.id):
            raise ValueError("UI contribution id must be a bounded portable identifier")
        if not isinstance(self.slot, UiSlot):
            raise TypeError("UI contribution slot must be a UiSlot")
        if not isinstance(self.kind, UiContributionKind):
            raise TypeError("UI contribution kind must be a UiContributionKind")
        if not isinstance(self.label, str) or not self.label.strip() or len(self.label) > _MAX_LABEL:
            raise ValueError(f"UI contribution label must contain 1 to {_MAX_LABEL} characters")
        if isinstance(self.priority, bool) or not isinstance(self.priority, int):
            raise TypeError("UI contribution priority must be an integer")
        if not -10_000 <= self.priority <= 10_000:
            raise ValueError("UI contribution priority must be between -10000 and 10000")
        if not self.platforms:
            raise ValueError("UI contribution must target at least one platform")
        normalized_platforms = tuple(dict.fromkeys(self.platforms))
        if any(not isinstance(platform, UiPlatform) for platform in normalized_platforms):
            raise TypeError("UI contribution platforms must contain UiPlatform values")
        object.__setattr__(self, "platforms", normalized_platforms)
        if not isinstance(self.owner, str) or not _OWNER.fullmatch(self.owner):
            raise ValueError("UI contribution owner must be a bounded portable identifier")
        if not isinstance(self.action, str) or len(self.action) > _MAX_ACTION:
            raise ValueError(f"UI action must not exceed {_MAX_ACTION} characters")
        if self.action and not self.action.startswith(PORTABLE_UI_ACTION_PREFIXES):
            raise ValueError(
                "UI action must use route:, prompt:, submit:, or plugin:"
            )
        if self.kind in _ACTION_KINDS and not self.action:
            raise ValueError(f"UI contribution kind {self.kind.value!r} requires an action")


class UiExtensionRegistry:
    def __init__(self) -> None:
        self._by_owner: dict[str, tuple[UiContribution, ...]] = {}

    def replace_owner(self, owner: str, contributions: Iterable[UiContribution]) -> None:
        if not _OWNER.fullmatch(owner):
            raise ValueError("UI owner must be a bounded portable identifier")
        staged = tuple(replace(item, owner=owner) for item in contributions)
        if len(staged) > _MAX_CONTRIBUTIONS:
            raise ValueError(f"UI owner may register at most {_MAX_CONTRIBUTIONS} contributions")
        keys: set[tuple[UiSlot, str]] = set()
        for item in staged:
            key = (item.slot, item.id)
            if key in keys:
                raise ValueError(f"duplicate UI contribution {item.slot.value}:{item.id}")
            keys.add(key)
        self._by_owner[owner] = staged

    def clear_owner(self, owner: str) -> None:
        self._by_owner.pop(owner, None)

    def snapshot(self) -> tuple[UiContribution, ...]:
        values = [item for owned in self._by_owner.values() for item in owned]
        return tuple(
            sorted(
                values,
                key=lambda item: (
                    item.priority,
                    item.slot.value,
                    item.owner,
                    item.id,
                ),
            )
        )

    def for_platform(
        self,
        platform: UiPlatform,
        slot: UiSlot | None = None,
    ) -> tuple[UiContribution, ...]:
        return tuple(
            item
            for item in self.snapshot()
            if platform in item.platforms and (slot is None or item.slot is slot)
        )


class UserUiInitLoadError(RuntimeError):
    pass


class _UiRegistrationFacade:
    def __init__(self) -> None:
        self._items: list[UiContribution] = []

    @property
    def contributions(self) -> tuple[UiContribution, ...]:
        return tuple(self._items)

    def add(
        self,
        id: str,
        slot: str,
        kind: str,
        label: str,
        action: str = "",
        priority: int = 100,
        platforms: Sequence[str] = ("desktop", "android"),
    ) -> None:
        try:
            normalized_platforms = tuple(UiPlatform(value) for value in platforms)
            contribution = UiContribution(
                id=id,
                slot=UiSlot(slot),
                kind=UiContributionKind(kind),
                label=label,
                action=action,
                priority=priority,
                platforms=normalized_platforms,
            )
        except (TypeError, ValueError) as error:
            raise ValueError(f"invalid UI contribution {id!r}: {error}") from error
        self._items.append(contribution)

    def surface(
        self,
        id: str,
        label: str,
        action: str,
        *,
        priority: int = 100,
        platforms: Sequence[str] = ("desktop", "android"),
    ) -> None:
        self.add(id, "drawer", "surface", label, action, priority, platforms)

    def button(
        self,
        id: str,
        slot: str,
        label: str,
        action: str,
        *,
        priority: int = 100,
        platforms: Sequence[str] = ("desktop", "android"),
    ) -> None:
        self.add(id, slot, "button", label, action, priority, platforms)

    def text(
        self,
        id: str,
        slot: str,
        label: str,
        *,
        priority: int = 100,
        platforms: Sequence[str] = ("desktop", "android"),
    ) -> None:
        self.add(id, slot, "text", label, "", priority, platforms)


class UiInitLoader:
    def __init__(self, *, config_dir: Path, registry: UiExtensionRegistry) -> None:
        self.config_dir = Path(config_dir)
        self.registry = registry

    @property
    def path(self) -> Path:
        return self.config_dir / "init.py"

    def load(self) -> int:
        return self._load_generation()

    def reload(self) -> int:
        return self._load_generation()

    def _load_generation(self) -> int:
        if not self.path.exists():
            self.registry.clear_owner(_USER_INIT_OWNER)
            return 0

        facade = _UiRegistrationFacade()
        try:
            module = self._load_module()
            register = getattr(module, "register", None)
            if not callable(register):
                raise TypeError("register must be callable")
            register(facade)
            staged = UiExtensionRegistry()
            staged.replace_owner(_USER_INIT_OWNER, facade.contributions)
        except Exception as error:
            raise UserUiInitLoadError(
                f"failed to load init.py ({type(error).__name__})"
            ) from error

        self.registry.replace_owner(_USER_INIT_OWNER, staged.snapshot())
        return len(facade.contributions)

    def _load_module(self) -> ModuleType:
        module_name = f"_zara_user_ui_init_{uuid.uuid4().hex}"
        spec = importlib.util.spec_from_file_location(module_name, self.path)
        if spec is None or spec.loader is None:
            raise ImportError("unable to create init.py module spec")
        module = importlib.util.module_from_spec(spec)
        spec.loader.exec_module(module)
        return module


__all__ = [
    "PORTABLE_UI_ACTION_PREFIXES",
    "UiContribution",
    "UiContributionKind",
    "UiExtensionRegistry",
    "UiInitLoader",
    "UiPlatform",
    "UiSlot",
    "UserUiInitLoadError",
]
