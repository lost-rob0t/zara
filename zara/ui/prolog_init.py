from __future__ import annotations

from pathlib import Path
from typing import Any

from zara.prolog_engine import PrologEngine

from .extensions import (
    UiContribution,
    UiContributionKind,
    UiExtensionRegistry,
    UiPlatform,
    UiSlot,
)


_USER_INIT_OWNER = "user:init.pl"
_MAX_CONTRIBUTIONS = 256


class UserPrologUiInitLoadError(RuntimeError):
    pass


def _text(value: Any) -> str:
    if isinstance(value, bytes):
        return value.decode("utf-8")
    return str(value)


class PrologUiInitLoader:
    """Execute trusted-local init.pl and project zara_ui/1 into the UI ABI."""

    def __init__(self, *, config_dir: Path, registry: UiExtensionRegistry) -> None:
        self.config_dir = Path(config_dir)
        self.registry = registry

    @property
    def path(self) -> Path:
        return self.config_dir / "init.pl"

    def load(self) -> int:
        return self._load_generation()

    def reload(self) -> int:
        return self._load_generation()

    def _load_generation(self) -> int:
        if not self.path.exists():
            self.registry.clear_owner(_USER_INIT_OWNER)
            return 0

        try:
            engine = PrologEngine(self.path)
            rows = engine.query_all(
                "zara_ui(ui(Id, Slot, Kind, Label, Action, Priority, Platforms))",
                max_solutions=_MAX_CONTRIBUTIONS + 1,
            )
            if len(rows) > _MAX_CONTRIBUTIONS:
                raise ValueError(
                    f"zara_ui may return at most {_MAX_CONTRIBUTIONS} contributions"
                )
            contributions = tuple(self._decode(row) for row in rows)
            staged = UiExtensionRegistry()
            staged.replace_owner(_USER_INIT_OWNER, contributions)
        except Exception as error:
            raise UserPrologUiInitLoadError(
                f"failed to load init.pl ({type(error).__name__})"
            ) from error

        self.registry.replace_owner(_USER_INIT_OWNER, staged.snapshot())
        return len(contributions)

    @staticmethod
    def _decode(row: dict[str, Any]) -> UiContribution:
        platforms = row.get("Platforms")
        if not isinstance(platforms, (list, tuple)):
            raise ValueError("zara_ui Platforms must be a list")
        priority = row.get("Priority")
        if isinstance(priority, bool) or not isinstance(priority, int):
            raise ValueError("zara_ui Priority must be an integer")
        return UiContribution(
            id=_text(row["Id"]),
            slot=UiSlot(_text(row["Slot"])),
            kind=UiContributionKind(_text(row["Kind"])),
            label=_text(row["Label"]),
            action=_text(row["Action"]),
            priority=priority,
            platforms=tuple(UiPlatform(_text(item)) for item in platforms),
        )


__all__ = ["PrologUiInitLoader", "UserPrologUiInitLoadError"]
