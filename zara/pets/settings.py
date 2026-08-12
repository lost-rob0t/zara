"""Persistent pet settings (enabled, selected pet, position, scale, reduced motion).

Uses Zarathushtra's existing XDG-aware application data locations. A
single ``pet-state.json`` under ``$XDG_CONFIG_HOME/zarathushtra/`` holds
the runtime pet state (position, selected pet, scale). The static
``[pets]`` config section in ``config.toml`` holds defaults and the
enabled flag.
"""

from __future__ import annotations

import json
import logging
import os
from dataclasses import asdict, dataclass, field
from pathlib import Path
from typing import Optional

logger = logging.getLogger(__name__)

STATE_FILENAME = "pet-state.json"


def _config_dir() -> Path:
    xdg = os.getenv("XDG_CONFIG_HOME")
    if xdg:
        return Path(xdg) / "zarathushtra"
    return Path.home() / ".config" / "zarathushtra"


@dataclass
class PetWindowState:
    """Persisted runtime state of the pet overlay."""

    selected_pet: str = "zara-default"
    x: Optional[int] = None
    y: Optional[int] = None
    scale: float = 1.0
    reduced_motion: str = "system"  # system|on|off
    enabled: bool = False
    monitor_key: Optional[str] = None  # heuristic monitor identity

    def to_dict(self) -> dict:
        return asdict(self)

    @classmethod
    def from_dict(cls, data: dict) -> "PetWindowState":
        return cls(
            selected_pet=str(data.get("selected_pet", "zara-default")),
            x=int(data["x"]) if data.get("x") is not None else None,
            y=int(data["y"]) if data.get("y") is not None else None,
            scale=float(data.get("scale", 1.0)),
            reduced_motion=str(data.get("reduced_motion", "system")),
            enabled=bool(data.get("enabled", False)),
            monitor_key=str(data["monitor_key"]) if data.get("monitor_key") else None,
        )


class PetSettings:
    """Load/save the persisted pet state."""

    def __init__(self, path: Optional[Path] = None) -> None:
        self.path = path or (_config_dir() / STATE_FILENAME)
        self.state = self.load()

    def load(self) -> PetWindowState:
        if not self.path.exists():
            return PetWindowState()
        try:
            data = json.loads(self.path.read_text(encoding="utf-8"))
            if not isinstance(data, dict):
                return PetWindowState()
            return PetWindowState.from_dict(data)
        except (OSError, json.JSONDecodeError, ValueError) as exc:
            logger.warning("[PetSettings] failed to load %s: %s", self.path, exc)
            return PetWindowState()

    def save(self, state: Optional[PetWindowState] = None) -> None:
        if state is not None:
            self.state = state
        self.path.parent.mkdir(parents=True, exist_ok=True)
        self.path.write_text(
            json.dumps(self.state.to_dict(), indent=2, sort_keys=True),
            encoding="utf-8",
        )

    def update(self, **changes) -> PetWindowState:
        for key, value in changes.items():
            if hasattr(self.state, key):
                setattr(self.state, key, value)
        return self.state