"""CLI entry point for the pet overlay + settings.

Usage:
    zara --pets           # launch the pet overlay
    zara --pets-settings  # open the settings dialog only

PySide6 is imported lazily so headless usage and tests never require Qt.
"""

from __future__ import annotations

import os
import sys
from collections.abc import MutableMapping
from typing import Optional

from .settings import PetSettings
from .storage import list_pets, load_pet


def _configure_pet_qpa_platform(
    environ: Optional[MutableMapping[str, str]] = None,
    platform_name: Optional[str] = None,
) -> Optional[str]:
    env = os.environ if environ is None else environ
    platform = sys.platform if platform_name is None else platform_name

    override = env.get("ZARA_PETS_QPA_PLATFORM", "").strip()
    if override:
        env["QT_QPA_PLATFORM"] = override
        return override

    configured = env.get("QT_QPA_PLATFORM", "").strip()
    if configured:
        return configured

    if not platform.startswith("linux"):
        return None

    session_type = env.get("XDG_SESSION_TYPE", "").strip().lower()
    wayland_session = session_type == "wayland" or bool(env.get("WAYLAND_DISPLAY"))
    if not wayland_session or not env.get("DISPLAY"):
        return None

    env["QT_QPA_PLATFORM"] = "xcb"
    return "xcb"


def _ensure_default_pet(settings: PetSettings):
    """Ensure a pet is selected and installed; install a synthetic one if needed."""
    selected = settings.state.selected_pet
    if selected:
        manifest = load_pet(selected)
        if manifest is not None:
            return manifest
    installed = list_pets()
    if installed:
        settings.update(selected_pet=installed[0].id)
        settings.save()
        return installed[0]
    # No pet installed: generate and install a synthetic default.
    from .importer import import_pet
    from pathlib import Path
    import subprocess
    import tempfile
    with tempfile.TemporaryDirectory() as tmp:
        subprocess.run(
            [sys.executable, "scripts/generate-pet-fixtures.py", tmp],
            check=False, capture_output=True,
        )
        native_json = Path(tmp) / "native" / "pet.json"
        if not native_json.exists():
            return None
        manifest = import_pet(native_json, pet_id="zara-default", display_name="Zara Default")
    settings.update(selected_pet=manifest.id)
    settings.save()
    return manifest


def main_overlay() -> int:
    _configure_pet_qpa_platform()
    from .qt_overlay import run_overlay
    settings = PetSettings()
    manifest = _ensure_default_pet(settings)
    if manifest is None:
        print("No pet available. Install one via 'zara --pets-settings'.",
              file=sys.stderr)
        return 1
    settings.update(enabled=True)
    settings.save()
    return run_overlay(settings, manifest)


def main_settings() -> int:
    from .qt_settings import run_settings_dialog
    settings = PetSettings()
    return run_settings_dialog(settings)


def main() -> int:
    if "--settings" in sys.argv:
        return main_settings()
    return main_overlay()
