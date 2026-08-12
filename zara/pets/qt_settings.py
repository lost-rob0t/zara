"""Qt settings dialog for the Pets section.

Lazy-imports PySide6. Provides controls equivalent to the task spec:

    Enabled, Selected Pet, Scale, Reduced Motion,
    Import Pet, Import ChatGPT Pet, Import from ChatGPT, Open Pet Folder
"""

from __future__ import annotations

import logging
import shutil
import subprocess
from pathlib import Path
from typing import Optional

from .discovery import ChatGPTPetDiscovery
from .importer import import_pet, preview
from .settings import PetSettings
from .storage import list_pets, pet_dir, remove_pet

logger = logging.getLogger(__name__)


def _open_folder(path: Path) -> None:
    try:
        subprocess.run(["xdg-open", str(path)], check=False, timeout=5)
    except (OSError, subprocess.TimeoutExpired):
        pass


def run_settings_dialog(settings: PetSettings) -> int:
    """Open the Pets settings dialog. Returns the Qt exit code."""
    from PySide6.QtCore import Qt
    from PySide6.QtWidgets import (
        QApplication, QDialog, QVBoxLayout, QFormLayout, QCheckBox,
        QComboBox, QDoubleSpinBox, QPushButton, QFileDialog, QMessageBox,
        QLabel, QHBoxLayout,
    )

    app = QApplication.instance() or QApplication([])
    dialog = QDialog()
    dialog.setWindowTitle("Zarathushtra — Pets")
    layout = QVBoxLayout(dialog)
    form = QFormLayout()

    enabled_box = QCheckBox("Enabled")
    enabled_box.setChecked(settings.state.enabled)
    form.addRow(enabled_box)

    pet_combo = QComboBox()
    installed = list_pets()
    for pet in installed:
        pet_combo.addItem(pet.name, userData=pet.id)
    if settings.state.selected_pet:
        idx = pet_combo.findData(settings.state.selected_pet)
        if idx >= 0:
            pet_combo.setCurrentIndex(idx)
    form.addRow("Selected Pet", pet_combo)

    scale_spin = QDoubleSpinBox()
    scale_spin.setRange(0.25, 4.0)
    scale_spin.setSingleStep(0.25)
    scale_spin.setValue(float(settings.state.scale or 1.0))
    form.addRow("Scale", scale_spin)

    motion_combo = QComboBox()
    motion_combo.addItem("System", "system")
    motion_combo.addItem("On", "on")
    motion_combo.addItem("Off", "off")
    idx = motion_combo.findData(settings.state.reduced_motion)
    if idx >= 0:
        motion_combo.setCurrentIndex(idx)
    form.addRow("Reduced Motion", motion_combo)

    layout.addLayout(form)

    import_btn = QPushButton("Import Pet...")
    chatgpt_btn = QPushButton("Import ChatGPT Pet...")
    discover_btn = QPushButton("Import from ChatGPT...")
    folder_btn = QPushButton("Open Pet Folder")

    def _import_native():
        path, _ = QFileDialog.getOpenFileName(
            dialog, "Import Pet", "", "Pet manifests (pet.json);;All Files (*)"
        )
        if not path:
            return
        try:
            manifest = import_pet(Path(path))
            QMessageBox.information(dialog, "Imported", f"Installed pet: {manifest.name}")
            pet_combo.addItem(manifest.name, userData=manifest.id)
        except Exception as exc:
            QMessageBox.warning(dialog, "Import failed", str(exc))

    def _import_chatgpt():
        path, _ = QFileDialog.getOpenFileName(
            dialog, "Import ChatGPT Pet",
            "",
            "Images (*.png *.webp);;Pet manifests (pet.json);;All Files (*)",
        )
        if not path:
            return
        try:
            manifest = import_pet(Path(path))
            QMessageBox.information(dialog, "Imported", f"Installed pet: {manifest.name}")
            pet_combo.addItem(manifest.name, userData=manifest.id)
        except Exception as exc:
            QMessageBox.warning(dialog, "Import failed", str(exc))

    def _discover():
        discovered = ChatGPTPetDiscovery().discover()
        if not discovered:
            QMessageBox.information(
                dialog, "Import from ChatGPT",
                "No ChatGPT/Codex pets were found in the standard locations.",
            )
            return
        from PySide6.QtWidgets import QDialog as _QDialog, QListWidget
        pick = _QDialog(dialog)
        pick.setWindowTitle("Import from ChatGPT")
        pick_layout = QVBoxLayout(pick)
        list_widget = QListWidget()
        for pet in discovered:
            list_widget.addItem(f"{pet.display_name} ({pet.source_format})")
        pick_layout.addWidget(list_widget)
        btn = QPushButton("Import selected")
        pick_layout.addWidget(btn)

        def _do_import():
            row = list_widget.currentRow()
            if row < 0 or row >= len(discovered):
                return
            target = discovered[row]
            try:
                manifest = import_pet(target.source_path)
                QMessageBox.information(pick, "Imported", f"Installed: {manifest.name}")
                pet_combo.addItem(manifest.name, userData=manifest.id)
            except Exception as exc:
                QMessageBox.warning(pick, "Import failed", str(exc))
            pick.accept()

        btn.clicked.connect(_do_import)
        pick.exec_()

    def _open_pet_folder():
        from .storage import pets_dir
        _open_folder(pets_dir())

    import_btn.clicked.connect(_import_native)
    chatgpt_btn.clicked.connect(_import_chatgpt)
    discover_btn.clicked.connect(_discover)
    folder_btn.clicked.connect(_open_pet_folder)

    btn_row = QHBoxLayout()
    btn_row.addWidget(import_btn)
    btn_row.addWidget(chatgpt_btn)
    btn_row.addWidget(discover_btn)
    btn_row.addWidget(folder_btn)
    layout.addLayout(btn_row)

    save_btn = QPushButton("Save")
    layout.addWidget(save_btn)

    def _save():
        settings.update(
            enabled=enabled_box.isChecked(),
            selected_pet=pet_combo.currentData(),
            scale=scale_spin.value(),
            reduced_motion=motion_combo.currentData(),
        )
        settings.save()
        dialog.accept()

    save_btn.clicked.connect(_save)
    return dialog.exec_()