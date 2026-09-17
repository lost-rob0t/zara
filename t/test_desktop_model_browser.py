from __future__ import annotations

import os
from pathlib import Path

os.environ.setdefault("QT_QPA_PLATFORM", "offscreen")

from PySide6.QtWidgets import QApplication, QComboBox, QLineEdit

from zara.config import DEFAULT_CONFIG_TOML, ZaraConfig
from zara.desktop.windows import SettingsWindow


def app() -> QApplication:
    instance = QApplication.instance()
    assert instance is None or isinstance(instance, QApplication)
    return instance or QApplication([])


def make_window(tmp_path: Path) -> tuple[SettingsWindow, Path]:
    app()
    model_dir = tmp_path / "models"
    model_dir.mkdir()
    (model_dir / "tiny.gguf").write_bytes(b"g" * 64)
    config_path = tmp_path / "config.toml"
    config_path.write_text(
        DEFAULT_CONFIG_TOML.replace(
            'model_dir = "~/.local/share/zarathushtra/models"',
            f'model_dir = "{model_dir}"',
        ),
        encoding="utf-8",
    )
    window = SettingsWindow(ZaraConfig(str(config_path)), repo_root=tmp_path)
    return window, config_path


def dispose(window: SettingsWindow) -> None:
    window.prepare_for_quit()
    window.close()
    window.deleteLater()
    app().processEvents()


def test_models_page_exposes_hardware_browser_and_offload_controls(tmp_path):
    window, _ = make_window(tmp_path)
    try:
        categories = [
            window.category_list.item(index).text()
            for index in range(window.category_list.count())
        ]
        assert "Models" in categories
        assert window.model_list.count() == 1
        assert "tiny.gguf" in window.model_list.item(0).text()
        assert window.hardware_summary.text()
        assert {
            "local_models.model_dir",
            "local_models.model_path",
            "local_models.binary",
            "local_models.managed",
            "local_models.offload_mode",
            "local_models.gpu_layers",
            "local_models.split_mode",
            "local_models.devices",
            "local_models.tensor_split",
            "local_models.main_gpu",
            "local_models.fit",
            "local_models.fit_target_mib",
            "local_models.context_size",
            "local_models.parallel",
        } <= set(window.setting_widgets)
    finally:
        dispose(window)


def test_selecting_gguf_model_configures_managed_llama_cpp(tmp_path):
    window, config_path = make_window(tmp_path)
    try:
        window.model_list.setCurrentRow(0)
        window.use_model_button.click()

        provider = window.setting_widgets["llm.provider"]
        assert isinstance(provider, QComboBox)
        assert provider.currentData() == "llama_cpp"
        model_path = window.setting_widgets["local_models.model_path"]
        assert isinstance(model_path, QLineEdit)
        assert model_path.text().endswith("tiny.gguf")

        window.save_settings()
        text = config_path.read_text(encoding="utf-8")
        assert 'provider = "llama_cpp"' in text
        assert 'model = "local"' in text
        assert 'managed = true' in text
        assert 'model_path = "' in text and "tiny.gguf" in text
    finally:
        dispose(window)


def test_model_fit_hint_changes_with_selection(tmp_path):
    window, _ = make_window(tmp_path)
    try:
        window.model_list.setCurrentRow(0)
        app().processEvents()
        assert "runtime overhead" in window.model_fit_hint.text().lower()
    finally:
        dispose(window)
