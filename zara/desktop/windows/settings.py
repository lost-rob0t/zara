"""Native Zara settings and Prolog source workspace.

THESIS: Configuration is one inspectable route; the surface refuses nested preference cards.
OWN-WORLD: Signal Cabin rails, open forms, semantic theme swatches, and a real code field.
STORY: Choose a look, tune Zara, edit source, add validated facts, save with clear restart truth.
FIRST VIEWPORT: Category rail left, one generous task surface center, save state anchored below.
FORM: The established Signal Cabin operating surface; seed 35e80c4d.
FINISH: unreviewed and undocumented is unfinished; this build ends with the finish review, the verdict, DESIGN.md, and every shipping raster carrying its provenance
"""

from __future__ import annotations

import os
import shlex
import tempfile
from collections.abc import Callable
from pathlib import Path
from typing import Any, Optional

from PySide6.QtCore import QRectF, Qt, Signal
from PySide6.QtGui import QColor, QCloseEvent, QPainter, QPaintEvent, QPen
from PySide6.QtWidgets import (
    QButtonGroup,
    QCheckBox,
    QComboBox,
    QDialog,
    QDialogButtonBox,
    QDoubleSpinBox,
    QFormLayout,
    QFrame,
    QHBoxLayout,
    QLabel,
    QLineEdit,
    QListWidget,
    QListWidgetItem,
    QPlainTextEdit,
    QPushButton,
    QScrollArea,
    QSpinBox,
    QSplitter,
    QStackedWidget,
    QVBoxLayout,
    QWidget,
)

from zara.config import ZaraConfig
from zara.desktop.preferences import SettingsDocument, SettingsValidationError
from zara.desktop.prolog_studio import (
    FACT_TYPES,
    ManagedFact,
    ManagedFactStore,
    PrologHighlighter,
    PrologSourceRepository,
    PrologStudioError,
)
from zara.desktop.theme import THEME_REGISTRY


_CATEGORIES = (
    "Appearance",
    "Assistant",
    "Voice & Speech",
    "Tools & Privacy",
    "Prolog",
    "Advanced",
)

_FACT_LABELS = {
    "app_mapping": "App mapping",
    "direct_app": "Direct app",
    "search_engine": "Search engine",
    "dictation_command": "Dictation command",
    "timer_sound": "Timer sound",
    "alarm_sound": "Alarm sound",
    "llm_provider": "LLM provider",
    "llm_model": "LLM model",
    "llm_endpoint": "LLM endpoint",
    "todo_destination": "TODO destination",
    "todo_context_mode": "TODO context mode",
    "verb_intent": "Intent mapping",
}


class ThemePreviewButton(QPushButton):
    """A compact, faithful preview of one complete semantic palette."""

    def __init__(self, theme_key: str, parent: Optional[QWidget] = None) -> None:
        definition = THEME_REGISTRY[theme_key]
        super().__init__(definition.label, parent)
        self.setObjectName("zaraThemePreview")
        self.theme_key = theme_key
        self.colors = definition.colors
        self.setCheckable(True)
        self.setAccessibleName(f"Use {definition.label} theme")
        self.setToolTip(definition.description)
        self.setMinimumSize(112, 72)

    def paintEvent(self, event: QPaintEvent) -> None:  # noqa: N802 - Qt API
        event.accept()
        painter = QPainter(self)
        painter.setRenderHint(QPainter.RenderHint.Antialiasing)
        rect = QRectF(self.rect()).adjusted(2.0, 2.0, -2.0, -2.0)
        emphasized = self.isChecked() or self.hasFocus()
        border = self.colors["primary"] if emphasized else self.colors["line_strong"]
        painter.setPen(QPen(QColor(border), 2.0 if emphasized else 1.0))
        painter.setBrush(QColor(self.colors["ground"]))
        painter.drawRoundedRect(rect, 10.0, 10.0)

        panel = QRectF(rect.left() + 8.0, rect.top() + 9.0, rect.width() - 16.0, 24.0)
        painter.setPen(Qt.PenStyle.NoPen)
        painter.setBrush(QColor(self.colors["panel_lift"]))
        painter.drawRoundedRect(panel, 6.0, 6.0)
        painter.setPen(QColor(self.colors["text"]))
        painter.drawText(panel.adjusted(8.0, 0.0, -4.0, 0.0), Qt.AlignmentFlag.AlignVCenter, self.text())

        for index, key in enumerate(("primary", "active", "danger")):
            painter.setBrush(QColor(self.colors[key]))
            painter.setPen(Qt.PenStyle.NoPen)
            painter.drawEllipse(QRectF(rect.left() + 11.0 + index * 20.0, rect.bottom() - 21.0, 10.0, 10.0))


class FactEditorDialog(QDialog):
    """Guided fact input whose output is ordinary Python data."""

    def __init__(self, parent: Optional[QWidget] = None, fact: ManagedFact | None = None) -> None:
        super().__init__(parent)
        self.setWindowTitle("Edit fact" if fact else "Add fact")
        self.setMinimumWidth(440)
        self.kind_combo = QComboBox()
        for kind in FACT_TYPES:
            self.kind_combo.addItem(_FACT_LABELS[kind], kind)
        self.fields_form = QFormLayout()
        self.inputs: dict[str, QWidget] = {}

        layout = QVBoxLayout(self)
        layout.addWidget(QLabel("Fact type"))
        layout.addWidget(self.kind_combo)
        layout.addLayout(self.fields_form)
        buttons = QDialogButtonBox(
            QDialogButtonBox.StandardButton.Save | QDialogButtonBox.StandardButton.Cancel
        )
        buttons.accepted.connect(self.accept)
        buttons.rejected.connect(self.reject)
        layout.addWidget(buttons)

        self.kind_combo.currentIndexChanged.connect(self._rebuild_fields)
        if fact is not None:
            self.kind_combo.setCurrentIndex(self.kind_combo.findData(fact.kind))
        self._rebuild_fields()
        if fact is not None:
            self._load_fields(fact.fields)

    @property
    def fact_kind(self) -> str:
        return str(self.kind_combo.currentData())

    def values(self) -> dict[str, Any]:
        values: dict[str, Any] = {}
        for name, widget in self.inputs.items():
            if isinstance(widget, QLineEdit):
                value: Any = widget.text().strip()
            elif isinstance(widget, QComboBox):
                value = widget.currentData()
            else:
                continue
            if name in {"argv", "command"}:
                try:
                    value = shlex.split(value)
                except ValueError as error:
                    raise PrologStudioError(f"command could not be parsed: {error}") from error
                name = "argv"
            values[name] = value
        return values

    def _clear_fields(self) -> None:
        while self.fields_form.rowCount():
            self.fields_form.removeRow(0)
        self.inputs.clear()

    def _line(self, name: str, label: str, placeholder: str = "") -> None:
        widget = QLineEdit()
        widget.setPlaceholderText(placeholder)
        self.inputs[name] = widget
        self.fields_form.addRow(label, widget)

    def _choice(self, name: str, label: str, choices: list[tuple[str, Any]]) -> None:
        widget = QComboBox()
        for text, value in choices:
            widget.addItem(text, value)
        self.inputs[name] = widget
        self.fields_form.addRow(label, widget)

    def _rebuild_fields(self) -> None:
        self._clear_fields()
        kind = self.fact_kind
        if kind == "app_mapping":
            self._line("name", "App name", "studio")
            self._line("argv", "Command", "code --new-window")
        elif kind == "direct_app":
            self._line("name", "App name", "wireshark")
        elif kind == "search_engine":
            self._line("template", "URL template", "https://example.test/?q=%s")
        elif kind == "dictation_command":
            self._line("argv", "Command", "zara-dictate small cpu")
        elif kind in {"timer_sound", "alarm_sound"}:
            self._line("value", "Sound", "disabled or /path/to/sound.wav")
        elif kind == "llm_provider":
            self._choice(
                "value",
                "Provider",
                [("Ollama", "ollama"), ("OpenAI", "openai"), ("Anthropic", "anthropic")],
            )
        elif kind in {"llm_model", "llm_endpoint", "todo_destination"}:
            self._line("value", _FACT_LABELS[kind])
        elif kind == "todo_context_mode":
            self._choice(
                "value",
                "Mode",
                [
                    ("Infer", "infer"),
                    ("Infer with LLM", "infer_with_llm"),
                    ("LLM only", "llm_only"),
                ],
            )
        else:
            self._line("phrase", "Phrase", "summon studio")
            self._line("intent", "Action", "open")
            self._choice("arity", "Arguments", [("None", 0), ("One", 1), ("Two", 2), ("Rest", "rest")])

    def _load_fields(self, fields: dict[str, Any]) -> None:
        for name, value in fields.items():
            widget = self.inputs.get(name)
            if isinstance(widget, QLineEdit):
                if name == "argv" and isinstance(value, list):
                    widget.setText(shlex.join(value))
                else:
                    widget.setText(str(value))
            elif isinstance(widget, QComboBox):
                index = widget.findData(value)
                if index >= 0:
                    widget.setCurrentIndex(index)


class SettingsWindow(QWidget):
    """One process-owned settings workspace for desktop and Prolog configuration."""

    theme_preview_requested = Signal(str)
    restart_requested = Signal()

    def __init__(
        self,
        config: ZaraConfig,
        *,
        repo_root: Path | None = None,
        prolog_reload: Callable[[], bool] | None = None,
        parent: Optional[QWidget] = None,
    ) -> None:
        super().__init__(parent)
        self.setObjectName("zaraSettings")
        self.setWindowTitle("Zara Settings")
        self.setMinimumSize(900, 640)
        self.resize(1120, 760)
        self.config = config
        self.document = SettingsDocument(config)
        self.repo_root = (repo_root or Path(__file__).resolve().parents[3]).resolve()
        self.user_prolog_config = config.config_dir / "config.pl"
        if not self.user_prolog_config.exists():
            self.user_prolog_config.write_text("% Zarathushtra User Configuration\n", encoding="utf-8")
        self.fact_store = ManagedFactStore(self.user_prolog_config)
        self.source_repository = PrologSourceRepository(self.repo_root, self.user_prolog_config)
        self.prolog_reload = prolog_reload
        self._allow_close = False
        self.setting_widgets: dict[str, QWidget] = {}
        self.theme_buttons: list[ThemePreviewButton] = []

        self.category_list = QListWidget()
        self.category_list.setObjectName("zaraSettingsCategories")
        for category in _CATEGORIES:
            self.category_list.addItem(category)
        self.category_list.setFixedWidth(190)

        rail = QWidget()
        rail.setObjectName("zaraSettingsRail")
        rail_layout = QVBoxLayout(rail)
        rail_layout.setContentsMargins(14, 18, 14, 18)
        brand = QLabel("ZARA")
        brand.setObjectName("zaraBrandName")
        rail_layout.addWidget(brand)
        rail_layout.addSpacing(18)
        rail_layout.addWidget(self.category_list, 1)

        self.stack = QStackedWidget()
        self.stack.addWidget(self._appearance_page())
        self.stack.addWidget(self._assistant_page())
        self.stack.addWidget(self._voice_page())
        self.stack.addWidget(self._tools_page())
        self.stack.addWidget(self._prolog_page())
        self.stack.addWidget(self._advanced_page())

        self.feedback_label = QLabel("Changes to runtime settings apply after restart.")
        self.feedback_label.setObjectName("zaraSettingsHint")
        self.feedback_label.setWordWrap(True)
        self.save_button = QPushButton("Save settings")
        self.save_button.setObjectName("zaraPrimaryAction")
        self.restart_button = QPushButton("Restart Zara")
        self.restart_button.setObjectName("zaraSecondaryAction")
        footer = QFrame()
        footer.setObjectName("zaraSettingsFooter")
        footer_layout = QHBoxLayout(footer)
        footer_layout.setContentsMargins(22, 12, 22, 14)
        footer_layout.addWidget(self.feedback_label, 1)
        footer_layout.addWidget(self.restart_button)
        footer_layout.addWidget(self.save_button)

        content = QWidget()
        content_layout = QVBoxLayout(content)
        content_layout.setContentsMargins(0, 0, 0, 0)
        content_layout.addWidget(self.stack, 1)
        content_layout.addWidget(footer)

        layout = QHBoxLayout(self)
        layout.setContentsMargins(0, 0, 0, 0)
        layout.setSpacing(0)
        layout.addWidget(rail)
        layout.addWidget(content, 1)

        self.category_list.currentRowChanged.connect(self.stack.setCurrentIndex)
        self.category_list.setCurrentRow(0)
        self.save_button.clicked.connect(self.save_settings)
        self.restart_button.clicked.connect(self.restart_requested.emit)

    def _page(self, title: str, description: str) -> tuple[QWidget, QFormLayout]:
        body = QWidget()
        body_layout = QVBoxLayout(body)
        body_layout.setContentsMargins(28, 24, 34, 30)
        body_layout.setSpacing(10)
        title_label = QLabel(title)
        title_label.setObjectName("zaraSectionTitle")
        description_label = QLabel(description)
        description_label.setObjectName("zaraSectionDescription")
        description_label.setWordWrap(True)
        body_layout.addWidget(title_label)
        body_layout.addWidget(description_label)
        body_layout.addSpacing(12)
        form = QFormLayout()
        form.setHorizontalSpacing(24)
        form.setVerticalSpacing(13)
        form.setFieldGrowthPolicy(QFormLayout.FieldGrowthPolicy.AllNonFixedFieldsGrow)
        body_layout.addLayout(form)
        body_layout.addStretch(1)
        scroll = QScrollArea()
        scroll.setFrameShape(QFrame.Shape.NoFrame)
        scroll.setWidgetResizable(True)
        scroll.setWidget(body)
        return scroll, form

    def _value(self, dotted_key: str, default: Any) -> Any:
        parts = dotted_key.split(".")
        current: Any = self.config.get_section(parts[0])
        for part in parts[1:]:
            if not isinstance(current, dict) or part not in current:
                return default
            current = current[part]
        return current

    def _register(self, form: QFormLayout, key: str, label: str, widget: QWidget) -> QWidget:
        self.setting_widgets[key] = widget
        form.addRow(label, widget)
        return widget

    def _line_setting(self, form: QFormLayout, key: str, label: str, default: str = "") -> QLineEdit:
        widget = QLineEdit(str(self._value(key, default)))
        return self._register(form, key, label, widget)  # type: ignore[return-value]

    def _check_setting(self, form: QFormLayout, key: str, label: str, default: bool) -> QCheckBox:
        widget = QCheckBox()
        widget.setChecked(bool(self._value(key, default)))
        return self._register(form, key, label, widget)  # type: ignore[return-value]

    def _spin_setting(
        self,
        form: QFormLayout,
        key: str,
        label: str,
        default: int,
        minimum: int,
        maximum: int,
    ) -> QSpinBox:
        widget = QSpinBox()
        widget.setRange(minimum, maximum)
        widget.setValue(int(self._value(key, default)))
        return self._register(form, key, label, widget)  # type: ignore[return-value]

    def _double_setting(
        self,
        form: QFormLayout,
        key: str,
        label: str,
        default: float,
        minimum: float,
        maximum: float,
        step: float,
    ) -> QDoubleSpinBox:
        widget = QDoubleSpinBox()
        widget.setRange(minimum, maximum)
        widget.setSingleStep(step)
        widget.setDecimals(2)
        widget.setValue(float(self._value(key, default)))
        return self._register(form, key, label, widget)  # type: ignore[return-value]

    def _combo_setting(
        self,
        form: QFormLayout,
        key: str,
        label: str,
        choices: list[tuple[str, str]],
        default: str,
    ) -> QComboBox:
        widget = QComboBox()
        for text, value in choices:
            widget.addItem(text, value)
        selected = widget.findData(self._value(key, default))
        widget.setCurrentIndex(max(0, selected))
        return self._register(form, key, label, widget)  # type: ignore[return-value]

    def _appearance_page(self) -> QWidget:
        page, form = self._page("Appearance", "Choose one complete theme. The preview applies to every open Zara surface immediately.")
        theme = self._combo_setting(
            form,
            "desktop.theme",
            "Theme",
            [(definition.label, key) for key, definition in THEME_REGISTRY.items()],
            "signal-cabin",
        )
        swatches = QWidget()
        swatches.setMinimumHeight(84)
        swatches_layout = QHBoxLayout(swatches)
        swatches_layout.setContentsMargins(0, 4, 0, 4)
        swatches_layout.setSpacing(8)
        group = QButtonGroup(swatches)
        group.setExclusive(True)
        current_key = str(theme.currentData())
        for key in THEME_REGISTRY:
            button = ThemePreviewButton(key)
            button.setChecked(key == current_key)
            button.clicked.connect(lambda _checked=False, value=key: theme.setCurrentIndex(theme.findData(value)))
            group.addButton(button)
            swatches_layout.addWidget(button)
            self.theme_buttons.append(button)
        form.addRow("Preview", swatches)
        theme.currentIndexChanged.connect(lambda _index: self._sync_theme_buttons(str(theme.currentData())))
        theme.currentIndexChanged.connect(lambda _index: self._preview_theme(str(theme.currentData())))
        return page

    def _assistant_page(self) -> QWidget:
        page, form = self._page("Assistant", "Provider, model, conversation depth, and agent behavior.")
        self._combo_setting(form, "llm.provider", "Provider", [("Ollama", "ollama"), ("OpenAI", "openai"), ("Anthropic", "anthropic"), ("OpenRouter", "openrouter")], "ollama")
        self._line_setting(form, "llm.model", "Model")
        self._line_setting(form, "llm.endpoint", "Endpoint")
        self._spin_setting(form, "llm.history_limit", "History messages", 20, 1, 500)
        self._spin_setting(form, "agent.max_steps", "Maximum tool steps", 10, 1, 100)
        prompt = QPlainTextEdit(str(self._value("agent.system_prompt", "")))
        prompt.setMaximumHeight(130)
        self._register(form, "agent.system_prompt", "System prompt", prompt)
        return page

    def _voice_page(self) -> QWidget:
        page, form = self._page("Voice & Speech", "Wake sensitivity, speech recognition, and voice output.")
        self._double_setting(form, "wake.threshold", "Wake threshold", 0.5, 0.0, 1.0, 0.05)
        self._double_setting(form, "wake.silence_duration", "Silence duration", 1.5, 0.1, 30.0, 0.1)
        self._combo_setting(form, "stt.provider", "Speech recognition", [("Faster Whisper", "faster-whisper"), ("whisper.cpp", "whisper-cpp"), ("Whisper", "whisper")], "faster-whisper")
        self._line_setting(form, "stt.model", "Speech model", "small")
        self._combo_setting(form, "stt.device", "Speech device", [("CPU", "cpu"), ("CUDA", "cuda"), ("Vulkan", "vulkan")], "cpu")
        self._combo_setting(form, "tts.provider", "Voice provider", [("Local", "local"), ("ElevenLabs", "11labs"), ("Edge", "edge"), ("Qwen3", "qwen3")], "qwen3")
        self._check_setting(form, "wake.acknowledgement.enabled", "Immediate acknowledgement", True)
        self._line_setting(form, "wake.acknowledgement.voice", "Acknowledgement voice", "en-US-AriaNeural")
        return page

    def _tools_page(self) -> QWidget:
        page, form = self._page("Tools & Privacy", "Control local tools, memory, file access, and anonymous latency metrics.")
        for key, label, default in (
            ("tools.calculator", "Calculator", True),
            ("tools.get_current_time", "Current time", True),
            ("tools.query_prolog", "Query Prolog", True),
            ("tools.remember", "Remember", True),
            ("tools.recall", "Recall", True),
            ("tools.file_tools", "File tools", False),
            ("memory.enabled", "Long-term memory", True),
            ("latency.enabled", "Latency metrics", True),
        ):
            self._check_setting(form, key, label, default)
        return page

    def _prolog_page(self) -> QWidget:
        page = QWidget()
        layout = QVBoxLayout(page)
        layout.setContentsMargins(28, 24, 30, 28)
        title = QLabel("Prolog")
        title.setObjectName("zaraSectionTitle")
        description = QLabel("Edit approved source with syntax highlighting, or add validated facts to the actual user config without writing Prolog.")
        description.setObjectName("zaraSectionDescription")
        description.setWordWrap(True)
        layout.addWidget(title)
        layout.addWidget(description)
        layout.addSpacing(10)

        splitter = QSplitter()
        source = QWidget()
        source_layout = QVBoxLayout(source)
        source_layout.setContentsMargins(0, 0, 14, 0)
        self.source_combo = QComboBox()
        for entry in self.source_repository.list():
            self.source_combo.addItem(entry.label, entry.id)
        self.source_status = QLabel()
        self.source_status.setObjectName("zaraSettingsHint")
        self.prolog_editor = QPlainTextEdit()
        self.prolog_editor.setObjectName("zaraPrologEditor")
        self.prolog_editor.setLineWrapMode(QPlainTextEdit.LineWrapMode.NoWrap)
        self.prolog_highlighter = PrologHighlighter(
            self.prolog_editor.document(),
            str(self._value("desktop.theme", "signal-cabin")),
        )
        self.save_prolog_button = QPushButton("Save source")
        self.save_prolog_button.setObjectName("zaraPrimaryAction")
        source_layout.addWidget(self.source_combo)
        source_layout.addWidget(self.source_status)
        source_layout.addWidget(self.prolog_editor, 1)
        source_layout.addWidget(self.save_prolog_button, 0, Qt.AlignmentFlag.AlignRight)

        facts = QWidget()
        facts.setObjectName("zaraKnowledgeStudioRail")
        facts_layout = QVBoxLayout(facts)
        facts_layout.setContentsMargins(16, 0, 0, 0)
        facts_header = QHBoxLayout()
        facts_header.addWidget(QLabel("Managed facts"))
        facts_header.addStretch(1)
        self.add_fact_button = QPushButton("Add")
        self.add_fact_button.setObjectName("zaraPrimaryAction")
        facts_header.addWidget(self.add_fact_button)
        self.fact_list = QListWidget()
        self.fact_list.setObjectName("zaraFactList")
        self.edit_fact_button = QPushButton("Edit")
        self.delete_fact_button = QPushButton("Delete")
        self.delete_fact_button.setObjectName("zaraDangerAction")
        fact_actions = QHBoxLayout()
        fact_actions.addWidget(self.edit_fact_button)
        fact_actions.addWidget(self.delete_fact_button)
        facts_layout.addLayout(facts_header)
        facts_layout.addWidget(self.fact_list, 1)
        facts_layout.addLayout(fact_actions)

        splitter.addWidget(source)
        splitter.addWidget(facts)
        splitter.setSizes([620, 300])
        layout.addWidget(splitter, 1)

        self.source_combo.currentIndexChanged.connect(self._load_selected_source)
        self.save_prolog_button.clicked.connect(self.save_prolog_source)
        self.add_fact_button.clicked.connect(self._open_add_fact)
        self.edit_fact_button.clicked.connect(self._open_edit_fact)
        self.delete_fact_button.clicked.connect(self.delete_selected_fact)
        self.fact_list.currentItemChanged.connect(self._sync_fact_actions)
        self._load_selected_source()
        self._refresh_facts()
        return page

    def _advanced_page(self) -> QWidget:
        page, form = self._page("Advanced", "Storage, runtime bounds, and the canonical config.toml source.")
        self._line_setting(form, "database.path", "Database path", "~/.local/share/zarathushtra/zara.db")
        self._line_setting(form, "prolog.main_file", "Prolog main file", "main.pl")
        self._check_setting(form, "prolog.load_on_startup", "Load Prolog on startup", True)
        self._double_setting(form, "plugins.lifecycle_timeout", "Plugin lifecycle timeout", 5.0, 0.1, 120.0, 0.5)
        self._spin_setting(form, "plugins.event_queue_size", "Plugin event queue", 256, 1, 4096)
        self.config_editor = QPlainTextEdit(self.config.config_file.read_text(encoding="utf-8"))
        self.config_editor.setObjectName("zaraConfigEditor")
        self.config_editor.setLineWrapMode(QPlainTextEdit.LineWrapMode.NoWrap)
        self.config_editor.setMinimumHeight(260)
        form.addRow("config.toml", self.config_editor)
        self.save_config_button = QPushButton("Validate and save file")
        self.save_config_button.clicked.connect(self.save_config_source)
        form.addRow("", self.save_config_button)
        return page

    def _setting_value(self, widget: QWidget) -> Any:
        if isinstance(widget, QLineEdit):
            return widget.text()
        if isinstance(widget, QPlainTextEdit):
            return widget.toPlainText()
        if isinstance(widget, QComboBox):
            return widget.currentData()
        if isinstance(widget, QCheckBox):
            return widget.isChecked()
        if isinstance(widget, (QSpinBox, QDoubleSpinBox)):
            return widget.value()
        raise SettingsValidationError("unsupported settings control")

    def _preview_theme(self, theme_key: str) -> None:
        if hasattr(self, "prolog_highlighter"):
            self.prolog_highlighter.set_theme(theme_key)
        self.theme_preview_requested.emit(theme_key)

    def _sync_theme_buttons(self, theme_key: str) -> None:
        for button in self.theme_buttons:
            button.setChecked(button.theme_key == theme_key)

    def save_settings(self) -> None:
        values = {key: self._setting_value(widget) for key, widget in self.setting_widgets.items()}
        try:
            self.document.update(values)
        except (OSError, SettingsValidationError) as error:
            self.feedback_label.setText(f"Settings were not saved: {error}")
            return
        self.config_editor.setPlainText(self.config.config_file.read_text(encoding="utf-8"))
        self.feedback_label.setText("Settings saved. Restart Zara to apply runtime changes.")

    def save_config_source(self) -> None:
        try:
            self.document.replace_source(self.config_editor.toPlainText())
        except (OSError, SettingsValidationError) as error:
            self.feedback_label.setText(f"config.toml was not saved: {error}")
            return
        theme = str(self.config.get("desktop", "theme", "signal-cabin"))
        self.setting_widgets["desktop.theme"].setCurrentIndex(
            self.setting_widgets["desktop.theme"].findData(theme)  # type: ignore[union-attr]
        )
        self.feedback_label.setText("config.toml saved. Restart Zara to apply runtime changes.")

    def _load_selected_source(self) -> None:
        source_id = str(self.source_combo.currentData() or "")
        try:
            source = next(entry for entry in self.source_repository.list() if entry.id == source_id)
            text = self.source_repository.read(source_id)
        except (OSError, StopIteration, PrologStudioError) as error:
            self.prolog_editor.clear()
            self.prolog_editor.setReadOnly(True)
            self.save_prolog_button.setEnabled(False)
            self.source_status.setText(str(error))
            return
        self.prolog_editor.setPlainText(text)
        self.prolog_editor.setReadOnly(not source.writable)
        self.save_prolog_button.setEnabled(source.writable)
        self.source_status.setText(str(source.path) + ("" if source.writable else " · read-only"))

    def save_prolog_source(self) -> None:
        source_id = str(self.source_combo.currentData() or "")
        source = next(
            (entry for entry in self.source_repository.list() if entry.id == source_id),
            None,
        )
        if source is None:
            self.feedback_label.setText("Prolog source was not saved: source is not approved")
            return
        before = source.path.read_bytes()
        try:
            self.source_repository.write(source_id, self.prolog_editor.toPlainText())
            if source_id == "user-config" and not self._reload_prolog():
                self._restore_bytes(source.path, before)
                self.feedback_label.setText("Prolog source was restored because config.pl could not be reloaded.")
                self._load_selected_source()
                return
        except (OSError, PrologStudioError) as error:
            self.feedback_label.setText(f"Prolog source was not saved: {error}")
            return
        if source_id == "user-config" and self.prolog_reload is None:
            self.feedback_label.setText("config.pl saved. Restart Zara to load the new Prolog source.")
        else:
            self.feedback_label.setText("Prolog source saved.")
        self._refresh_facts()

    def add_fact(self, kind: str, fields: dict[str, Any]) -> ManagedFact | None:
        before = self.user_prolog_config.read_bytes()
        try:
            fact = self.fact_store.add(kind, fields)
            if not self._reload_prolog():
                self._restore_bytes(self.user_prolog_config, before)
                self.feedback_label.setText("Fact was restored because config.pl could not be reloaded.")
                self._refresh_facts()
                self._load_selected_source_if_user_config()
                return None
        except (OSError, PrologStudioError) as error:
            self.feedback_label.setText(f"Fact was not added: {error}")
            return None
        if self.prolog_reload is None:
            self.feedback_label.setText("Fact added to config.pl. Restart Zara to load it.")
        else:
            self.feedback_label.setText("Fact added to config.pl and reloaded.")
        self._refresh_facts()
        self._load_selected_source_if_user_config()
        return fact

    def _open_add_fact(self) -> None:
        dialog = FactEditorDialog(self)
        if dialog.exec() != QDialog.DialogCode.Accepted:
            return
        try:
            fields = dialog.values()
        except PrologStudioError as error:
            self.feedback_label.setText(f"Fact was not added: {error}")
            return
        self.add_fact(dialog.fact_kind, fields)

    def _selected_fact(self) -> ManagedFact | None:
        item = self.fact_list.currentItem()
        if item is None:
            return None
        fact_id = str(item.data(Qt.ItemDataRole.UserRole))
        return next((fact for fact in self.fact_store.list() if fact.id == fact_id), None)

    def _open_edit_fact(self) -> None:
        fact = self._selected_fact()
        if fact is None:
            self.feedback_label.setText("Select a fact to edit.")
            return
        dialog = FactEditorDialog(self, fact)
        if dialog.exec() != QDialog.DialogCode.Accepted:
            return
        before = self.user_prolog_config.read_bytes()
        try:
            fields = dialog.values()
            self.fact_store.update(fact.id, dialog.fact_kind, fields)
            if not self._reload_prolog():
                self._restore_bytes(self.user_prolog_config, before)
                self.feedback_label.setText("Fact edit was restored because config.pl could not be reloaded.")
            else:
                message = "Fact updated in config.pl and reloaded."
                if self.prolog_reload is None:
                    message = "Fact updated in config.pl. Restart Zara to load it."
                self.feedback_label.setText(message)
        except (OSError, PrologStudioError) as error:
            self.feedback_label.setText(f"Fact was not updated: {error}")
        self._refresh_facts()
        self._load_selected_source_if_user_config()

    def delete_selected_fact(self) -> None:
        fact = self._selected_fact()
        if fact is None:
            self.feedback_label.setText("Select a fact to delete.")
            return
        before = self.user_prolog_config.read_bytes()
        try:
            self.fact_store.delete(fact.id)
            if not self._reload_prolog():
                self._restore_bytes(self.user_prolog_config, before)
                self.feedback_label.setText("Fact deletion was restored because config.pl could not be reloaded.")
            else:
                message = "Fact deleted from config.pl and reloaded."
                if self.prolog_reload is None:
                    message = "Fact deleted from config.pl. Restart Zara to load it."
                self.feedback_label.setText(message)
        except (OSError, PrologStudioError) as error:
            self.feedback_label.setText(f"Fact was not deleted: {error}")
        self._refresh_facts()
        self._load_selected_source_if_user_config()

    def _reload_prolog(self) -> bool:
        if self.prolog_reload is None:
            return True
        try:
            return bool(self.prolog_reload())
        except Exception as error:
            self.feedback_label.setText(f"config.pl could not be reloaded: {error}")
            return False

    @staticmethod
    def _restore_bytes(path: Path, content: bytes) -> None:
        temporary_path: Path | None = None
        try:
            with tempfile.NamedTemporaryFile(
                "wb",
                dir=path.parent,
                prefix=f".{path.name}.",
                suffix=".restore",
                delete=False,
            ) as temporary:
                temporary.write(content)
                temporary.flush()
                os.fsync(temporary.fileno())
                temporary_path = Path(temporary.name)
            if path.exists():
                os.chmod(temporary_path, path.stat().st_mode)
            os.replace(temporary_path, path)
            temporary_path = None
        finally:
            if temporary_path is not None:
                temporary_path.unlink(missing_ok=True)

    def _refresh_facts(self) -> None:
        selected = self._selected_fact()
        selected_id = selected.id if selected is not None else None
        self.fact_list.clear()
        try:
            facts = self.fact_store.list()
        except PrologStudioError as error:
            self.feedback_label.setText(f"Facts could not be read: {error}")
            self._sync_fact_actions()
            return
        for fact in facts:
            item = QListWidgetItem(self._fact_summary(fact))
            item.setData(Qt.ItemDataRole.UserRole, fact.id)
            self.fact_list.addItem(item)
            if fact.id == selected_id:
                self.fact_list.setCurrentItem(item)
        self._sync_fact_actions()

    def _sync_fact_actions(self, *_items: object) -> None:
        has_selection = self.fact_list.currentItem() is not None
        self.edit_fact_button.setEnabled(has_selection)
        self.delete_fact_button.setEnabled(has_selection)

    @staticmethod
    def _fact_summary(fact: ManagedFact) -> str:
        if fact.kind == "app_mapping":
            return f"App · {fact.fields['name']} → {shlex.join(fact.fields['argv'])}"
        if fact.kind == "verb_intent":
            return f"Intent · {fact.fields['phrase']} → {fact.fields['intent']}"
        value = fact.fields.get("name", fact.fields.get("value", fact.fields.get("template", fact.kind)))
        return f"{_FACT_LABELS[fact.kind]} · {value}"

    def _load_selected_source_if_user_config(self) -> None:
        if self.source_combo.currentData() == "user-config":
            self._load_selected_source()

    def show_raised(self) -> None:
        self.show()
        if self.isMinimized():
            self.showNormal()
        self.raise_()
        self.activateWindow()

    def prepare_for_quit(self) -> None:
        self._allow_close = True

    def closeEvent(self, event: QCloseEvent) -> None:  # noqa: N802 - Qt API
        if self._allow_close:
            event.accept()
            return
        self.hide()
        event.ignore()


__all__ = ["FactEditorDialog", "SettingsWindow", "ThemePreviewButton"]
