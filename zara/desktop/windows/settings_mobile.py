"""Android-parity projection for the canonical Desktop Settings window.

This module deliberately reuses the existing SettingsWindow, SettingsDocument,
ZaraConfig, pairing, Prolog, and plugin config authorities.  It changes only
presentation/navigation semantics; no second settings store or runtime owner is
introduced.
"""

from __future__ import annotations

from typing import Any

from PySide6.QtCore import Qt
from PySide6.QtWidgets import (
    QFormLayout,
    QFrame,
    QHBoxLayout,
    QLabel,
    QLineEdit,
    QScrollArea,
    QSplitter,
    QVBoxLayout,
    QWidget,
)

from .settings import SettingsWindow as CanonicalSettingsWindow


_CATEGORIES = (
    "Runtime",
    "Connection",
    "Permissions",
    "Appearance",
    "Plugins",
    "Updates",
    "Diagnostics",
    "About",
)

_CATEGORY_SEARCH_TERMS = {
    "Runtime": "assistant model provider voice speech wake stt tts history agent",
    "Connection": "remote server endpoint pair pairing trust connection api",
    "Permissions": "tools privacy memory calculator prolog files telemetry",
    "Appearance": "theme color outrun starintel midnight terminal light system",
    "Plugins": "plugin lifecycle workers queue extensions",
    "Updates": "update version release package channel",
    "Diagnostics": "diagnostics prolog logic source facts knowledge config debug",
    "About": "about advanced database config toml paths source",
}

_SETTING_META = {
    "desktop.theme": ("Choose the semantic theme used by Zara desktop surfaces.", "live"),
    "llm.provider": ("Default provider backend when provider-backed execution is selected.", "restart"),
    "llm.model": ("Model identifier used by the selected provider.", "restart"),
    "llm.endpoint": ("Optional provider/API endpoint; Zara node pairing remains separately owned.", "restart"),
    "llm.history_limit": ("Maximum conversation messages retained in provider context.", "restart"),
    "agent.max_steps": ("Maximum bounded tool/agent steps for one turn.", "restart"),
    "agent.system_prompt": ("System instructions for provider-backed agent execution.", "restart"),
    "wake.threshold": ("Wake detector confidence threshold.", "restart"),
    "wake.silence_duration": ("Trailing silence needed to finish a wake utterance.", "restart"),
    "wake.acknowledgement.enabled": ("Play an acknowledgement after wake detection.", "restart"),
    "wake.acknowledgement.voice": ("Voice used for wake acknowledgement.", "restart"),
    "stt.provider": ("Speech-recognition backend.", "restart"),
    "stt.model": ("Speech-recognition model.", "restart"),
    "stt.device": ("Device used by local speech recognition.", "restart"),
    "tts.provider": ("Voice output provider.", "restart"),
    "tools.calculator": ("Allow the bounded calculator tool.", "restart"),
    "tools.get_current_time": ("Allow Zara to read the current time.", "restart"),
    "tools.query_prolog": ("Allow read-only symbolic Prolog queries.", "restart"),
    "tools.remember": ("Allow writes through Zara's canonical memory boundary.", "restart"),
    "tools.recall": ("Allow reads from Zara's canonical memory boundary.", "restart"),
    "tools.file_tools": ("Allow file tools through normal capability/approval boundaries.", "restart"),
    "memory.enabled": ("Enable Zara's long-term memory subsystem.", "restart"),
    "latency.enabled": ("Record bounded latency metrics.", "restart"),
    "database.path": ("Canonical Zara database path.", "restart"),
    "prolog.main_file": ("Primary Prolog entry source.", "restart"),
    "prolog.load_on_startup": ("Load the configured Prolog source at startup.", "restart"),
    "plugins.lifecycle_timeout": ("Timeout for plugin lifecycle transitions.", "restart"),
    "plugins.event_queue_size": ("Bounded plugin event queue size.", "restart"),
    "plugins.max_managed_workers": ("Maximum managed plugin workers.", "restart"),
}


class SettingsWindow(CanonicalSettingsWindow):
    """Canonical settings owner with Android-parity navigation and row semantics."""

    def __init__(self, *args: Any, **kwargs: Any) -> None:
        super().__init__(*args, **kwargs)
        self._install_mobile_parity_shell()

    def _page(self, title: str, description: str) -> tuple[QWidget, QFormLayout]:
        body = QWidget()
        body_layout = QVBoxLayout(body)
        body_layout.setContentsMargins(30, 24, 34, 30)
        body_layout.setSpacing(9)

        breadcrumb = QLabel(f"SETTINGS / {title.upper()}")
        breadcrumb.setObjectName("zaraSettingsBreadcrumb")
        title_label = QLabel(title)
        title_label.setObjectName("zaraSectionTitle")
        description_label = QLabel(description)
        description_label.setObjectName("zaraSectionDescription")
        description_label.setWordWrap(True)
        body_layout.addWidget(breadcrumb)
        body_layout.addWidget(title_label)
        body_layout.addWidget(description_label)
        body_layout.addSpacing(8)

        card = QFrame()
        card.setObjectName("zaraSettingsSectionCard")
        card_layout = QVBoxLayout(card)
        card_layout.setContentsMargins(18, 16, 18, 18)
        form = QFormLayout()
        form.setHorizontalSpacing(28)
        form.setVerticalSpacing(14)
        form.setFieldGrowthPolicy(QFormLayout.FieldGrowthPolicy.AllNonFixedFieldsGrow)
        form.setLabelAlignment(Qt.AlignmentFlag.AlignLeft | Qt.AlignmentFlag.AlignTop)
        card_layout.addLayout(form)
        body_layout.addWidget(card)
        body_layout.addStretch(1)

        scroll = QScrollArea()
        scroll.setFrameShape(QFrame.Shape.NoFrame)
        scroll.setWidgetResizable(True)
        scroll.setWidget(body)
        return scroll, form

    def _setting_label(self, key: str, label: str) -> QWidget:
        description, apply_mode = _SETTING_META.get(
            key,
            ("Stored in Zara's canonical config.toml.", "restart"),
        )
        shell = QWidget()
        shell.setObjectName("zaraSettingLabel")
        layout = QVBoxLayout(shell)
        layout.setContentsMargins(0, 1, 0, 1)
        layout.setSpacing(3)

        header = QHBoxLayout()
        header.setContentsMargins(0, 0, 0, 0)
        title = QLabel(label)
        title.setObjectName("zaraSettingName")
        badge = QLabel("LIVE" if apply_mode == "live" else "RESTART")
        badge.setObjectName("zaraSettingBadge")
        badge.setProperty("applyMode", apply_mode)
        header.addWidget(title)
        header.addSpacing(8)
        header.addWidget(badge)
        header.addStretch(1)

        detail = QLabel(description)
        detail.setObjectName("zaraSettingDescription")
        detail.setWordWrap(True)
        layout.addLayout(header)
        layout.addWidget(detail)
        return shell

    def _register(self, form: QFormLayout, key: str, label: str, widget: QWidget) -> QWidget:
        description, apply_mode = _SETTING_META.get(
            key,
            ("Stored in Zara's canonical config.toml.", "restart"),
        )
        widget.setProperty("zaraApplyMode", apply_mode)
        widget.setToolTip(description)
        self.setting_widgets[key] = widget
        form.addRow(self._setting_label(key, label), widget)
        return widget

    def _install_mobile_parity_shell(self) -> None:
        # Base SettingsWindow already constructed every canonical control and the
        # live pairing/Prolog flows.  Reparent those pages instead of rebuilding
        # their authorities.
        pages = [self.stack.widget(index) for index in range(self.stack.count())]
        if len(pages) != 7:
            raise RuntimeError(f"unexpected canonical settings page count: {len(pages)}")
        appearance, assistant, connections, voice, permissions, diagnostics, about = pages
        while self.stack.count():
            self.stack.removeWidget(self.stack.widget(0))

        runtime = QWidget()
        runtime.setObjectName("zaraRuntimeSettingsPage")
        runtime_layout = QVBoxLayout(runtime)
        runtime_layout.setContentsMargins(0, 0, 0, 0)
        runtime_splitter = QSplitter(Qt.Orientation.Vertical)
        runtime_splitter.setObjectName("zaraRuntimeSettingsSplitter")
        runtime_splitter.addWidget(assistant)
        runtime_splitter.addWidget(voice)
        runtime_splitter.setSizes([430, 330])
        runtime_layout.addWidget(runtime_splitter)

        plugins = self._plugins_page()
        updates = self._updates_page()
        for page in (
            runtime,
            connections,
            permissions,
            appearance,
            plugins,
            updates,
            diagnostics,
            about,
        ):
            self.stack.addWidget(page)
        self.stack.setObjectName("zaraSettingsStack")

        self.category_list.blockSignals(True)
        self.category_list.clear()
        for category in _CATEGORIES:
            self.category_list.addItem(category)
        self.category_list.setFixedWidth(220)
        self.category_list.blockSignals(False)

        rail = self.category_list.parentWidget()
        if rail is None or rail.layout() is None:
            raise RuntimeError("settings navigation rail is unavailable")
        rail_layout = rail.layout()
        brand = self.findChild(QLabel, "zaraBrandName")
        if brand is not None:
            rail_layout.removeWidget(brand)
            brand.deleteLater()

        self.settings_title = QLabel("Settings")
        self.settings_title.setObjectName("zaraSettingsTitle")
        self.settings_subtitle = QLabel("Desktop · same Zara settings language as Android")
        self.settings_subtitle.setObjectName("zaraSettingsSubtitle")
        self.settings_subtitle.setWordWrap(True)
        self.settings_search = QLineEdit()
        self.settings_search.setObjectName("zaraSettingsSearch")
        self.settings_search.setPlaceholderText("Search settings")
        self.settings_search.setClearButtonEnabled(True)
        rail_layout.insertWidget(0, self.settings_title)
        rail_layout.insertWidget(1, self.settings_subtitle)
        rail_layout.insertWidget(2, self.settings_search)
        self.settings_search.textChanged.connect(self._filter_categories)

        self.category_list.setCurrentRow(0)
        self.stack.setCurrentIndex(0)

    def _filter_categories(self, text: str) -> None:
        query = str(text).strip().lower()
        first_visible = -1
        current_visible = False
        for index, category in enumerate(_CATEGORIES):
            item = self.category_list.item(index)
            haystack = f"{category} {_CATEGORY_SEARCH_TERMS.get(category, '')}".lower()
            visible = not query or query in haystack
            item.setHidden(not visible)
            if visible and first_visible < 0:
                first_visible = index
            if visible and self.category_list.currentRow() == index:
                current_visible = True
        if not current_visible and first_visible >= 0:
            self.category_list.setCurrentRow(first_visible)

    def _plugins_page(self) -> QWidget:
        page, form = self._page(
            "Plugins",
            "Bound lifecycle and queue settings for the existing plugin runtime; install and capability authority remain canonical.",
        )
        self._double_setting(
            form,
            "plugins.lifecycle_timeout",
            "Lifecycle timeout",
            5.0,
            0.1,
            120.0,
            0.5,
        )
        self._spin_setting(
            form,
            "plugins.event_queue_size",
            "Event queue",
            256,
            1,
            4096,
        )
        self._spin_setting(
            form,
            "plugins.max_managed_workers",
            "Managed workers",
            8,
            1,
            128,
        )
        return page

    def _updates_page(self) -> QWidget:
        page, form = self._page(
            "Updates",
            "Desktop updates follow the installed Zara package/release channel; Settings does not create another updater.",
        )
        note = QLabel(
            "Use the canonical package/update path for this installation. "
            "Android APK update authority remains Android-owned."
        )
        note.setObjectName("zaraSettingsNotice")
        note.setWordWrap(True)
        form.addRow("Update authority", note)
        return page
