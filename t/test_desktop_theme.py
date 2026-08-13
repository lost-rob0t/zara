from __future__ import annotations

import os

os.environ.setdefault("QT_QPA_PLATFORM", "offscreen")

from PySide6.QtGui import QColor, QPalette
from PySide6.QtWidgets import QApplication, QTextBrowser

import zara.desktop.app as desktop_app
from zara.desktop.chat_widgets import ChatComposer, MessageWidget
from zara.desktop.conversation import MessageRecord, MessageRole, MessageStatus
from zara.desktop.theme import MIN_TEXT_CONTRAST, apply_readable_palette, contrast_ratio, repair_palette

_GROUPS = (
    QPalette.ColorGroup.Active,
    QPalette.ColorGroup.Inactive,
    QPalette.ColorGroup.Disabled,
)

_PAIRS = (
    (QPalette.ColorRole.WindowText, QPalette.ColorRole.Window),
    (QPalette.ColorRole.Text, QPalette.ColorRole.Base),
    (QPalette.ColorRole.ButtonText, QPalette.ColorRole.Button),
    (QPalette.ColorRole.PlaceholderText, QPalette.ColorRole.Base),
    (QPalette.ColorRole.HighlightedText, QPalette.ColorRole.Highlight),
    (QPalette.ColorRole.ToolTipText, QPalette.ColorRole.ToolTipBase),
)


def app() -> QApplication:
    instance = QApplication.instance()
    assert instance is None or isinstance(instance, QApplication)
    return instance or QApplication([])


def set_pair(
    palette: QPalette,
    foreground_role: QPalette.ColorRole,
    background_role: QPalette.ColorRole,
    foreground: str,
    background: str,
) -> None:
    for group in _GROUPS:
        palette.setColor(group, foreground_role, QColor(foreground))
        palette.setColor(group, background_role, QColor(background))


def assert_readable(palette: QPalette) -> None:
    for group in _GROUPS:
        for foreground_role, background_role in _PAIRS:
            ratio = contrast_ratio(
                palette.color(group, foreground_role),
                palette.color(group, background_role),
            )
            assert ratio >= MIN_TEXT_CONTRAST


def test_readable_palette_is_preserved():
    source = QPalette()
    for foreground_role, background_role in _PAIRS:
        set_pair(source, foreground_role, background_role, "black", "white")

    repaired = repair_palette(source)

    for group in _GROUPS:
        for foreground_role, background_role in _PAIRS:
            assert repaired.color(group, foreground_role) == source.color(group, foreground_role)
            assert repaired.color(group, background_role) == source.color(group, background_role)


def test_white_on_white_foregrounds_are_repaired():
    source = QPalette()
    for foreground_role, background_role in _PAIRS:
        set_pair(source, foreground_role, background_role, "white", "white")

    repaired = repair_palette(source)

    assert_readable(repaired)
    assert repaired.color(QPalette.ColorGroup.Active, QPalette.ColorRole.WindowText) == QColor("black")
    assert repaired.color(QPalette.ColorGroup.Active, QPalette.ColorRole.Text) == QColor("black")
    assert repaired.color(QPalette.ColorGroup.Active, QPalette.ColorRole.ButtonText) == QColor("black")


def test_black_on_black_foregrounds_are_repaired():
    source = QPalette()
    for foreground_role, background_role in _PAIRS:
        set_pair(source, foreground_role, background_role, "black", "black")

    repaired = repair_palette(source)

    assert_readable(repaired)
    assert repaired.color(QPalette.ColorGroup.Active, QPalette.ColorRole.WindowText) == QColor("white")
    assert repaired.color(QPalette.ColorGroup.Active, QPalette.ColorRole.Text) == QColor("white")
    assert repaired.color(QPalette.ColorGroup.Active, QPalette.ColorRole.ButtonText) == QColor("white")


def test_palette_repair_is_idempotent():
    source = QPalette()
    for foreground_role, background_role in _PAIRS:
        set_pair(source, foreground_role, background_role, "white", "white")

    once = repair_palette(source)
    twice = repair_palette(once)

    for group in _GROUPS:
        for foreground_role, background_role in _PAIRS:
            assert twice.color(group, foreground_role) == once.color(group, foreground_role)
            assert twice.color(group, background_role) == once.color(group, background_role)


def test_chat_widgets_inherit_repaired_text_and_base_colors():
    qt_app = app()
    original = QPalette(qt_app.palette())
    broken = QPalette(original)
    set_pair(broken, QPalette.ColorRole.WindowText, QPalette.ColorRole.Window, "white", "white")
    set_pair(broken, QPalette.ColorRole.Text, QPalette.ColorRole.Base, "white", "white")
    qt_app.setPalette(broken)
    apply_readable_palette(qt_app)

    composer = ChatComposer()
    message = MessageWidget(
        MessageRecord(
            id="message-1",
            conversation_id="conversation-1",
            sequence=1,
            role=MessageRole.ASSISTANT,
            content="Hello — I’m Zarathustra.",
            status=MessageStatus.COMPLETE,
            created_at="2026-08-13T00:00:00Z",
            updated_at="2026-08-13T00:00:00Z",
        )
    )
    browser = message.findChild(QTextBrowser)

    try:
        composer_palette = composer.palette()
        assert (
            contrast_ratio(
                composer_palette.color(QPalette.ColorRole.Text),
                composer_palette.color(QPalette.ColorRole.Base),
            )
            >= MIN_TEXT_CONTRAST
        )
        assert browser is not None
        browser_palette = browser.palette()
        assert (
            contrast_ratio(
                browser_palette.color(QPalette.ColorRole.Text),
                browser_palette.color(QPalette.ColorRole.Base),
            )
            >= MIN_TEXT_CONTRAST
        )
        message_palette = message.role_label.palette()
        assert (
            contrast_ratio(
                message_palette.color(QPalette.ColorRole.WindowText),
                message_palette.color(QPalette.ColorRole.Window),
            )
            >= MIN_TEXT_CONTRAST
        )
    finally:
        composer.deleteLater()
        message.deleteLater()
        qt_app.setPalette(original)
        qt_app.processEvents()


def test_create_application_repairs_palette_before_controller_construction(monkeypatch):
    qt_app = app()
    controller_attr = "_zara_desktop_controller"
    if hasattr(qt_app, controller_attr):
        delattr(qt_app, controller_attr)

    original = QPalette(qt_app.palette())
    broken = QPalette(original)
    set_pair(broken, QPalette.ColorRole.WindowText, QPalette.ColorRole.Window, "white", "white")
    set_pair(broken, QPalette.ColorRole.Text, QPalette.ColorRole.Base, "white", "white")
    set_pair(broken, QPalette.ColorRole.ButtonText, QPalette.ColorRole.Button, "white", "white")
    qt_app.setPalette(broken)
    seen: dict[str, float] = {}

    class FakeBridge:
        def __init__(self, host, parent=None) -> None:
            self.host = host
            self.parent = parent

    class FakeController:
        def __init__(self, qt_application, host, bridge) -> None:
            palette = qt_application.palette()
            seen["window"] = contrast_ratio(
                palette.color(QPalette.ColorRole.WindowText),
                palette.color(QPalette.ColorRole.Window),
            )
            seen["text"] = contrast_ratio(
                palette.color(QPalette.ColorRole.Text),
                palette.color(QPalette.ColorRole.Base),
            )
            seen["button"] = contrast_ratio(
                palette.color(QPalette.ColorRole.ButtonText),
                palette.color(QPalette.ColorRole.Button),
            )
            self.host = host
            self.bridge = bridge

    monkeypatch.setattr(desktop_app, "QtRuntimeBridge", FakeBridge)
    monkeypatch.setattr(desktop_app, "DesktopController", FakeController)

    try:
        result_app, _controller = desktop_app.create_application([], host=object())
        assert result_app is qt_app
        assert seen["window"] >= MIN_TEXT_CONTRAST
        assert seen["text"] >= MIN_TEXT_CONTRAST
        assert seen["button"] >= MIN_TEXT_CONTRAST
    finally:
        if hasattr(qt_app, controller_attr):
            delattr(qt_app, controller_attr)
        qt_app.setPalette(original)
