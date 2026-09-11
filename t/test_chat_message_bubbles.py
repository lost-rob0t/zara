from __future__ import annotations

import os

os.environ.setdefault("QT_QPA_PLATFORM", "offscreen")

from PySide6.QtWidgets import QApplication

from zara.desktop.chat_widgets import MessageWidget
from zara.desktop.conversation import MessageRecord, MessageRole, MessageStatus


def app() -> QApplication:
    instance = QApplication.instance()
    assert instance is None or isinstance(instance, QApplication)
    return instance or QApplication([])


def message(role: MessageRole) -> MessageRecord:
    return MessageRecord(
        id=f"message-{role.value}",
        conversation_id="conversation-1",
        sequence=1,
        role=role,
        content="Bubble me.",
        status=MessageStatus.COMPLETE,
        created_at="2026-09-10T00:00:00Z",
        updated_at="2026-09-10T00:00:00Z",
    )


def test_message_widget_uses_rounded_surfaces_instead_of_dividers():
    qt_app = app()
    widget = MessageWidget(message(MessageRole.USER))

    try:
        qt_app.processEvents()
        stylesheet = widget.styleSheet()

        assert "border-radius: 14px" in stylesheet
        assert "border: none" in stylesheet
        assert "border-top" not in stylesheet
        assert 'QFrame#zaraMessage[messageRole="user"]' in stylesheet
        assert 'QFrame#zaraMessage[messageRole="assistant"]' in stylesheet
        assert "background: palette(button)" in stylesheet
        assert "background: palette(alternate-base)" in stylesheet

        margins = widget.layout().contentsMargins()
        assert margins.left() == margins.right() == 14
        assert margins.top() == 10
        assert margins.bottom() == 12
    finally:
        widget.deleteLater()
        qt_app.processEvents()
