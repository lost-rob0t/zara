"""Native Qt widgets used by Zara's full conversation surface."""

from __future__ import annotations

import re
from typing import Optional

from PySide6.QtCore import Qt, Signal
from PySide6.QtGui import QKeyEvent
from PySide6.QtWidgets import (
    QApplication,
    QFrame,
    QHBoxLayout,
    QLabel,
    QPlainTextEdit,
    QPushButton,
    QTextBrowser,
    QVBoxLayout,
    QWidget,
)

from zara.desktop.conversation import MessageRecord, MessageRole, MessageStatus
from zara.desktop.theme import refresh_dynamic_style

_FENCE_RE = re.compile(r"```([^\n`]*)\n(.*?)```", re.DOTALL)


class ChatComposer(QPlainTextEdit):
    """Multiline composer: Enter submits, Shift+Enter inserts a newline."""

    submit_requested = Signal()

    def keyPressEvent(self, event: QKeyEvent) -> None:  # noqa: N802 - Qt API
        if event.key() in {Qt.Key.Key_Return, Qt.Key.Key_Enter}:
            if event.modifiers() & Qt.KeyboardModifier.ShiftModifier:
                super().keyPressEvent(event)
                return
            self.submit_requested.emit()
            event.accept()
            return
        super().keyPressEvent(event)


class MessageWidget(QFrame):
    """Render one message without requiring WebEngine or rebuilding its siblings."""

    def __init__(self, message: MessageRecord, parent: Optional[QWidget] = None) -> None:
        super().__init__(parent)
        self.setObjectName("zaraMessage")
        self.setFrameShape(QFrame.Shape.NoFrame)
        self._message = message
        self.code_copy_buttons: list[QPushButton] = []
        self.code_blocks: list[str] = []

        self.role_label = QLabel()
        self.role_label.setObjectName("zaraMessageRole")
        self.status_label = QLabel()
        self.status_label.setObjectName("zaraMessageStatus")
        self.error_label = QLabel()
        self.error_label.setObjectName("zaraMessageError")
        self.error_label.setWordWrap(True)

        header = QHBoxLayout()
        header.addWidget(self.role_label)
        header.addStretch(1)
        header.addWidget(self.status_label)

        self.content_widget = QWidget()
        self.content_layout = QVBoxLayout(self.content_widget)
        self.content_layout.setContentsMargins(0, 0, 0, 0)
        self.content_layout.setSpacing(6)

        layout = QVBoxLayout(self)
        layout.setContentsMargins(12, 9, 5, 11)
        layout.setSpacing(7)
        layout.addLayout(header)
        layout.addWidget(self.content_widget)
        layout.addWidget(self.error_label)

        self.set_message(message)

    @property
    def message_id(self) -> str:
        return self._message.id

    @property
    def message(self) -> MessageRecord:
        return self._message

    def set_message(self, message: MessageRecord) -> None:
        self._message = message
        self.setProperty("messageRole", message.role.value)
        self.status_label.setProperty("messageStatus", message.status.value)
        self.role_label.setText(self._role_text(message.role))
        self.status_label.setText(self._status_text(message.status))
        self.error_label.setText(message.error)
        self.error_label.setVisible(bool(message.error))
        self._render_content(message)
        refresh_dynamic_style(self)
        refresh_dynamic_style(self.status_label)

    def copy_code(self, index: int) -> None:
        QApplication.clipboard().setText(self.code_blocks[index])

    def _render_content(self, message: MessageRecord) -> None:
        while self.content_layout.count():
            item = self.content_layout.takeAt(0)
            widget = item.widget()
            if widget is not None:
                widget.deleteLater()
        self.code_copy_buttons.clear()
        self.code_blocks.clear()

        if message.role is MessageRole.ASSISTANT:
            self._render_markdown_with_code(message.content or "…")
            return

        text = QTextBrowser()
        text.setObjectName("zaraMessageBody")
        text.setOpenExternalLinks(True)
        text.setFrameShape(QFrame.Shape.NoFrame)
        text.viewport().setAutoFillBackground(False)
        if message.role is MessageRole.USER:
            text.setPlainText(message.content)
        else:
            text.setMarkdown(message.content)
        text.setMinimumHeight(36)
        text.setMaximumHeight(190)
        self.content_layout.addWidget(text)

    def _render_markdown_with_code(self, content: str) -> None:
        cursor = 0
        matches = list(_FENCE_RE.finditer(content))
        if not matches:
            self._add_markdown(content)
            return

        for match in matches:
            prefix = content[cursor : match.start()]
            if prefix:
                self._add_markdown(prefix)
            language = match.group(1).strip()
            code = match.group(2)
            self._add_code_block(language, code)
            cursor = match.end()
        suffix = content[cursor:]
        if suffix:
            self._add_markdown(suffix)

    def _add_markdown(self, markdown: str) -> None:
        view = QTextBrowser()
        view.setObjectName("zaraMessageBody")
        view.setOpenExternalLinks(True)
        view.setFrameShape(QFrame.Shape.NoFrame)
        view.viewport().setAutoFillBackground(False)
        view.setMarkdown(markdown)
        view.setMinimumHeight(36)
        view.setMaximumHeight(230)
        self.content_layout.addWidget(view)

    def _add_code_block(self, language: str, code: str) -> None:
        container = QFrame()
        container.setObjectName("zaraCodeBlock")
        container.setFrameShape(QFrame.Shape.NoFrame)
        layout = QVBoxLayout(container)
        layout.setContentsMargins(11, 10, 11, 11)
        layout.setSpacing(8)

        header = QHBoxLayout()
        header.addWidget(QLabel(language or "code"))
        header.addStretch(1)
        copy_button = QPushButton("Copy")
        copy_button.setObjectName("zaraSecondaryAction")
        index = len(self.code_blocks)
        copy_button.clicked.connect(lambda _checked=False, i=index: self.copy_code(i))
        header.addWidget(copy_button)

        editor = QPlainTextEdit()
        editor.setObjectName("zaraCodeEditor")
        editor.setReadOnly(True)
        editor.setPlainText(code)
        editor.setMinimumHeight(72)
        editor.setMaximumHeight(260)

        self.code_blocks.append(code)
        self.code_copy_buttons.append(copy_button)
        layout.addLayout(header)
        layout.addWidget(editor)
        self.content_layout.addWidget(container)

    @staticmethod
    def _role_text(role: MessageRole) -> str:
        return {
            MessageRole.USER: "You",
            MessageRole.ASSISTANT: "Zara",
            MessageRole.SYSTEM: "System",
            MessageRole.TOOL: "Tool",
        }[role]

    @staticmethod
    def _status_text(status: MessageStatus) -> str:
        return {
            MessageStatus.PENDING: "Pending",
            MessageStatus.STREAMING: "Generating…",
            MessageStatus.COMPLETE: "",
            MessageStatus.ERROR: "Error",
            MessageStatus.CANCELLED: "Cancelled",
        }[status]


__all__ = ["ChatComposer", "MessageWidget"]
