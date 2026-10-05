"""Native Qt widgets used by Zara's full conversation surface."""

from __future__ import annotations

import math
import re
from typing import Optional

from PySide6.QtCore import QSize, Qt, QTimer, Signal
from PySide6.QtGui import QFontMetrics, QKeyEvent, QResizeEvent
from PySide6.QtWidgets import (
    QApplication,
    QFrame,
    QHBoxLayout,
    QLabel,
    QPlainTextEdit,
    QPushButton,
    QSizePolicy,
    QStyle,
    QTextBrowser,
    QVBoxLayout,
    QWidget,
)

from zara.desktop.conversation import MessageRecord, MessageRole, MessageStatus
from zara.desktop.theme import refresh_dynamic_style

_FENCE_RE = re.compile(r"```([^\n`]*)\n(.*?)```", re.DOTALL)


class _MessageBody(QTextBrowser):
    """Rich message text that follows document height instead of filling the viewport."""

    def __init__(self, *, maximum_height: int = 280) -> None:
        super().__init__()
        self._maximum_body_height = maximum_height
        self._fit_timer = QTimer(self)
        self._fit_timer.setSingleShot(True)
        self._fit_timer.timeout.connect(self._fit_document)
        self.setObjectName("zaraMessageBody")
        self.setOpenExternalLinks(True)
        self.setFrameShape(QFrame.Shape.NoFrame)
        self.viewport().setAutoFillBackground(False)
        self.viewport().setStyleSheet("background: transparent;")
        self.setSizePolicy(QSizePolicy.Policy.Expanding, QSizePolicy.Policy.Fixed)
        self.document().documentLayout().documentSizeChanged.connect(self._fit_document)

    def resizeEvent(self, event: QResizeEvent) -> None:  # noqa: N802 - Qt API
        super().resizeEvent(event)
        self._fit_timer.start(0)

    def _fit_document(self) -> None:
        viewport_width = self.viewport().width()
        if viewport_width > 0:
            self.document().setTextWidth(viewport_width)
        target = math.ceil(self.document().size().height()) + self.frameWidth() * 2 + 4
        self.setFixedHeight(max(32, min(self._maximum_body_height, target)))


class ChatComposer(QPlainTextEdit):
    """Multiline composer: Enter submits, Shift+Enter inserts a newline."""

    submit_requested = Signal()

    def __init__(self, parent: Optional[QWidget] = None) -> None:
        super().__init__(parent)
        self._preferred_height = 32
        self._minimum_composer_height = 32
        self.setSizePolicy(QSizePolicy.Policy.Expanding, QSizePolicy.Policy.Fixed)
        self.document().documentLayout().documentSizeChanged.connect(self._fit_document)
        self.textChanged.connect(self._fit_document)
        QTimer.singleShot(0, self._fit_document)

    def sizeHint(self) -> QSize:  # noqa: N802 - Qt API
        hint = super().sizeHint()
        hint.setHeight(self._preferred_height)
        return hint

    def minimumSizeHint(self) -> QSize:  # noqa: N802 - Qt API
        hint = super().minimumSizeHint()
        hint.setHeight(self.minimumHeight())
        return hint

    def keyPressEvent(self, event: QKeyEvent) -> None:  # noqa: N802 - Qt API
        if event.key() in {Qt.Key.Key_Return, Qt.Key.Key_Enter}:
            if event.modifiers() & Qt.KeyboardModifier.ShiftModifier:
                super().keyPressEvent(event)
                return
            self.submit_requested.emit()
            event.accept()
            return
        super().keyPressEvent(event)

    def _fit_document(self) -> None:
        line_count = max(1, self.document().blockCount())
        text_height = line_count * self.fontMetrics().lineSpacing()
        target = max(self._minimum_composer_height, text_height + 12)
        preferred_height = min(self.maximumHeight(), target)
        if preferred_height == self._preferred_height:
            return
        self._preferred_height = preferred_height
        self.setMinimumHeight(preferred_height)
        self.resize(self.width(), preferred_height)
        self.updateGeometry()


class ComposerActionButton(QPushButton):
    """One familiar composer control that becomes Stop during generation."""

    def __init__(self, parent: Optional[QWidget] = None) -> None:
        super().__init__(parent)
        self.setObjectName("zaraComposerAction")
        self.setIconSize(QSize(17, 17))
        self.set_action_mode("send")

    @property
    def action_mode(self) -> str:
        return str(self.property("actionMode"))

    def set_action_mode(self, mode: str) -> None:
        if mode not in {"send", "stop"}:
            raise ValueError(f"unsupported composer action mode: {mode}")
        self.setProperty("actionMode", mode)
        if mode == "stop":
            icon = self.style().standardIcon(QStyle.StandardPixmap.SP_MediaStop)
            label = "Stop generating"
        else:
            icon = self.style().standardIcon(QStyle.StandardPixmap.SP_ArrowUp)
            label = "Send message"
        self.setIcon(icon)
        self.setToolTip(label)
        self.setAccessibleName(label)
        refresh_dynamic_style(self)


class MessageWidget(QFrame):
    """Render one message without requiring WebEngine or rebuilding its siblings."""

    def __init__(self, message: MessageRecord, parent: Optional[QWidget] = None) -> None:
        super().__init__(parent)
        self.setObjectName("zaraMessage")
        self.setFrameShape(QFrame.Shape.NoFrame)
        self._message = message
        self.code_copy_buttons: list[QPushButton] = []
        self.code_blocks: list[str] = []
        self.body_text = ""

        self.setSizePolicy(QSizePolicy.Policy.Expanding, QSizePolicy.Policy.Fixed)

        self.bubble = QFrame()
        self.bubble.setObjectName("zaraMessageBubble")
        self.bubble.setFrameShape(QFrame.Shape.NoFrame)
        self.bubble.setSizePolicy(QSizePolicy.Policy.Fixed, QSizePolicy.Policy.Fixed)

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
        self.content_widget.setObjectName("zaraMessageContent")
        self.content_layout = QVBoxLayout(self.content_widget)
        self.content_layout.setContentsMargins(0, 0, 0, 0)
        self.content_layout.setSpacing(6)

        bubble_layout = QVBoxLayout(self.bubble)
        bubble_layout.setContentsMargins(14, 10, 14, 11)
        bubble_layout.setSpacing(7)
        bubble_layout.addLayout(header)
        bubble_layout.addWidget(self.content_widget)
        bubble_layout.addWidget(self.error_label)

        layout = QHBoxLayout(self)
        layout.setContentsMargins(4, 2, 4, 2)
        layout.setSpacing(0)
        layout.addWidget(self.bubble)

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
        message_kind = "activity" if message.role is MessageRole.TOOL else "chat"
        self.setProperty("messageKind", message_kind)
        self.bubble.setProperty("messageRole", message.role.value)
        self.bubble.setProperty("messageKind", message_kind)
        self.status_label.setProperty("messageStatus", message.status.value)
        role_text, body_text = self._presentation_text(message)
        self.body_text = body_text
        self.role_label.setText(role_text)
        self.role_label.setVisible(message.role is not MessageRole.USER)
        self.status_label.setText(self._status_text(message.role, message.status))
        self.status_label.setVisible(bool(self.status_label.text()))
        self.error_label.setText(message.error)
        self.error_label.setVisible(bool(message.error))
        self._render_content(message, body_text)
        self.setAccessibleName(f"{role_text or 'You'} message")
        self.layout().setAlignment(
            self.bubble,
            Qt.AlignmentFlag.AlignRight
            if message.role is MessageRole.USER
            else Qt.AlignmentFlag.AlignLeft,
        )
        self._fit_bubble_width()
        refresh_dynamic_style(self)
        refresh_dynamic_style(self.bubble)
        refresh_dynamic_style(self.status_label)

    def copy_code(self, index: int) -> None:
        QApplication.clipboard().setText(self.code_blocks[index])

    def resizeEvent(self, event: QResizeEvent) -> None:  # noqa: N802 - Qt API
        super().resizeEvent(event)
        self._fit_bubble_width()

    def _render_content(self, message: MessageRecord, content: str) -> None:
        while self.content_layout.count():
            item = self.content_layout.takeAt(0)
            widget = item.widget()
            if widget is not None:
                widget.setParent(None)
                widget.deleteLater()
        self.code_copy_buttons.clear()
        self.code_blocks.clear()
        self.content_widget.show()

        if message.role is MessageRole.TOOL and content.lower() in {
            "queued",
            "running",
            "waiting for approval",
            "completed",
            "failed",
            "cancelled",
        }:
            self.content_widget.hide()
            return

        if message.role is MessageRole.ASSISTANT:
            self._render_markdown_with_code(content or "...")
            return

        text = _MessageBody(maximum_height=220)
        if message.role is MessageRole.USER:
            text.setPlainText(content)
        else:
            text.setMarkdown(content)
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
        view = _MessageBody()
        view.setMarkdown(markdown)
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

    def _fit_bubble_width(self) -> None:
        available = max(220, self.width() - 8)
        ratio = 0.72 if self._message.role is MessageRole.USER else 0.84
        if self._message.role in {MessageRole.SYSTEM, MessageRole.TOOL}:
            ratio = 0.76
        maximum = max(180, int(available * ratio))
        metrics = QFontMetrics(self.font())
        longest_line = max(self.body_text.splitlines() or [""], key=len)
        body_width = metrics.horizontalAdvance(longest_line) + 48
        header_width = (
            metrics.horizontalAdvance(self.role_label.text())
            + metrics.horizontalAdvance(self.status_label.text())
            + 68
        )
        desired = max(body_width, header_width)
        self.bubble.setFixedWidth(max(150, min(maximum, desired)))

    @staticmethod
    def _presentation_text(message: MessageRecord) -> tuple[str, str]:
        if message.role is MessageRole.TOOL:
            name, separator, detail = message.content.partition(":")
            if separator and name.strip():
                return name.strip(), detail.strip() or "Working"
            return "Activity", message.content
        return {
            MessageRole.USER: ("You", message.content),
            MessageRole.ASSISTANT: ("Zara", message.content),
            MessageRole.SYSTEM: ("System", message.content),
        }[message.role]

    @staticmethod
    def _status_text(role: MessageRole, status: MessageStatus) -> str:
        if role is MessageRole.TOOL:
            return {
                MessageStatus.PENDING: "Waiting",
                MessageStatus.STREAMING: "Running",
                MessageStatus.COMPLETE: "Done",
                MessageStatus.ERROR: "Failed",
                MessageStatus.CANCELLED: "Cancelled",
            }[status]
        return {
            MessageStatus.PENDING: "Pending",
            MessageStatus.STREAMING: "Generating...",
            MessageStatus.COMPLETE: "",
            MessageStatus.ERROR: "Error",
            MessageStatus.CANCELLED: "Cancelled",
        }[status]


__all__ = ["ChatComposer", "ComposerActionButton", "MessageWidget"]
