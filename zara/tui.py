"""Curses terminal UI backed only by the ZaraClient boundary."""

from __future__ import annotations

import curses
import textwrap
from dataclasses import dataclass
from typing import Optional

from zara.client import ZaraClient
from zara.runtime import events
from zara.runtime.commands import ApproveTool, CancelTurn, RejectTool, SubmitTurn


@dataclass(frozen=True)
class _Line:
    speaker: str
    text: str


class ZaraTui:
    """Interactive terminal surface for Zara's canonical runtime."""

    def __init__(self, *, client: ZaraClient, endpoint: Optional[str] = None) -> None:
        self.client = client
        self.endpoint = endpoint
        self.subscription = None
        self.active_turn_id: Optional[str] = None
        self.pending_tool_run_id: Optional[str] = None
        self.rendered_turns: set[str] = set()
        self.return_code = 0
        self.chat: list[_Line] = []
        self.activity: list[str] = []
        self.status = "starting runtime"
        self.input_buffer = ""
        self._running = True
        self._closed = False

    def run(self) -> None:
        try:
            self.client.start().result(timeout=30.0)
            self.subscription = self.client.subscribe()
        except Exception as error:
            self.return_code = 2
            raise RuntimeError(f"failed to start Zara runtime: {error}") from error

        mode = f"daemon {self.endpoint}" if self.endpoint else "standalone runtime"
        self.activity.append(f"connected · {mode}")
        self.status = f"ready · {self.client.state.value}"

        try:
            curses.wrapper(self._main)
        finally:
            self._close_client()

    def _main(self, screen) -> None:
        curses.curs_set(1)
        screen.keypad(True)
        screen.timeout(50)

        while self._running:
            self._drain_events()
            self._draw(screen)
            key = screen.getch()
            if key == -1:
                continue
            self._handle_key(key)

    def _handle_key(self, key: int) -> None:
        if key in (4,):
            self._running = False
            return
        if key in (3,):
            self._cancel_active_turn()
            return
        if key in (12,):
            self.chat.clear()
            return
        if key in (curses.KEY_BACKSPACE, 127, 8):
            self.input_buffer = self.input_buffer[:-1]
            return
        if key in (curses.KEY_ENTER, 10, 13):
            text = self.input_buffer.strip()
            self.input_buffer = ""
            if text:
                self._submit_input(text)
            return
        if key == curses.KEY_RESIZE:
            return
        if 32 <= key <= 126:
            self.input_buffer += chr(key)

    def _submit_input(self, text: str) -> None:
        if text.startswith("/"):
            self._handle_terminal_command(text)
            return

        self.chat.append(_Line("You", text))
        try:
            receipt = self.client.submit(SubmitTurn(text=text)).result(timeout=5.0)
        except Exception as error:
            self.chat.append(_Line("Error", str(error)))
            self.status = "submit failed"
            return

        self.active_turn_id = receipt.turn_id
        self.status = f"thinking · {receipt.turn_id or 'pending'}"

    def _handle_terminal_command(self, raw: str) -> None:
        command, _, rest = raw.partition(" ")
        command = command.lower()
        rest = rest.strip()

        if command in {"/quit", "/exit"}:
            self._running = False
            return
        if command == "/help":
            self.activity.append(
                "/status · /cancel · /approve [tool-id] · "
                "/reject [tool-id] [reason] · /clear · /quit"
            )
            return
        if command == "/status":
            self.activity.append(
                f"runtime={self.client.state.value} "
                f"turn={self.active_turn_id or '-'} "
                f"approval={self.pending_tool_run_id or '-'}"
            )
            return
        if command == "/clear":
            self.chat.clear()
            return
        if command == "/cancel":
            self._cancel_active_turn()
            return
        if command == "/approve":
            tool_run_id = rest or self.pending_tool_run_id
            if not tool_run_id:
                self.activity.append("error · no pending tool approval")
                return
            self._submit_runtime_command(ApproveTool(tool_run_id=tool_run_id))
            return
        if command == "/reject":
            tool_run_id = self.pending_tool_run_id
            reason = ""
            if rest:
                first, separator, tail = rest.partition(" ")
                if tool_run_id is None or first == tool_run_id:
                    tool_run_id = first
                    reason = tail.strip() if separator else ""
                else:
                    reason = rest
            if not tool_run_id:
                self.activity.append("error · no pending tool approval")
                return
            self._submit_runtime_command(
                RejectTool(tool_run_id=tool_run_id, reason=reason)
            )
            return

        self.activity.append(f"error · unknown terminal command: {command}")

    def _submit_runtime_command(self, command) -> None:
        try:
            receipt = self.client.submit(command).result(timeout=5.0)
        except Exception as error:
            self.activity.append(f"error · {error}")
            return
        self.activity.append(receipt.detail or "accepted")

    def _cancel_active_turn(self) -> None:
        if not self.active_turn_id:
            self.activity.append("error · no active turn")
            return
        self._submit_runtime_command(CancelTurn(turn_id=self.active_turn_id))

    def _drain_events(self) -> None:
        if self.subscription is None:
            return
        for envelope in self.subscription.drain(limit=100):
            self._handle_event(envelope.event)

    def _handle_event(self, event: events.RuntimeEvent) -> None:
        if isinstance(event, events.ResponseText):
            self._render_response(event.turn_id, event.text)
            return
        if isinstance(event, events.AssistantComplete):
            if event.success and event.text:
                self._render_response(event.turn_id, event.text)
            elif not event.success:
                self.chat.append(_Line("Error", event.text or "assistant generation failed"))
            return
        if isinstance(event, (events.AssistantFailed, events.AgentFailed)):
            self.chat.append(_Line("Error", event.reason or "assistant turn failed"))
            if event.turn_id == self.active_turn_id:
                self.active_turn_id = None
            self.status = "ready"
            return
        if isinstance(event, events.AgentStarted):
            self.status = f"thinking · {event.turn_id or 'turn'}"
            return
        if isinstance(event, events.AgentCompleted):
            if event.turn_id == self.active_turn_id:
                self.active_turn_id = None
            self.status = "ready"
            return
        if isinstance(event, events.TurnCancelled):
            self.activity.append(f"cancelled · {event.turn_id or ''}")
            if event.turn_id == self.active_turn_id:
                self.active_turn_id = None
            self.status = "ready"
            return
        if isinstance(event, events.ToolWaitingForUser):
            self.pending_tool_run_id = event.tool_run_id
            tool_name = event.tool_name or "tool"
            prompt = event.prompt or "approval required"
            self.activity.extend(
                [
                    f"{tool_name} awaiting approval",
                    prompt,
                    f"/approve {event.tool_run_id or ''}",
                    f"/reject {event.tool_run_id or ''} [reason]",
                ]
            )
            self.status = f"approval · {tool_name}"
            return
        if isinstance(event, events.ToolStarted):
            self.activity.append(f"tool · {event.tool_name or 'unknown'} started")
            return
        if isinstance(event, events.ToolProgress):
            detail = event.message or "working"
            if event.progress is not None:
                detail = f"{int(event.progress * 100)}% · {detail}"
            self.activity.append(f"tool · {event.tool_name or 'unknown'} · {detail}")
            return
        if isinstance(event, events.ToolCompleted):
            result = "completed" if event.success else "failed"
            self.activity.append(f"tool · {event.tool_name or 'unknown'} · {result}")
            if event.tool_run_id == self.pending_tool_run_id:
                self.pending_tool_run_id = None
            return
        if isinstance(event, (events.ToolFailed, events.ToolCancelled)):
            self.activity.append(
                f"tool · {event.tool_name or 'unknown'} · "
                f"{event.reason or type(event).__name__}"
            )
            if event.tool_run_id == self.pending_tool_run_id:
                self.pending_tool_run_id = None
            return
        if isinstance(event, events.IntentResolved):
            self.activity.append(f"intent · {event.intent} · {event.resolver}")
            return
        if isinstance(event, events.PrologQueryCompleted):
            result = "ok" if event.success else "failed"
            self.activity.append(f"prolog · {result} · {event.summary}")
            return
        if isinstance(event, events.ProviderChanged):
            self.activity.append(f"provider · {event.provider} · {event.model}")
            return
        if isinstance(event, events.RuntimeError):
            self.activity.append(f"runtime error · {event.reason or 'unknown error'}")
            if event.fatal:
                self.return_code = 2
                self.status = "runtime failed"
            return
        if isinstance(event, events.RuntimeStarted):
            self.status = "ready"
            return
        if isinstance(event, events.RuntimeStopped):
            self.status = "stopped"
            return
        if isinstance(event, events.VoiceStateChanged):
            self.activity.append(f"voice · {event.state} · {event.detail}")

    def _render_response(self, turn_id: Optional[str], text: str) -> None:
        key = turn_id or f"uncorrelated:{text}"
        if key in self.rendered_turns:
            return
        self.rendered_turns.add(key)
        self.chat.append(_Line("Zara", text))
        if turn_id == self.active_turn_id:
            self.active_turn_id = None
        self.status = "ready"

    def _draw(self, screen) -> None:
        screen.erase()
        height, width = screen.getmaxyx()
        if height < 8 or width < 50:
            self._safe_addstr(screen, 0, 0, "Zara TUI needs at least 50x8")
            screen.refresh()
            return

        header_height = 1
        footer_height = 3
        body_top = header_height
        body_height = height - header_height - footer_height
        activity_width = max(24, min(38, width // 3))
        chat_width = width - activity_width - 1

        self._safe_addstr(screen, 0, 0, " Zara ", curses.A_REVERSE)
        mode = "daemon" if self.endpoint else "local"
        right_header = f" {mode} · {self.client.state.value} "
        self._safe_addstr(
            screen,
            0,
            max(0, width - len(right_header)),
            right_header,
            curses.A_REVERSE,
        )

        self._draw_chat(screen, body_top, body_height, chat_width)
        self._draw_separator(screen, body_top, body_height, chat_width)
        self._draw_activity(
            screen,
            body_top,
            body_height,
            chat_width + 1,
            activity_width - 1,
        )

        status_row = height - 3
        prompt_row = height - 2
        help_row = height - 1
        self._safe_addstr(screen, status_row, 0, self.status[: width - 1], curses.A_DIM)
        prompt = f"> {self.input_buffer}"
        self._safe_addstr(screen, prompt_row, 0, prompt[: width - 1])
        help_text = "Ctrl-C cancel · Ctrl-L clear · Ctrl-D quit · /help"
        self._safe_addstr(screen, help_row, 0, help_text[: width - 1], curses.A_DIM)

        cursor_x = min(width - 1, 2 + len(self.input_buffer))
        try:
            screen.move(prompt_row, cursor_x)
        except curses.error:
            pass
        screen.refresh()

    def _draw_chat(self, screen, top: int, height: int, width: int) -> None:
        lines: list[str] = []
        content_width = max(1, width - 2)
        for item in self.chat:
            lines.append(f"{item.speaker}:")
            wrapped = textwrap.wrap(
                item.text,
                width=content_width,
                replace_whitespace=False,
                drop_whitespace=True,
            ) or [""]
            lines.extend(wrapped)
            lines.append("")
        visible = lines[-height:]
        for index, line in enumerate(visible):
            self._safe_addstr(screen, top + index, 1, line[:content_width])

    def _draw_separator(self, screen, top: int, height: int, column: int) -> None:
        for row in range(top, top + height):
            self._safe_addstr(screen, row, column, "│", curses.A_DIM)

    def _draw_activity(
        self,
        screen,
        top: int,
        height: int,
        left: int,
        width: int,
    ) -> None:
        content_width = max(1, width - 2)
        lines = ["Activity", ""]
        for item in self.activity:
            lines.extend(textwrap.wrap(item, width=content_width) or [""])
        visible = lines[-height:]
        for index, line in enumerate(visible):
            attr = curses.A_BOLD if line == "Activity" else curses.A_NORMAL
            self._safe_addstr(screen, top + index, left + 1, line[:content_width], attr)

    @staticmethod
    def _safe_addstr(screen, row: int, column: int, text: str, attr: int = 0) -> None:
        if not text:
            return
        try:
            screen.addstr(row, column, text, attr)
        except curses.error:
            pass

    def _close_client(self) -> None:
        if self._closed:
            return
        self._closed = True
        subscription = self.subscription
        self.subscription = None
        if subscription is not None:
            try:
                subscription.close()
            except Exception:
                self.return_code = 2
        try:
            self.client.close(timeout=5.0)
        except Exception:
            self.return_code = 2


__all__ = ["ZaraTui"]
