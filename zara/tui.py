"""Textual terminal UI backed only by the ZaraClient boundary."""

from __future__ import annotations

import asyncio
from typing import Optional

from rich.markup import escape
from textual.app import App, ComposeResult
from textual.containers import Horizontal, Vertical
from textual.widgets import Footer, Header, Input, RichLog, Static

from zara.client import ZaraClient
from zara.runtime import events
from zara.runtime.commands import ApproveTool, CancelTurn, RejectTool, SubmitTurn


class ZaraTui(App):
    """Interactive terminal surface for the canonical Zara runtime."""

    TITLE = "Zara"
    SUB_TITLE = "agent runtime"

    CSS = """
    Screen {
        layout: vertical;
    }

    #body {
        height: 1fr;
    }

    #chat {
        width: 1fr;
        border: round $accent;
        padding: 0 1;
    }

    #activity {
        width: 34;
        border: round $surface-lighten-2;
        padding: 0 1;
    }

    #status {
        height: 1;
        padding: 0 1;
    }

    #prompt {
        dock: bottom;
        margin: 0 1 1 1;
    }
    """

    BINDINGS = [
        ("ctrl+c", "cancel_turn", "Cancel"),
        ("ctrl+l", "clear_chat", "Clear"),
        ("ctrl+d", "quit_app", "Quit"),
    ]

    def __init__(self, *, client: ZaraClient, endpoint: Optional[str] = None) -> None:
        super().__init__()
        self.client = client
        self.endpoint = endpoint
        self.subscription = None
        self.active_turn_id: Optional[str] = None
        self.pending_tool_run_id: Optional[str] = None
        self.rendered_turns: set[str] = set()
        self.return_code = 0
        self._closed = False

    def compose(self) -> ComposeResult:
        yield Header()
        with Horizontal(id="body"):
            yield RichLog(id="chat", wrap=True, markup=True, highlight=True)
            yield RichLog(id="activity", wrap=True, markup=True, highlight=True)
        yield Static("starting runtime…", id="status")
        yield Input(
            placeholder="Ask Zara anything — /help for terminal commands",
            id="prompt",
        )
        yield Footer()

    async def on_mount(self) -> None:
        try:
            await asyncio.wrap_future(self.client.start())
            self.subscription = self.client.subscribe()
        except Exception as error:
            self.return_code = 2
            self._chat_error(str(error))
            self._set_status("runtime unavailable")
            return

        mode = f"daemon {self.endpoint}" if self.endpoint else "standalone runtime"
        self.query_one("#activity", RichLog).write(f"[bold]connected[/] · {escape(mode)}")
        self._set_status(f"ready · {self.client.state.value}")
        self.set_interval(0.05, self._drain_events)
        self.query_one("#prompt", Input).focus()

    async def on_unmount(self) -> None:
        await self._close_client()

    async def on_input_submitted(self, message: Input.Submitted) -> None:
        text = message.value.strip()
        message.input.value = ""
        if not text:
            return
        if text.startswith("/"):
            await self._handle_terminal_command(text)
            return
        await self._submit_turn(text)

    async def _submit_turn(self, text: str) -> None:
        chat = self.query_one("#chat", RichLog)
        chat.write(f"[bold]You[/]\n{escape(text)}")
        try:
            receipt = await asyncio.wrap_future(self.client.submit(SubmitTurn(text=text)))
        except Exception as error:
            self._chat_error(str(error))
            self._set_status("submit failed")
            return

        self.active_turn_id = receipt.turn_id
        self._set_status(f"thinking · {receipt.turn_id or 'pending'}")

    async def _handle_terminal_command(self, raw: str) -> None:
        command, _, rest = raw.partition(" ")
        command = command.lower()
        rest = rest.strip()

        if command in {"/quit", "/exit"}:
            await self.action_quit_app()
            return
        if command == "/help":
            self.query_one("#activity", RichLog).write(
                "[bold]terminal[/]\n"
                "/status · /cancel · /approve [tool-id] · "
                "/reject [tool-id] [reason] · /clear · /quit"
            )
            return
        if command == "/status":
            self.query_one("#activity", RichLog).write(
                f"runtime={escape(self.client.state.value)} "
                f"turn={escape(self.active_turn_id or '-')} "
                f"approval={escape(self.pending_tool_run_id or '-')}"
            )
            return
        if command == "/clear":
            self.action_clear_chat()
            return
        if command == "/cancel":
            await self._cancel_active_turn()
            return
        if command == "/approve":
            tool_run_id = rest or self.pending_tool_run_id
            if not tool_run_id:
                self._activity_error("no pending tool approval")
                return
            await self._submit_runtime_command(ApproveTool(tool_run_id=tool_run_id))
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
                self._activity_error("no pending tool approval")
                return
            await self._submit_runtime_command(
                RejectTool(tool_run_id=tool_run_id, reason=reason)
            )
            return

        self._activity_error(f"unknown terminal command: {command}")

    async def _submit_runtime_command(self, command) -> None:
        try:
            receipt = await asyncio.wrap_future(self.client.submit(command))
        except Exception as error:
            self._activity_error(str(error))
            return
        self.query_one("#activity", RichLog).write(escape(receipt.detail or "accepted"))

    async def _cancel_active_turn(self) -> None:
        if not self.active_turn_id:
            self._activity_error("no active turn")
            return
        await self._submit_runtime_command(CancelTurn(turn_id=self.active_turn_id))

    def _drain_events(self) -> None:
        if self.subscription is None:
            return
        for envelope in self.subscription.drain(limit=100):
            self._handle_event(envelope.event)

    def _handle_event(self, event: events.RuntimeEvent) -> None:
        activity = self.query_one("#activity", RichLog)

        if isinstance(event, events.ResponseText):
            self._render_response(event.turn_id, event.text)
            return
        if isinstance(event, events.AssistantComplete):
            if event.success and event.text:
                self._render_response(event.turn_id, event.text)
            elif not event.success:
                self._chat_error(event.text or "assistant generation failed")
            return
        if isinstance(event, (events.AssistantFailed, events.AgentFailed)):
            self._chat_error(event.reason or "assistant turn failed")
            if event.turn_id == self.active_turn_id:
                self.active_turn_id = None
            self._set_status("ready")
            return
        if isinstance(event, events.AgentStarted):
            self._set_status(f"thinking · {event.turn_id or 'turn'}")
            return
        if isinstance(event, events.AgentCompleted):
            if event.turn_id == self.active_turn_id:
                self.active_turn_id = None
            self._set_status("ready")
            return
        if isinstance(event, events.TurnCancelled):
            activity.write(f"[yellow]cancelled[/] {escape(event.turn_id or '')}")
            if event.turn_id == self.active_turn_id:
                self.active_turn_id = None
            self._set_status("ready")
            return
        if isinstance(event, events.ToolWaitingForUser):
            self.pending_tool_run_id = event.tool_run_id
            tool_name = event.tool_name or "tool"
            prompt = event.prompt or "approval required"
            activity.write(
                f"[bold yellow]{escape(tool_name)} awaiting approval[/]\n"
                f"{escape(prompt)}\n"
                f"/approve {escape(event.tool_run_id or '')} · "
                f"/reject {escape(event.tool_run_id or '')} [reason]"
            )
            self._set_status(f"approval · {tool_name}")
            return
        if isinstance(event, events.ToolStarted):
            activity.write(f"[bold]tool[/] {escape(event.tool_name or 'unknown')} started")
            return
        if isinstance(event, events.ToolProgress):
            detail = event.message or "working"
            if event.progress is not None:
                detail = f"{int(event.progress * 100)}% · {detail}"
            activity.write(f"[bold]tool[/] {escape(event.tool_name or 'unknown')} · {escape(detail)}")
            return
        if isinstance(event, events.ToolCompleted):
            activity.write(
                f"[bold]tool[/] {escape(event.tool_name or 'unknown')} "
                f"{'completed' if event.success else 'failed'}"
            )
            if event.tool_run_id == self.pending_tool_run_id:
                self.pending_tool_run_id = None
            return
        if isinstance(event, (events.ToolFailed, events.ToolCancelled)):
            reason = event.reason or type(event).__name__
            activity.write(
                f"[red]tool {escape(event.tool_name or 'unknown')}[/] · {escape(reason)}"
            )
            if event.tool_run_id == self.pending_tool_run_id:
                self.pending_tool_run_id = None
            return
        if isinstance(event, events.IntentResolved):
            activity.write(
                f"[bold]intent[/] {escape(event.intent)} · {escape(event.resolver)}"
            )
            return
        if isinstance(event, events.PrologQueryCompleted):
            activity.write(
                f"[bold]prolog[/] {'ok' if event.success else 'failed'} · "
                f"{escape(event.summary)}"
            )
            return
        if isinstance(event, events.ProviderChanged):
            activity.write(
                f"[bold]provider[/] {escape(event.provider)} · {escape(event.model)}"
            )
            return
        if isinstance(event, events.RuntimeError):
            self._activity_error(event.reason or "runtime error")
            if event.fatal:
                self.return_code = 2
                self._set_status("runtime failed")
            return
        if isinstance(event, events.RuntimeStarted):
            self._set_status("ready")
            return
        if isinstance(event, events.RuntimeStopped):
            self._set_status("stopped")
            return
        if isinstance(event, events.VoiceStateChanged):
            activity.write(f"[bold]voice[/] {escape(event.state)} · {escape(event.detail)}")

    def _render_response(self, turn_id: Optional[str], text: str) -> None:
        key = turn_id or f"uncorrelated:{text}"
        if key in self.rendered_turns:
            return
        self.rendered_turns.add(key)
        self.query_one("#chat", RichLog).write(f"[bold]Zara[/]\n{escape(text)}")
        if turn_id == self.active_turn_id:
            self.active_turn_id = None
        self._set_status("ready")

    def _set_status(self, text: str) -> None:
        self.query_one("#status", Static).update(text)

    def _chat_error(self, text: str) -> None:
        self.query_one("#chat", RichLog).write(f"[bold red]Zara error[/]\n{escape(text)}")

    def _activity_error(self, text: str) -> None:
        self.query_one("#activity", RichLog).write(f"[red]error[/] · {escape(text)}")

    async def action_cancel_turn(self) -> None:
        await self._cancel_active_turn()

    def action_clear_chat(self) -> None:
        self.query_one("#chat", RichLog).clear()

    async def action_quit_app(self) -> None:
        await self._close_client()
        self.exit()

    async def _close_client(self) -> None:
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
            await asyncio.to_thread(self.client.close)
        except Exception as error:
            self.return_code = 2
            self._activity_error(str(error))


__all__ = ["ZaraTui"]
