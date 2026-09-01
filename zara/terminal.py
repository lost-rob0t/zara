"""Unified terminal surface for Zara.

Both one-shot tasks and the interactive TUI use the same ZaraClient command/event
boundary. The terminal never constructs AgentManager, PrologEngine, or plugin
objects directly.
"""

from __future__ import annotations

import queue
import sys
import time
from typing import Optional

from zara.client import InProcessZaraClient, ZaraClient
from zara.runtime import events
from zara.runtime.commands import SubmitTurn


TURN_TIMEOUT_SECONDS = 30.0


class TerminalTurnError(RuntimeError):
    pass


def make_client(*, endpoint: Optional[str], config=None) -> ZaraClient:
    if endpoint:
        from zara.zmq_transport import ZmqZaraClient

        return ZmqZaraClient(endpoint)
    return InProcessZaraClient(config=config)


def wait_for_turn(
    subscription,
    turn_id: str,
    *,
    timeout: float = TURN_TIMEOUT_SECONDS,
) -> str:
    if not turn_id:
        raise TerminalTurnError("runtime did not assign a turn id")

    deadline = time.monotonic() + timeout
    while True:
        remaining = deadline - time.monotonic()
        if remaining <= 0:
            raise TimeoutError("Zara turn timed out")
        try:
            envelope = subscription.get(timeout=remaining)
        except queue.Empty as error:
            raise TimeoutError("Zara turn timed out") from error

        event = envelope.event
        if event.turn_id != turn_id:
            continue
        if isinstance(event, events.AssistantComplete):
            if not event.success:
                raise TerminalTurnError(event.text or "assistant generation failed")
            return event.text
        if isinstance(event, events.ResponseText):
            return event.text
        if isinstance(event, (events.AssistantFailed, events.AgentFailed)):
            raise TerminalTurnError(event.reason or "assistant turn failed")
        if isinstance(event, events.TurnCancelled):
            raise TerminalTurnError(event.reason or "assistant turn cancelled")


def run_task(
    text: str,
    *,
    endpoint: Optional[str] = None,
    config=None,
    timeout: float = TURN_TIMEOUT_SECONDS,
) -> int:
    client = None
    subscription = None
    exit_code = 0
    try:
        client = make_client(endpoint=endpoint, config=config)
        client.start().result(timeout=timeout)
        subscription = client.subscribe()
        receipt = client.submit(SubmitTurn(text=text)).result(timeout=timeout)
        response = wait_for_turn(subscription, receipt.turn_id or "", timeout=timeout)
        if response:
            print(response)
    except Exception as error:
        print(f"Error: {error}", file=sys.stderr)
        exit_code = 2
    finally:
        if subscription is not None:
            try:
                subscription.close()
            except Exception as error:
                if exit_code == 0:
                    print(f"Error: {error}", file=sys.stderr)
                    exit_code = 2
        if client is not None:
            try:
                client.close(timeout=timeout)
            except Exception as error:
                if exit_code == 0:
                    print(f"Error: {error}", file=sys.stderr)
                    exit_code = 2
    return exit_code


def run_tui(*, endpoint: Optional[str] = None, config=None) -> int:
    from zara.tui import ZaraTui

    client = None
    try:
        client = make_client(endpoint=endpoint, config=config)
        app = ZaraTui(client=client, endpoint=endpoint)
        app.run()
        return int(app.return_code)
    except Exception as error:
        print(f"Error: {error}", file=sys.stderr)
        if client is not None:
            try:
                client.close(timeout=5.0)
            except Exception:
                pass
        return 2


def tui_main() -> int:
    from zara.config import init_config

    return run_tui(config=init_config())


def console_main() -> int:
    """Compatibility entry point that delegates to the canonical CLI parser."""
    from zara import __main__ as cli

    return cli.run(["--console", *sys.argv[1:]])


__all__ = [
    "TerminalTurnError",
    "console_main",
    "make_client",
    "run_task",
    "run_tui",
    "tui_main",
    "wait_for_turn",
]
