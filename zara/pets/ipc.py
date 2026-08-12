"""Cross-process IPC for pets using ZeroMQ PUB/SUB.

The pet overlay (``zara --pets``) and the runtime (``zara --wake``) run in
separate processes. A ZMQ PUB/SUB pair carries JSON event lines from the
runtime to the overlay.

Topology (canonical ZMQ PUB/SUB):
    runtime (wake)  --PUB bind------>   tcp://127.0.0.1:35621
    pet (overlay)   --SUB connect---->  tcp://127.0.0.1:35621

The runtime binds a PUB socket; the pet connects a SUB socket. When the
runtime exits/restarts, ZMQ auto-reconnects the subscriber. When no pet
is running, the PUB drops events silently (no blocking of the voice
loop).

Wire format: one JSON object per message frame:
    {"event": "ModelStarted", "label": "llm", ...}

The schema mirrors ``zara.pets.events``.
"""

from __future__ import annotations

import json
import logging
import os
from typing import Optional

logger = logging.getLogger(__name__)

DEFAULT_ENDPOINT = "tcp://127.0.0.1:35621"
ENV_ENDPOINT = "ZARA_PET_ENDPOINT"


def endpoint() -> str:
    return os.getenv(ENV_ENDPOINT, DEFAULT_ENDPOINT)


def encode_event(event_name: str, **kwargs) -> bytes:
    payload = {"event": event_name}
    payload.update(kwargs)
    return json.dumps(payload, sort_keys=True).encode("utf-8")


class PetPublisher:
    """PUB socket the runtime connects to forward events to the pet.

    Connects to the pet's SUB broker. If the pet isn't running, messages
    are dropped by ZMQ (PUB drops when no subscribers), so the voice
    loop is never blocked.
    """

    def __init__(self) -> None:
        self._ctx = None
        self._socket = None

    def start(self) -> None:
        try:
            import zmq
        except ImportError:
            logger.debug("[PetIPC] pyzmq unavailable; runtime->pet bridge disabled")
            return
        self._ctx = zmq.Context.instance()
        self._socket = self._ctx.socket(zmq.PUB)
        self._socket.setsockopt(zmq.LINGER, 0)
        try:
            self._socket.bind(endpoint())
        except Exception as exc:
            # Port already in use (another wake process) — connect instead.
            logger.debug("[PetIPC] bind failed (%s); connecting", exc)
            self._socket.connect(endpoint())
        logger.info("[PetIPC] publisher on %s", endpoint())

    def publish(self, event_name: str, **kwargs) -> None:
        if self._socket is None:
            return
        try:
            self._socket.send(encode_event(event_name, **kwargs))
        except Exception:
            logger.debug("[PetIPC] publish failed", exc_info=True)

    def stop(self) -> None:
        if self._socket is not None:
            try:
                self._socket.close(0)
            except Exception:
                pass
            self._socket = None


class PetSubscriber:
    """SUB socket the pet process binds to receive runtime events.

    Call ``start()`` to bind, then ``poll()`` from a Qt timer to drain
    messages and forward them via ``on_event``.
    """

    def __init__(self, on_event) -> None:
        self._on_event = on_event
        self._ctx = None
        self._socket = None

    def start(self) -> bool:
        try:
            import zmq
        except ImportError:
            logger.warning("[PetIPC] pyzmq unavailable; pet will not react to runtime events")
            return False
        self._ctx = zmq.Context.instance()
        self._socket = self._ctx.socket(zmq.SUB)
        self._socket.setsockopt(zmq.SUBSCRIBE, b"")
        self._socket.setsockopt(zmq.LINGER, 0)
        self._socket.setsockopt(zmq.RCVTIMEO, 0)
        try:
            self._socket.connect(endpoint())
        except Exception as exc:
            logger.error("[PetIPC] cannot connect %s: %s", endpoint(), exc)
            return False
        logger.info("[PetIPC] subscriber connected to %s", endpoint())
        return True

    def poll(self) -> None:
        """Non-blocking poll. Call from a Qt timer (~30 Hz is plenty)."""
        if self._socket is None:
            return
        while True:
            try:
                data = self._socket.recv(flags=__zmq_noblock__())
            except Exception:
                return
            if not data:
                return
            try:
                payload = json.loads(data.decode("utf-8"))
                self._on_event(payload)
            except (json.JSONDecodeError, UnicodeDecodeError) as exc:
                logger.warning("[PetIPC] bad event: %s (%s)", data[:64], exc)

    def stop(self) -> None:
        if self._socket is not None:
            try:
                self._socket.close(0)
            except Exception:
                pass
            self._socket = None


def __zmq_noblock__():
    """Return the ZMQ NOBLOCK flag lazily so the module imports without zmq."""
    import zmq
    return zmq.NOBLOCK