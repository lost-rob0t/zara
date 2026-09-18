"""Short-lived QR bootstrap for enrolling Zara Android clients.

The QR contains only public server identity, the reachable ZARA/1 endpoint, and
one ephemeral bearer token. Long-term client secrets are generated on Android
and never leave the device; the daemon's long-term secret never leaves its
owner-private security state.
"""

from __future__ import annotations

import argparse
import hashlib
import hmac
import json
import os
import queue
import secrets
import shutil
import socket
import subprocess
import threading
import time
from dataclasses import dataclass, field
from pathlib import Path
from typing import Callable, Optional
from urllib.parse import parse_qs, urlencode, urlparse

from zara.security import SecurityRegistry

_PROTOCOL_VERSION = 1
_MAX_MESSAGE_BYTES = 4096
_DEFAULT_TIMEOUT_SECONDS = 120
_WILDCARD_HOSTS = {"", "0.0.0.0", "::", "*"}
_LOOPBACK_HOSTS = {"localhost", "127.0.0.1", "::1"}
_CLIENT_KINDS = {"android", "desktop"}


class PairingError(RuntimeError):
    pass


class PairingExpired(PairingError):
    pass


@dataclass
class PairingClaim:
    public_key: str
    device_id: str
    verification_code: str
    _connection: socket.socket = field(repr=False)
    _decision: queue.Queue[tuple[str, str | None]] = field(
        default_factory=lambda: queue.Queue(maxsize=1),
        repr=False,
    )


@dataclass(frozen=True)
class PairingResult:
    approved: bool
    code: str


def derive_device_id(public_key: str, client_kind: str = "android") -> str:
    normalized = SecurityRegistry._normalize_public_key(public_key)
    normalized_kind = str(client_kind).strip().lower()
    if normalized_kind not in _CLIENT_KINDS:
        raise ValueError("unsupported pairing client kind")
    digest = hashlib.sha256(normalized.encode("ascii")).hexdigest()
    return f"{normalized_kind}-{digest[:12]}"


def verification_code(token: str, public_key: str) -> str:
    if not isinstance(token, str) or not token:
        raise ValueError("pairing token must not be empty")
    normalized = SecurityRegistry._normalize_public_key(public_key)
    digest = hmac.new(
        token.encode("utf-8"),
        normalized.encode("ascii"),
        hashlib.sha256,
    ).digest()
    value = int.from_bytes(digest[:4], "big") % 1_000_000
    return f"{value:06d}"


def _encode_message(payload: object) -> bytes:
    encoded = (json.dumps(payload, sort_keys=True, separators=(",", ":")) + "\n").encode(
        "utf-8"
    )
    if len(encoded) > _MAX_MESSAGE_BYTES:
        raise PairingError("pairing message exceeds byte limit")
    return encoded


def _recv_message(connection: socket.socket) -> object:
    raw = bytearray()
    while True:
        block = connection.recv(min(1024, _MAX_MESSAGE_BYTES + 1 - len(raw)))
        if not block:
            raise PairingError("pairing connection closed before message completion")
        raw.extend(block)
        if len(raw) > _MAX_MESSAGE_BYTES:
            raise PairingError("pairing message exceeds byte limit")
        newline = raw.find(b"\n")
        if newline >= 0:
            if bytes(raw[newline + 1 :]).strip():
                raise PairingError("pairing accepts one request per connection")
            payload = bytes(raw[:newline])
            break
    try:
        value = json.loads(payload.decode("utf-8"))
    except (UnicodeError, ValueError, RecursionError) as error:
        raise PairingError("pairing request is invalid JSON") from error
    return value


def _safe_send(connection: socket.socket, payload: object) -> None:
    try:
        connection.sendall(_encode_message(payload))
    except (OSError, PairingError):
        pass


class PairingBroker:
    """Single-use pairing actor with an explicit operator approval boundary."""

    def __init__(
        self,
        *,
        endpoint: str,
        server_public_key: str,
        token: str,
        expires_at: int,
        bind_host: str = "0.0.0.0",
        bind_port: int = 0,
        enroll: Optional[Callable[[str, str], object]] = None,
    ) -> None:
        self.endpoint = _validate_tcp_endpoint(endpoint)
        self.server_public_key = SecurityRegistry._normalize_public_key(server_public_key)
        if not isinstance(token, str) or len(token.encode("utf-8")) < 8:
            raise ValueError("pairing token is too short")
        self.token = token
        self.expires_at = int(expires_at)
        self._enroll = enroll or (lambda _public_key, _device_id: None)
        self._claims: queue.Queue[PairingClaim] = queue.Queue(maxsize=1)
        self._result: queue.Queue[PairingResult] = queue.Queue(maxsize=1)
        self._stop = threading.Event()
        self._claimed = threading.Event()
        self._listener = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
        self._listener.setsockopt(socket.SOL_SOCKET, socket.SO_REUSEADDR, 1)
        self._listener.bind((bind_host, int(bind_port)))
        self._listener.listen(8)
        self._listener.settimeout(0.1)
        self._thread = threading.Thread(
            target=self._run,
            name="zara-pairing-broker",
            daemon=True,
        )
        self._thread.start()

    @property
    def address(self) -> tuple[str, int]:
        host, port = self._listener.getsockname()[:2]
        return str(host), int(port)

    def pairing_uri(self, *, advertise_host: str) -> str:
        host = str(advertise_host).strip()
        if not host or any(character.isspace() for character in host):
            raise ValueError("advertised pairing host is invalid")
        query = urlencode(
            {
                "broker_host": host,
                "broker_port": str(self.address[1]),
                "endpoint": self.endpoint,
                "server_key": self.server_public_key,
                "token": self.token,
                "expires": str(self.expires_at),
            }
        )
        return f"zara://pair/v1?{query}"

    def wait_for_claim(self) -> PairingClaim:
        remaining = self.expires_at - time.time()
        if remaining <= 0:
            raise PairingExpired("pairing session expired")
        try:
            return self._claims.get(timeout=remaining)
        except queue.Empty as error:
            raise PairingExpired("pairing session expired") from error

    def approve(self, claim: PairingClaim) -> None:
        self._decide(claim, "approve", None)

    def reject(self, claim: PairingClaim, *, code: str = "operator_rejected") -> None:
        normalized = str(code).strip()
        if not normalized or len(normalized) > 64:
            raise ValueError("pairing rejection code is invalid")
        self._decide(claim, "reject", normalized)

    def wait_for_result(self, timeout: float = 5.0) -> PairingResult:
        try:
            return self._result.get(timeout=max(0.0, float(timeout)))
        except queue.Empty as error:
            raise PairingError("pairing broker did not finish") from error

    def close(self) -> None:
        self._stop.set()
        try:
            self._listener.close()
        except OSError:
            pass
        self._thread.join(timeout=2.0)

    def _decide(self, claim: PairingClaim, action: str, code: str | None) -> None:
        if not self._claimed.is_set():
            raise PairingError("pairing claim is not active")
        try:
            claim._decision.put_nowait((action, code))
        except queue.Full as error:
            raise PairingError("pairing claim was already decided") from error

    def _run(self) -> None:
        while not self._stop.is_set() and time.time() < self.expires_at:
            try:
                connection, _address = self._listener.accept()
            except socket.timeout:
                continue
            except OSError:
                break
            if self._handle_connection(connection):
                return
        if not self._claimed.is_set():
            self._put_result(PairingResult(False, "expired"))

    def _handle_connection(self, connection: socket.socket) -> bool:
        connection.settimeout(max(0.1, min(5.0, self.expires_at - time.time())))
        try:
            request = _recv_message(connection)
            if not isinstance(request, dict):
                raise PairingError("pairing request must be an object")
            if set(request) != {"version", "token", "public_key", "client_kind"}:
                raise PairingError("pairing request has invalid fields")
            if request.get("version") != _PROTOCOL_VERSION:
                _safe_send(connection, {"status": "rejected", "code": "unsupported_version"})
                connection.close()
                return False
            token = request.get("token")
            if not isinstance(token, str) or not hmac.compare_digest(token, self.token):
                _safe_send(connection, {"status": "rejected", "code": "invalid_token"})
                connection.close()
                return False
            public_key = request.get("public_key")
            if not isinstance(public_key, str):
                raise PairingError("pairing public key is invalid")
            client_kind = request.get("client_kind")
            if not isinstance(client_kind, str) or client_kind not in _CLIENT_KINDS:
                raise PairingError("pairing client kind is invalid")
            normalized_key = SecurityRegistry._normalize_public_key(public_key)
        except (OSError, PairingError, TypeError, ValueError):
            _safe_send(connection, {"status": "rejected", "code": "invalid_payload"})
            connection.close()
            return False

        device_id = derive_device_id(normalized_key, client_kind)
        code = verification_code(self.token, normalized_key)
        claim = PairingClaim(
            public_key=normalized_key,
            device_id=device_id,
            verification_code=code,
            _connection=connection,
        )
        self._claimed.set()
        _safe_send(
            connection,
            {
                "status": "pending",
                "verification_code": code,
                "device_id": device_id,
            },
        )
        self._claims.put(claim)
        self._finish_claim(claim)
        return True

    def _finish_claim(self, claim: PairingClaim) -> None:
        deadline = float(self.expires_at)
        decision: tuple[str, str | None] | None = None
        while not self._stop.is_set() and time.time() < deadline:
            try:
                decision = claim._decision.get(timeout=min(0.1, max(0.01, deadline - time.time())))
                break
            except queue.Empty:
                continue

        if decision is None:
            _safe_send(claim._connection, {"status": "rejected", "code": "expired"})
            claim._connection.close()
            self._put_result(PairingResult(False, "expired"))
            return

        action, rejection_code = decision
        if action != "approve":
            code = rejection_code or "operator_rejected"
            _safe_send(claim._connection, {"status": "rejected", "code": code})
            claim._connection.close()
            self._put_result(PairingResult(False, code))
            return

        try:
            self._enroll(claim.public_key, claim.device_id)
        except Exception:
            _safe_send(claim._connection, {"status": "rejected", "code": "enrollment_failed"})
            claim._connection.close()
            self._put_result(PairingResult(False, "enrollment_failed"))
            return

        _safe_send(
            claim._connection,
            {
                "status": "approved",
                "endpoint": self.endpoint,
                "server_key": self.server_public_key,
                "device_id": claim.device_id,
            },
        )
        claim._connection.close()
        self._put_result(PairingResult(True, "ok"))

    def _put_result(self, result: PairingResult) -> None:
        try:
            self._result.put_nowait(result)
        except queue.Full:
            pass


@dataclass(frozen=True)
class PairingClientProgress:
    verification_code: str
    device_id: str


@dataclass(frozen=True)
class PairingClientOutcome:
    endpoint: str
    device_id: str


@dataclass(frozen=True)
class PairingPayload:
    broker_host: str
    broker_port: int
    endpoint: str
    server_key: str
    token: str
    expires_at: int


def parse_pairing_uri(raw: str, *, now: int | None = None) -> PairingPayload:
    parsed = urlparse(str(raw).strip())
    if parsed.scheme.lower() != "zara" or parsed.netloc.lower() != "pair" or parsed.path != "/v1":
        raise PairingError("invalid Zara pairing URI")
    if parsed.fragment or parsed.username or parsed.password:
        raise PairingError("pairing URI has invalid extras")
    try:
        query = parse_qs(parsed.query, strict_parsing=True, keep_blank_values=True)
    except ValueError as error:
        raise PairingError("pairing URI query is malformed") from error
    required = {"broker_host", "broker_port", "endpoint", "server_key", "token", "expires"}
    if set(query) != required or any(len(values) != 1 for values in query.values()):
        raise PairingError("pairing URI fields are invalid")
    broker_host = query["broker_host"][0].strip()
    if not broker_host or len(broker_host) > 255 or any(ch.isspace() for ch in broker_host):
        raise PairingError("pairing broker host is invalid")
    try:
        broker_port = int(query["broker_port"][0])
    except ValueError as error:
        raise PairingError("pairing broker port is invalid") from error
    if not 1 <= broker_port <= 65535:
        raise PairingError("pairing broker port is invalid")
    endpoint = _validate_tcp_endpoint(query["endpoint"][0])
    server_key = SecurityRegistry._normalize_public_key(query["server_key"][0])
    token = query["token"][0]
    if not 8 <= len(token) <= 512 or any(ord(ch) < 0x20 for ch in token):
        raise PairingError("pairing token is invalid")
    try:
        expires_at = int(query["expires"][0])
    except ValueError as error:
        raise PairingError("pairing expiry is invalid") from error
    current = int(time.time()) if now is None else int(now)
    if expires_at <= current:
        raise PairingExpired("pairing URI has expired")
    if expires_at - current > 600:
        raise PairingError("pairing URI lifetime is invalid")
    return PairingPayload(
        broker_host=broker_host,
        broker_port=broker_port,
        endpoint=endpoint,
        server_key=server_key,
        token=token,
        expires_at=expires_at,
    )


def _read_client_message(stream) -> dict[str, object]:
    raw = stream.readline(_MAX_MESSAGE_BYTES + 2)
    if not raw or not raw.endswith(b"\n") or len(raw) > _MAX_MESSAGE_BYTES + 1:
        raise PairingError("pairing server response is invalid")
    try:
        payload = json.loads(raw.decode("utf-8"))
    except (UnicodeError, ValueError, RecursionError) as error:
        raise PairingError("pairing server returned invalid JSON") from error
    if not isinstance(payload, dict):
        raise PairingError("pairing server response must be an object")
    return payload


def pair_client(
    raw_uri: str,
    *,
    store=None,
    config=None,
    on_progress: Callable[[PairingClientProgress], None] | None = None,
) -> PairingClientOutcome:
    from zara.client_enrollment import ClientEnrollmentStore
    from zara.config import get_config

    payload = parse_pairing_uri(raw_uri)
    active_store = store or ClientEnrollmentStore.for_config(config or get_config())
    public_key, _secret_key = active_store.identity_or_create()
    expected_device_id = derive_device_id(public_key, "desktop")
    expected_code = verification_code(payload.token, public_key)
    timeout = max(1.0, min(600.0, payload.expires_at - time.time()))
    try:
        connection = socket.create_connection(
            (payload.broker_host, payload.broker_port),
            timeout=min(5.0, timeout),
        )
    except OSError as error:
        raise PairingError("could not reach pairing broker") from error

    try:
        connection.settimeout(timeout)
        stream = connection.makefile("rwb", buffering=0)
        try:
            request = {
                "version": _PROTOCOL_VERSION,
                "token": payload.token,
                "public_key": public_key,
                "client_kind": "desktop",
            }
            stream.write(_encode_message(request))
            pending = _read_client_message(stream)
            if pending.get("status") != "pending":
                raise PairingError(f"pairing rejected: {pending.get('code', 'protocol_failure')}")
            if set(pending) != {"status", "verification_code", "device_id"}:
                raise PairingError("pairing pending response has invalid fields")
            server_code = pending.get("verification_code")
            server_device_id = pending.get("device_id")
            if server_code != expected_code or server_device_id != expected_device_id:
                raise PairingError("pairing verification response did not match this desktop")
            if on_progress is not None:
                on_progress(
                    PairingClientProgress(
                        verification_code=expected_code,
                        device_id=expected_device_id,
                    )
                )
            final = _read_client_message(stream)
            if final.get("status") != "approved":
                raise PairingError(f"pairing rejected: {final.get('code', 'protocol_failure')}")
            if set(final) != {"status", "endpoint", "server_key", "device_id"}:
                raise PairingError("pairing approval response has invalid fields")
            if (
                final.get("endpoint") != payload.endpoint
                or final.get("server_key") != payload.server_key
                or final.get("device_id") != expected_device_id
            ):
                raise PairingError("pairing approval did not match the selected server")
            active_store.complete_pairing(
                endpoint=payload.endpoint,
                server_public_key=payload.server_key,
            )
            return PairingClientOutcome(payload.endpoint, expected_device_id)
        finally:
            stream.close()
    except OSError as error:
        raise PairingError("pairing connection failed") from error
    finally:
        connection.close()


def pair_client_main(argv: Optional[list[str]] = None, *, config=None) -> int:
    parser = argparse.ArgumentParser(
        prog="zara pair-client",
        description="Enroll this desktop Zara client with a server pairing URI.",
    )
    parser.add_argument("uri", help="zara://pair/v1 URI from the server")
    args = parser.parse_args(argv)

    try:
        outcome = pair_client(
            args.uri,
            config=config,
            on_progress=lambda progress: print(
                f"Verification code: {progress.verification_code}\n"
                f"Device: {progress.device_id}\n"
                "Confirm the same code on the server."
            ),
        )
    except (OSError, PairingError, TypeError, ValueError) as error:
        print(f"Pairing failed: {error}", file=os.sys.stderr)
        return 2
    print(f"Paired {outcome.device_id}. Zara will now use {outcome.endpoint}.")
    return 0


def _validate_tcp_endpoint(endpoint: str) -> str:
    value = str(endpoint).strip()
    parsed = urlparse(value)
    if parsed.scheme != "tcp" or not parsed.hostname or parsed.port is None:
        raise ValueError("pairing requires a tcp://HOST:PORT Zara endpoint")
    if parsed.username is not None or parsed.password is not None:
        raise ValueError("pairing endpoint must not contain user information")
    if parsed.path not in ("", "/") or parsed.query or parsed.fragment:
        raise ValueError("pairing endpoint must be a bare tcp://HOST:PORT origin")
    return value.rstrip("/")


def _endpoint_host(endpoint: str) -> str:
    parsed = urlparse(endpoint)
    host = parsed.hostname or ""
    if host in _WILDCARD_HOSTS or host in _LOOPBACK_HOSTS:
        return ""
    return host


def _endpoint_with_host(endpoint: str, host: str) -> str:
    parsed = urlparse(endpoint)
    port = parsed.port
    if port is None:
        raise ValueError("pairing endpoint has no port")
    formatted = f"[{host}]" if ":" in host and not host.startswith("[") else host
    return f"tcp://{formatted}:{port}"


def _local_route_address() -> str:
    probe = socket.socket(socket.AF_INET, socket.SOCK_DGRAM)
    try:
        probe.connect(("192.0.2.1", 9))
        address = str(probe.getsockname()[0])
    except OSError as error:
        raise PairingError("could not determine a reachable local address; use --advertise-host") from error
    finally:
        probe.close()
    if address in _LOOPBACK_HOSTS or address in _WILDCARD_HOSTS:
        raise PairingError("could not determine a reachable local address; use --advertise-host")
    return address


def _security_directory(config, explicit: str | None) -> Path:
    if explicit:
        return Path(explicit).expanduser()
    environment = os.getenv("ZARA_SECURITY_DIR", "").strip()
    if environment:
        return Path(environment).expanduser()
    if config is not None:
        section = config.get_section("daemon") or {}
        if isinstance(section, dict):
            configured = str(section.get("security_dir") or "").strip()
            if configured:
                return Path(configured).expanduser()
    state_home = os.getenv("XDG_STATE_HOME", "").strip()
    base = Path(state_home).expanduser() if state_home else Path.home() / ".local" / "state"
    return base / "zarathushtra" / "security"


def _render_qr(uri: str) -> bool:
    executable = shutil.which("qrencode")
    if executable is None:
        return False
    subprocess.run(
        [executable, "-t", "ANSIUTF8", "-m", "1"],
        input=uri.encode("utf-8"),
        check=True,
    )
    return True


def _fingerprint(public_key: str) -> str:
    return hashlib.sha256(public_key.encode("ascii")).hexdigest()[:16]


def main(argv: Optional[list[str]] = None, *, config=None) -> int:
    parser = argparse.ArgumentParser(
        prog="zara pair",
        description="Pair one Zara client with the running secure Zara daemon.",
    )
    parser.add_argument("--endpoint", help="Reachable tcp:// Zara endpoint to store on Android")
    parser.add_argument("--security-dir", help="Override Zara's owner-private security directory")
    parser.add_argument("--advertise-host", help="Host/IP the Android device can reach")
    parser.add_argument("--bind-host", default="0.0.0.0", help=argparse.SUPPRESS)
    parser.add_argument("--timeout", type=int, default=_DEFAULT_TIMEOUT_SECONDS)
    args = parser.parse_args(argv)

    if args.timeout < 15 or args.timeout > 600:
        parser.error("--timeout must be between 15 and 600 seconds")

    from zara.daemon_client import resolve_daemon_endpoint
    from zara.security_admin import SecurityAdminClient, SecurityAdminError
    from zara.security_state import PersistentSecurityState, SecurityStateError

    try:
        endpoint = _validate_tcp_endpoint(
            resolve_daemon_endpoint(config, explicit=args.endpoint)
        )
        advertise_host = str(args.advertise_host or _endpoint_host(endpoint)).strip()
        if not advertise_host:
            advertise_host = _local_route_address()
        advertised_endpoint = _endpoint_with_host(endpoint, advertise_host)

        state = PersistentSecurityState(_security_directory(config, args.security_dir))
        server_public_key = state.server_public_key()
        admin = SecurityAdminClient(state.control_socket_path)
        admin.request("list")
    except (OSError, PairingError, SecurityAdminError, SecurityStateError, TypeError, ValueError) as error:
        print(f"Pairing unavailable: {error}", file=os.sys.stderr)
        return 2

    token = secrets.token_urlsafe(32)
    expires_at = int(time.time()) + args.timeout

    def enroll(public_key: str, device_id: str) -> object:
        return admin.request("enroll", public_key=public_key, device_id=device_id)

    try:
        broker = PairingBroker(
            endpoint=advertised_endpoint,
            server_public_key=server_public_key,
            token=token,
            expires_at=expires_at,
            bind_host=args.bind_host,
            enroll=enroll,
        )
    except (OSError, TypeError, ValueError) as error:
        print(f"Pairing unavailable: {error}", file=os.sys.stderr)
        return 2

    try:
        uri = broker.pairing_uri(advertise_host=advertise_host)
        print("Scan this with Zara Android, or paste the URI into Zara Desktop:")
        print()
        if not _render_qr(uri):
            print("qrencode is unavailable; use this bootstrap URI instead:")
            print(uri)
        print()
        print(f"Pairing expires in {args.timeout} seconds.")
        print("Waiting for Zara client...")

        claim = broker.wait_for_claim()
        print()
        print(f"Client device: {claim.device_id}")
        print(f"Client fingerprint: {_fingerprint(claim.public_key)}")
        print(f"Verification code: {claim.verification_code}")
        print("Confirm that Zara Android shows the same six-digit code.")
        try:
            approved = input("Approve this device? [y/N] ").strip().lower() in {"y", "yes"}
        except EOFError:
            approved = False

        if approved:
            broker.approve(claim)
        else:
            broker.reject(claim)
        result = broker.wait_for_result(timeout=5.0)
        if not result.approved:
            print(f"Pairing rejected: {result.code}", file=os.sys.stderr)
            return 1
        print(f"Paired {claim.device_id}. Android can now authenticate to {advertised_endpoint}.")
        return 0
    except (OSError, PairingError, PairingExpired, subprocess.SubprocessError) as error:
        print(f"Pairing failed: {error}", file=os.sys.stderr)
        return 2
    finally:
        broker.close()


__all__ = [
    "PairingBroker",
    "PairingClientOutcome",
    "PairingClientProgress",
    "PairingPayload",
    "PairingClaim",
    "PairingError",
    "PairingExpired",
    "PairingResult",
    "derive_device_id",
    "main",
    "pair_client",
    "pair_client_main",
    "parse_pairing_uri",
    "verification_code",
]
