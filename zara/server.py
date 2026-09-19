"""Production Zara server facade with authenticated local and remote transport."""

from __future__ import annotations

import argparse
import json
import logging
import os
import signal
import socket
import sys
import threading
from pathlib import Path
from typing import Optional

from zara import server_core as _core
from zara.principals import PrincipalContext

GatewayFactory = _core.GatewayFactory
HostFactory = _core.HostFactory
PrincipalLimitExceeded = _core.PrincipalLimitExceeded
PrincipalMismatch = _core.PrincipalMismatch
PrincipalRuntime = _core.PrincipalRuntime
RuntimeSupervisor = _core.RuntimeSupervisor
ServerAlreadyRunning = _core.ServerAlreadyRunning
ServerError = _core.ServerError
ServerLease = _core.ServerLease
ServerState = _core.ServerState
ServerStateError = _core.ServerStateError
default_zmq_endpoint = _core.default_zmq_endpoint

logger = logging.getLogger(__name__)

_DEFAULT_REMOTE_ENDPOINT = "tcp://0.0.0.0:17865"
_WILDCARD_REMOTE_HOSTS = frozenset({"", "0.0.0.0", "::", "*"})
_LOOPBACK_REMOTE_HOSTS = frozenset({"localhost", "127.0.0.1", "::1"})
_SAFE_REMOTE_CAPABILITIES = frozenset(
    {
        "session.basic",
        "runtime.status",
        "turn.submit",
        "turn.cancel",
        "tool.approve",
    }
)


class _ScalarSingleValueAction(argparse.Action):
    """Consume exactly one token while preserving a scalar Namespace value."""

    def __call__(self, parser, namespace, values, option_string=None) -> None:
        del parser, option_string
        setattr(namespace, self.dest, values[0])


def default_security_state_directory() -> Path:
    """Return Zara's persistent owner-private daemon security-state directory."""
    explicit = os.environ.get("ZARA_SECURITY_DIR", "").strip()
    if explicit:
        path = Path(explicit).expanduser()
        if not path.is_absolute():
            raise ValueError("ZARA_SECURITY_DIR must be an absolute path")
        return path
    xdg_state = os.environ.get("XDG_STATE_HOME", "").strip()
    if xdg_state and Path(xdg_state).is_absolute():
        root = Path(xdg_state)
    else:
        root = Path.home() / ".local" / "state"
    return root / "zarathushtra" / "security"


def default_control_socket_path(runtime_dir: Path | str | None = None) -> Path:
    """Return the owner-local live daemon control socket without acquiring its lease."""
    return ServerLease(runtime_dir)._runtime_dir() / "zara-control.sock"


def _split_tcp_endpoint(endpoint: str) -> tuple[str, int]:
    if not isinstance(endpoint, str) or not endpoint.startswith("tcp://"):
        raise ValueError("remote endpoint must use TCP")
    address = endpoint.removeprefix("tcp://")
    if address.startswith("["):
        close = address.find("]")
        if close <= 1 or close + 1 >= len(address) or address[close + 1] != ":":
            raise ValueError("remote endpoint is malformed")
        host = address[1:close]
        port_text = address[close + 2 :]
    else:
        host, separator, port_text = address.rpartition(":")
        if not separator or not host:
            raise ValueError("remote endpoint is malformed")
    try:
        port = int(port_text)
    except ValueError as error:
        raise ValueError("remote endpoint port is invalid") from error
    if not 1 <= port <= 65535:
        raise ValueError("remote endpoint port is invalid")
    return host, port


def _local_route_address() -> str:
    """Resolve the normal outbound IPv4 source address without sending traffic."""
    probe = socket.socket(socket.AF_INET, socket.SOCK_DGRAM)
    try:
        probe.connect(("192.0.2.1", 9))
        address = str(probe.getsockname()[0])
    except OSError as error:
        raise ServerError("could not determine a reachable local address") from error
    finally:
        probe.close()
    if address in _WILDCARD_REMOTE_HOSTS or address in _LOOPBACK_REMOTE_HOSTS:
        raise ServerError("could not determine a reachable local address")
    return address


def _advertised_remote_endpoint(endpoint: str) -> str:
    host, port = _split_tcp_endpoint(endpoint)
    if host in _WILDCARD_REMOTE_HOSTS:
        configured = os.environ.get("ZARA_ADVERTISE_HOST", "").strip()
        if configured:
            if len(configured) > 255 or any(character.isspace() for character in configured):
                raise ServerError("ZARA_ADVERTISE_HOST is invalid")
            host = configured.removeprefix("[").removesuffix("]")
        else:
            host = _local_route_address()
    rendered_host = f"[{host}]" if ":" in host else host
    return f"tcp://{rendered_host}:{port}"


class ZaraServer(_core.ZaraServer):
    """Zara service with owner-controlled authenticated TCP bootstrap."""

    def __init__(
        self,
        *,
        supervisor: Optional[RuntimeSupervisor] = None,
        lease: Optional[ServerLease] = None,
        runtime_dir: Optional[Path | str] = None,
        endpoint: Optional[str] = None,
        remote_endpoint: Optional[str] = None,
        gateway_factory: Optional[GatewayFactory] = None,
        shutdown_timeout: float = 5.0,
        principal: Optional[PrincipalContext] = None,
        config=None,
        security_state=None,
        gateway_transport_config=None,
    ) -> None:
        secure_tcp = isinstance(endpoint, str) and endpoint.startswith("tcp://")
        if remote_endpoint is not None:
            if not isinstance(remote_endpoint, str) or not remote_endpoint.startswith("tcp://"):
                raise ValueError("remote endpoint must use TCP")
            if secure_tcp:
                raise ValueError("remote endpoint requires a local IPC primary endpoint")
            if gateway_factory is not None:
                raise ValueError("remote endpoint does not accept a custom gateway factory")
            from zara.security import validate_listener_security

            validate_listener_security(remote_endpoint, curve_enabled=True, zap_enabled=True)
        if secure_tcp:
            if security_state is None:
                raise ValueError("TCP endpoint requires explicit security state")
            if gateway_factory is not None:
                raise ValueError("secure TCP endpoint does not accept a custom gateway factory")
            from zara.security import validate_listener_security

            validate_listener_security(endpoint, curve_enabled=True, zap_enabled=True)
            core_endpoint = None
        else:
            core_endpoint = endpoint

        super().__init__(
            supervisor=supervisor,
            lease=lease,
            runtime_dir=runtime_dir,
            endpoint=core_endpoint,
            gateway_factory=gateway_factory,
            shutdown_timeout=shutdown_timeout,
            principal=principal,
            config=config,
        )
        self._security_state = security_state
        self._gateway_transport_config = gateway_transport_config
        self._secure_tcp = secure_tcp
        self._remote_endpoint = remote_endpoint
        self._remote_advertised_endpoint: str | None = None
        self._remote_gateway = None
        self._remote_listener_lock = threading.RLock()
        self._security_registry = None
        self._security_admin = None
        self._security_admin_lock = threading.RLock()
        self._security_admin_closing = False
        if secure_tcp:
            self._endpoint_override = endpoint

    def _build_default_gateway(self, endpoint: str, *, supervisor, principal):
        if not endpoint.startswith("tcp://"):
            return super()._build_default_gateway(
                endpoint,
                supervisor=supervisor,
                principal=principal,
            )

        if self._security_state is None:
            raise ServerError("secure TCP listener has no security state")

        from zara.runtime.tts_output import TtsOutputBridge
        from zara.voice_runtime import RuntimeVoiceIngress

        voice_ingress = RuntimeVoiceIngress(supervisor, principal=principal)
        self._voice_ingress = voice_ingress

        sample_rate = self._audio_output_sample_rate()
        try:
            self._tts_bridge = TtsOutputBridge(
                subscription=supervisor.subscribe(principal, maxsize=256),
                publish=lambda event: supervisor.publish(principal, event),
                engine_factory=self._build_tts_engine,
                sample_rate=sample_rate,
            )
        except AttributeError:
            self._tts_bridge = None

        return self._build_secure_gateway(
            endpoint,
            supervisor=supervisor,
            voice_ingress=voice_ingress,
        )

    def _security_state_object(self):
        state = self._security_state
        if state is None:
            from zara.security_state import PersistentSecurityState

            state = PersistentSecurityState(default_security_state_directory())
            self._security_state = state
        return state

    def _control_socket_path(self) -> Path:
        lease_path = self._lease.path
        if lease_path is not None:
            return Path(lease_path).parent / "zara-control.sock"
        return default_control_socket_path(self._runtime_dir_override)

    def _ensure_security_admin(self):
        with self._security_admin_lock:
            if self._security_admin_closing:
                raise ServerStateError("zara-server owner control is shutting down")
            if self._security_admin is not None:
                return self._security_admin

            from zara.security import Capability
            from zara.security_admin import SecurityAdminServer

            state = self._security_state_object()
            admin = SecurityAdminServer(
                state,
                capabilities={Capability(value) for value in _SAFE_REMOTE_CAPABILITIES},
                control_socket_path=self._control_socket_path(),
                ensure_remote_listener=self.ensure_remote_listener,
                remote_listener_status=self.remote_listener_status,
            )
            self._security_admin = admin
            try:
                admin.start()
            except BaseException:
                self._security_admin = None
                raise
            return admin

    def _ensure_security_registry(self):
        with self._security_admin_lock:
            if self._security_registry is not None:
                return self._security_registry
            admin = self._ensure_security_admin()
            state = self._security_state_object()
            state.initialize()
            registry = state.load_registry()
            admin.bind_registry(registry)
            self._security_registry = registry
            return registry

    def _build_secure_gateway(self, endpoint: str, *, supervisor, voice_ingress):
        from zara.security_gateway import SecureZaraZmqGateway

        registry = self._ensure_security_registry()
        state = self._security_state
        if state is None:
            raise ServerError("secure listener authority is unavailable")
        return SecureZaraZmqGateway(
            endpoint,
            supervisor=supervisor,
            security_registry=registry,
            curve_server=state.load_server_config(),
            context=None,
            config=self._gateway_transport_config,
            voice_ingress=voice_ingress,
        )

    def _close_security_admin(self) -> bool:
        with self._security_admin_lock:
            self._security_admin_closing = True
            admin = self._security_admin
            self._security_admin = None
            self._security_registry = None
        if admin is None:
            return True
        try:
            admin.close(timeout=self._shutdown_timeout)
            return True
        except BaseException:
            logger.exception("Failed to stop owner security admin endpoint cleanly")
            return False

    def _remote_metadata_locked(self, *, active: bool) -> dict[str, object]:
        state = self._security_state
        public_key = None
        if active and state is not None and self._security_registry is not None:
            public_key = state.server_public_key()
        endpoint = self._remote_advertised_endpoint if active else None
        return {
            "active": active,
            "endpoint": endpoint,
            "server_public_key": public_key,
        }

    def _start_remote_gateway_locked(self, endpoint: str) -> None:
        advertised = _advertised_remote_endpoint(endpoint)
        gateway = self._build_secure_gateway(
            endpoint,
            supervisor=self._supervisor,
            voice_ingress=self._voice_ingress,
        )
        self._remote_gateway = gateway
        try:
            gateway.start().result(timeout=self._shutdown_timeout)
        except BaseException:
            self._remote_gateway = None
            try:
                gateway.close(timeout=self._shutdown_timeout)
            except BaseException:
                logger.exception("Failed to close remote ZARA/1 gateway after startup failure")
            raise
        self._remote_endpoint = endpoint
        self._remote_advertised_endpoint = advertised

    def ensure_remote_listener(self) -> dict[str, object]:
        """Idempotently expose authenticated ZARA/1 from an owner-local request."""
        with self._remote_listener_lock:
            if self.state not in {ServerState.READY, ServerState.DEGRADED}:
                raise ServerStateError("zara-server is not ready for remote listener activation")
            self._ensure_security_admin()
            if self._secure_tcp:
                self._ensure_security_registry()
                if self._remote_advertised_endpoint is None:
                    self._remote_advertised_endpoint = _advertised_remote_endpoint(
                        str(self._endpoint_override)
                    )
                return self._remote_metadata_locked(active=True)
            if self._remote_gateway is None:
                self._start_remote_gateway_locked(
                    self._remote_endpoint or _DEFAULT_REMOTE_ENDPOINT
                )
            return self._remote_metadata_locked(active=True)

    def remote_listener_status(self) -> dict[str, object]:
        with self._remote_listener_lock:
            active = self.state in {ServerState.READY, ServerState.DEGRADED} and (
                self._secure_tcp or self._remote_gateway is not None
            )
            if active and self._remote_advertised_endpoint is None:
                endpoint = self._endpoint_override if self._secure_tcp else self._remote_endpoint
                if endpoint is not None:
                    self._remote_advertised_endpoint = _advertised_remote_endpoint(str(endpoint))
            return self._remote_metadata_locked(active=active)

    def start(self) -> ServerState:
        with self._security_admin_lock:
            self._security_admin_closing = False
        try:
            state = super().start()
            self._ensure_security_admin()
            with self._remote_listener_lock:
                if self._secure_tcp:
                    self._remote_advertised_endpoint = _advertised_remote_endpoint(
                        str(self._endpoint_override)
                    )
                elif self._remote_endpoint is not None and self._remote_gateway is None:
                    self._start_remote_gateway_locked(self._remote_endpoint)
            return state
        except BaseException:
            if self.state in {ServerState.READY, ServerState.DEGRADED}:
                self.stop()
            else:
                self._close_security_admin()
            raise

    def stop(self) -> bool:
        admin_clean = self._close_security_admin()
        remote_clean = True
        with self._remote_listener_lock:
            remote = self._remote_gateway
            self._remote_gateway = None
            self._remote_advertised_endpoint = None
            if remote is not None:
                try:
                    remote.close(timeout=self._shutdown_timeout)
                except BaseException:
                    logger.exception("Failed to stop remote ZARA/1 gateway cleanly")
                    remote_clean = False
        return super().stop() and admin_clean and remote_clean


def _parse_curve_public_key(value: str) -> str:
    from zara.security import SecurityRegistry

    try:
        return SecurityRegistry._normalize_public_key(value)
    except ValueError as error:
        raise argparse.ArgumentTypeError(str(error)) from error


def _parser():
    parser = _core._parser()
    parser.description = (
        "Long-lived Zara assistant service. Local IPC is the default; authenticated "
        "remote ZARA/1 can be enabled through the owner-local control plane."
    )
    parser.add_argument(
        "--security-dir",
        help="Owner-private directory containing daemon CURVE identity and enrolled clients",
    )
    parser.add_argument(
        "--remote-endpoint",
        help="Serve authenticated TCP alongside the default owner-private local IPC endpoint",
    )
    management = parser.add_mutually_exclusive_group()
    management.add_argument(
        "--security-init",
        action="store_true",
        help="Initialize durable daemon CURVE state and print only the public server key",
    )
    management.add_argument(
        "--security-show-public-key",
        action="store_true",
        help="Print the daemon CURVE public key",
    )
    management.add_argument(
        "--security-enroll-key",
        metavar="Z85_KEY",
        nargs=1,
        action=_ScalarSingleValueAction,
        type=_parse_curve_public_key,
        help="Enroll one client CURVE public key for the local-owner principal",
    )
    management.add_argument(
        "--security-revoke-device",
        metavar="DEVICE_ID",
        help="Revoke an enrolled device in durable security state",
    )
    management.add_argument(
        "--security-list-clients",
        action="store_true",
        help="List enrolled public client metadata as JSON",
    )
    parser.add_argument(
        "--security-device-id",
        help="Device id required with --security-enroll-key",
    )
    return parser


def _security_state(args):
    if not args.security_dir:
        return None
    from zara.security_state import PersistentSecurityState

    return PersistentSecurityState(args.security_dir)


def _live_security_admin(state, *, runtime_dir: Path | str | None = None):
    from zara.security_admin import SecurityAdminClient

    runtime_path = default_control_socket_path(runtime_dir)
    if os.path.lexists(runtime_path):
        return SecurityAdminClient(runtime_path)
    if os.path.lexists(state.control_socket_path):
        return SecurityAdminClient(state.control_socket_path)
    return None


def _require_daemon_offline(args) -> None:
    """Prove no Zara daemon owns the runtime lease before touching disk directly."""
    probe = ServerLease(args.runtime_dir)
    try:
        probe.acquire()
    except ServerAlreadyRunning as error:
        raise RuntimeError(
            "zara-server is running but its live security admin endpoint is unavailable; "
            "refusing disk-only security mutation"
        ) from error
    finally:
        if probe.held:
            probe.release()


def _run_security_management(args) -> Optional[int]:
    requested = any(
        (
            args.security_init,
            args.security_show_public_key,
            args.security_enroll_key is not None,
            args.security_revoke_device is not None,
            args.security_list_clients,
        )
    )
    if not requested:
        return None
    state = _security_state(args)
    if state is None:
        raise ValueError("security management requires --security-dir")

    if args.security_init:
        _require_daemon_offline(args)
        state.initialize()
        print(state.server_public_key())
        return 0
    if args.security_show_public_key:
        print(state.server_public_key())
        return 0
    if args.security_enroll_key is not None:
        if not args.security_device_id:
            raise ValueError("--security-enroll-key requires --security-device-id")
        admin = _live_security_admin(state, runtime_dir=args.runtime_dir)
        if admin is not None:
            result = admin.request(
                "enroll",
                public_key=args.security_enroll_key,
                device_id=args.security_device_id,
            )
            print(json.dumps(result, sort_keys=True))
            return 0
        _require_daemon_offline(args)
        from zara.security import Capability

        capabilities = {Capability(value) for value in _SAFE_REMOTE_CAPABILITIES}
        enrolled = state.enroll_client(
            args.security_enroll_key,
            device_id=args.security_device_id,
            principal=PrincipalContext.local_owner(),
            capabilities=capabilities,
        )
        print(
            json.dumps(
                {
                    "device_id": enrolled.device_id,
                    "principal_id": enrolled.principal.principal_id,
                    "public_key": enrolled.public_key,
                    "capabilities": sorted(capability.value for capability in enrolled.capabilities),
                    "active": enrolled.active,
                },
                sort_keys=True,
            )
        )
        return 0
    if args.security_revoke_device is not None:
        admin = _live_security_admin(state, runtime_dir=args.runtime_dir)
        if admin is not None:
            result = admin.request("revoke", device_id=args.security_revoke_device)
            print(json.dumps(result, sort_keys=True))
            return 0
        _require_daemon_offline(args)
        state.revoke_device(args.security_revoke_device)
        print(json.dumps({"device_id": args.security_revoke_device, "active": False}, sort_keys=True))
        return 0
    if args.security_list_clients:
        admin = _live_security_admin(state, runtime_dir=args.runtime_dir)
        if admin is not None:
            clients = admin.request("list")
        else:
            _require_daemon_offline(args)
            clients = state.list_clients()
        print(json.dumps(clients, sort_keys=True))
        return 0
    return None


def main(argv: Optional[list[str]] = None) -> int:
    args = _parser().parse_args(argv)
    logging.basicConfig(
        level=logging.DEBUG if args.verbose else logging.INFO,
        format="%(asctime)s %(levelname)s %(name)s: %(message)s",
    )

    try:
        management_result = _run_security_management(args)
    except (OSError, RuntimeError, TypeError, ValueError) as error:
        print(str(error), file=sys.stderr)
        return 2
    if management_result is not None:
        return management_result

    security_state = _security_state(args)
    if isinstance(args.endpoint, str) and args.endpoint.startswith("tcp://"):
        if security_state is None:
            print("TCP endpoint requires --security-dir", file=sys.stderr)
            return 2

    stop_event = threading.Event()

    def request_stop(_signum, _frame) -> None:
        stop_event.set()

    signal.signal(signal.SIGINT, request_stop)
    signal.signal(signal.SIGTERM, request_stop)

    try:
        server = ZaraServer(
            runtime_dir=args.runtime_dir,
            endpoint=args.endpoint,
            remote_endpoint=args.remote_endpoint,
            shutdown_timeout=args.shutdown_timeout,
            security_state=security_state,
        )
    except (OSError, RuntimeError, TypeError, ValueError) as error:
        print(str(error), file=sys.stderr)
        return 2

    try:
        return server.run(stop_event)
    except ServerAlreadyRunning as error:
        print(str(error), file=sys.stderr)
        return 2
    except KeyboardInterrupt:
        stop_event.set()
        return 0 if server.stop() else 1
    except BaseException:
        logger.exception("zara-server failed")
        server.stop()
        return 1


if __name__ == "__main__":
    raise SystemExit(main())


__all__ = [
    "GatewayFactory",
    "PrincipalContext",
    "PrincipalLimitExceeded",
    "PrincipalMismatch",
    "PrincipalRuntime",
    "RuntimeSupervisor",
    "ServerAlreadyRunning",
    "ServerError",
    "ServerLease",
    "ServerState",
    "ServerStateError",
    "ZaraServer",
    "default_control_socket_path",
    "default_security_state_directory",
    "default_zmq_endpoint",
    "main",
]
