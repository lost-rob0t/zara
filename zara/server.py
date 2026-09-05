"""Production Zara server facade with opt-in authenticated remote transport.

The process/runtime lifecycle remains in :mod:`zara.server_core`. Local IPC is
still the default. A TCP listener is accepted only when an explicit persistent
security state is supplied, and is always backed by CURVE/ZAP.
"""

from __future__ import annotations

import json
import logging
import os
import signal
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

_SAFE_REMOTE_CAPABILITIES = frozenset(
    {
        "session.basic",
        "runtime.status",
        "turn.submit",
        "turn.cancel",
        "tool.approve",
    }
)


class ZaraServer(_core.ZaraServer):
    """Zara service with secure opt-in TCP and unchanged local IPC defaults."""

    def __init__(
        self,
        *,
        supervisor: Optional[RuntimeSupervisor] = None,
        lease: Optional[ServerLease] = None,
        runtime_dir: Optional[Path | str] = None,
        endpoint: Optional[str] = None,
        gateway_factory: Optional[GatewayFactory] = None,
        shutdown_timeout: float = 5.0,
        principal: Optional[PrincipalContext] = None,
        config=None,
        security_state=None,
        gateway_transport_config=None,
    ) -> None:
        secure_tcp = isinstance(endpoint, str) and endpoint.startswith("tcp://")
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
        self._security_registry = None
        self._security_admin = None
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
        from zara.security import Capability
        from zara.security_admin import SecurityAdminServer
        from zara.security_gateway import SecureZaraZmqGateway
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

        admin = SecurityAdminServer(
            self._security_state,
            capabilities={Capability(value) for value in _SAFE_REMOTE_CAPABILITIES},
        )
        admin.start()
        try:
            registry = self._security_state.load_registry()
            admin.bind_registry(registry)
            gateway = SecureZaraZmqGateway(
                endpoint,
                supervisor=supervisor,
                security_registry=registry,
                curve_server=self._security_state.load_server_config(),
                context=None,
                config=self._gateway_transport_config,
                voice_ingress=voice_ingress,
            )
        except BaseException:
            admin.close(timeout=self._shutdown_timeout)
            raise
        self._security_registry = registry
        self._security_admin = admin
        return gateway

    def _close_security_admin(self) -> bool:
        admin = self._security_admin
        self._security_admin = None
        if admin is None:
            return True
        try:
            admin.close(timeout=self._shutdown_timeout)
            return True
        except BaseException:
            logger.exception("Failed to stop owner security admin endpoint cleanly")
            return False

    def start(self) -> ServerState:
        try:
            return super().start()
        except BaseException:
            self._close_security_admin()
            raise

    def stop(self) -> bool:
        admin_clean = self._close_security_admin()
        return super().stop() and admin_clean


def _parser():
    parser = _core._parser()
    parser.description = (
        "Long-lived Zara assistant service. Local IPC is the default; remote TCP "
        "requires explicit owner-managed CURVE/ZAP security state."
    )
    parser.add_argument(
        "--security-dir",
        help="Owner-private directory containing daemon CURVE identity and enrolled clients",
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


def _live_security_admin(state):
    if not os.path.lexists(state.control_socket_path):
        return None
    from zara.security_admin import SecurityAdminClient

    return SecurityAdminClient(state.control_socket_path)


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
        admin = _live_security_admin(state)
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
        admin = _live_security_admin(state)
        if admin is not None:
            result = admin.request("revoke", device_id=args.security_revoke_device)
            print(json.dumps(result, sort_keys=True))
            return 0
        _require_daemon_offline(args)
        state.revoke_device(args.security_revoke_device)
        print(json.dumps({"device_id": args.security_revoke_device, "active": False}, sort_keys=True))
        return 0
    if args.security_list_clients:
        admin = _live_security_admin(state)
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
    "default_zmq_endpoint",
    "main",
]
