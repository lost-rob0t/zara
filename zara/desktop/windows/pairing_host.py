"""Remote-device pairing host for the desktop Connections surface.

THESIS: Onboarding a mesh device is one inspectable flow with visible data.
OWN-WORLD: Steps, QR, live URI, countdown, claim approval, and nicknames.
STORY: Scan, compare the six-digit code, name the device, approve it once.
FORM: The established Signal Cabin operating surface; seed 35e80c4d.
"""

from __future__ import annotations

import hashlib
import io
import queue
import secrets
import threading
import time
from dataclasses import dataclass
from typing import Callable, Optional

from PySide6.QtCore import Qt, QTimer, Signal
from PySide6.QtGui import QPixmap
from PySide6.QtWidgets import (
    QDialog,
    QFrame,
    QHBoxLayout,
    QLabel,
    QLineEdit,
    QPushButton,
    QVBoxLayout,
    QWidget,
)

from zara.pairing import (
    PairingBroker,
    PairingCancelled,
    PairingError,
    PairingExpired,
    PairingHostPlan,
    plan_host_pairing,
)
from zara.security_state import PersistentSecurityState, SecurityStateError

SESSION_SECONDS = 120
_NICKNAME_LIMIT = 48
_KIND_LABELS = {"android": "Phone", "desktop": "Desktop"}

_STEPS = (
    "1. Open Zara on the device and choose Pair with server.",
    "2. Scan this QR code, or paste the URI into the device's pairing field.",
    "3. Compare the six-digit verification code on both screens.",
    "4. Name the device and approve it here when it connects.",
)


class PairingHostError(RuntimeError):
    pass


@dataclass(frozen=True)
class ClaimView:
    device_id: str
    verification_code: str
    fingerprint: str
    nickname_suggestion: str


def paired_security_state(config) -> PersistentSecurityState:
    from zara.pairing import security_directory

    return PersistentSecurityState(security_directory(config, None))


def list_paired_devices(config) -> tuple[dict[str, object], ...]:
    return paired_security_state(config).list_clients()


def set_device_nickname(config, device_id: str, nickname: str) -> dict[str, object]:
    return paired_security_state(config).rename_device(device_id, nickname)


def device_kind(device_id: str) -> str:
    return device_id.split("-", 1)[0] if "-" in device_id else "device"


def nickname_suggestion(device_id: str) -> str:
    return _KIND_LABELS.get(device_kind(device_id), "Device")


def fingerprint(public_key: str) -> str:
    return hashlib.sha256(public_key.encode("ascii")).hexdigest()[:16]


def qr_pixmap(payload: str, *, box_size: int = 6, border: int = 2) -> QPixmap:
    try:
        import qrcode
    except ImportError as error:
        raise PairingHostError("QR rendering is unavailable; copy the pairing URI instead") from error
    code = qrcode.QRCode(
        error_correction=qrcode.constants.ERROR_CORRECT_M,
        box_size=box_size,
        border=border,
    )
    code.add_data(payload)
    code.make(fit=True)
    image = code.make_image(fill_color="black", back_color="white")
    raw = io.BytesIO()
    image.save(raw, format="PNG")
    pixmap = QPixmap()
    if not pixmap.loadFromData(raw.getvalue(), "PNG"):
        raise PairingHostError("QR image could not be decoded")
    return pixmap


def _default_broker_factory(
    plan: PairingHostPlan,
    token: str,
    expires_at: int,
) -> PairingBroker:
    from zara.security_admin import SecurityAdminClient

    admin = SecurityAdminClient(plan.control_socket_path)
    return PairingBroker(
        endpoint=plan.endpoint,
        server_public_key=plan.server_public_key,
        token=token,
        expires_at=expires_at,
        bind_host=plan.bind_host,
        enroll=lambda public_key, device_id: admin.request(
            "enroll",
            public_key=public_key,
            device_id=device_id,
        ),
    )


class PairRemoteDeviceDialog(QDialog):
    """Host one short-lived pairing window for any remote Zara device."""

    session_ready = Signal(str, object)
    claim_observed = Signal(object)
    session_finished = Signal(str, str)

    def __init__(
        self,
        config,
        *,
        parent: Optional[QWidget] = None,
        session_seconds: int = SESSION_SECONDS,
        plan_factory: Callable[..., PairingHostPlan] = plan_host_pairing,
        broker_factory: Callable[[PairingHostPlan, str, int], PairingBroker] | None = None,
        nickname_writer: Callable[[PairingHostPlan, str, str], object] | None = None,
    ) -> None:
        super().__init__(parent)
        self.setWindowTitle("Pair Remote device")
        self.setMinimumWidth(560)
        self.config = config
        self._session_seconds = int(session_seconds)
        self._plan_factory = plan_factory
        self._broker_factory = broker_factory or _default_broker_factory
        self._nickname_writer = nickname_writer or self._write_nickname
        self._stop = threading.Event()
        self._decision: queue.Queue[tuple[str, str]] = queue.Queue(maxsize=1)
        self._pending_nickname = ""
        self._plan: PairingHostPlan | None = None
        self._broker: PairingBroker | None = None
        self._expires_at = 0.0
        self._worker: threading.Thread | None = None
        self._finished = False

        layout = QVBoxLayout(self)
        layout.setContentsMargins(22, 20, 22, 18)
        layout.setSpacing(10)

        title = QLabel("Pair a remote device")
        title.setObjectName("zaraSectionTitle")
        layout.addWidget(title)

        steps = QLabel("\n".join(_STEPS))
        steps.setObjectName("zaraSettingsHint")
        steps.setWordWrap(True)
        layout.addWidget(steps)

        self.qr_label = QLabel()
        self.qr_label.setObjectName("zaraPairingQr")
        self.qr_label.setMinimumSize(220, 220)
        self.qr_label.setAlignment(Qt.AlignmentFlag.AlignCenter)
        layout.addWidget(self.qr_label)

        uri_row = QHBoxLayout()
        self.uri_input = QLineEdit()
        self.uri_input.setObjectName("zaraPairingUri")
        self.uri_input.setReadOnly(True)
        self.copy_button = QPushButton("Copy URI")
        self.copy_button.setObjectName("zaraSecondaryAction")
        self.copy_button.clicked.connect(self._copy_uri)
        uri_row.addWidget(self.uri_input, 1)
        uri_row.addWidget(self.copy_button)
        layout.addLayout(uri_row)

        self.endpoint_label = QLabel("Endpoint: —")
        self.host_label = QLabel("Advertised host: —")
        self.expires_label = QLabel("Pairing window: —")
        for label in (self.endpoint_label, self.host_label, self.expires_label):
            label.setObjectName("zaraSettingsHint")
            layout.addWidget(label)

        self.status_label = QLabel("Starting pairing window…")
        self.status_label.setObjectName("zaraPairingStatus")
        self.status_label.setWordWrap(True)
        layout.addWidget(self.status_label)

        self.claim_panel = QFrame()
        self.claim_panel.setObjectName("zaraPairingClaim")
        claim_layout = QVBoxLayout(self.claim_panel)
        claim_layout.setContentsMargins(0, 6, 0, 0)
        claim_layout.setSpacing(8)
        self.device_label = QLabel()
        self.fingerprint_label = QLabel()
        self.code_label = QLabel()
        self.code_label.setObjectName("zaraPairingCode")
        nickname_row = QHBoxLayout()
        nickname_row.addWidget(QLabel("Nickname"))
        self.nickname_input = QLineEdit()
        self.nickname_input.setObjectName("zaraPairingNickname")
        self.nickname_input.setMaxLength(_NICKNAME_LIMIT)
        self.nickname_input.setPlaceholderText("Pixel 9")
        nickname_row.addWidget(self.nickname_input, 1)
        buttons_row = QHBoxLayout()
        self.reject_button = QPushButton("Reject")
        self.reject_button.setObjectName("zaraDangerAction")
        self.approve_button = QPushButton("Approve device")
        self.approve_button.setObjectName("zaraPrimaryAction")
        buttons_row.addStretch(1)
        buttons_row.addWidget(self.reject_button)
        buttons_row.addWidget(self.approve_button)
        claim_layout.addWidget(self.device_label)
        claim_layout.addWidget(self.fingerprint_label)
        claim_layout.addWidget(self.code_label)
        claim_layout.addLayout(nickname_row)
        claim_layout.addLayout(buttons_row)
        self.claim_panel.setVisible(False)
        layout.addWidget(self.claim_panel)

        self.approve_button.clicked.connect(self._approve)
        self.reject_button.clicked.connect(self._reject)
        self.session_ready.connect(self._on_session_ready)
        self.claim_observed.connect(self._on_claim)
        self.session_finished.connect(self._on_finished)
        self._timer = QTimer(self)
        self._timer.setInterval(1000)
        self._timer.timeout.connect(self._tick_countdown)

    # ------------------------------------------------------------------
    # Session control

    def current_uri(self) -> str:
        return self.uri_input.text()

    def is_session_active(self) -> bool:
        return not self._finished

    def start_session(self) -> None:
        if self._worker is not None:
            return
        self._worker = threading.Thread(
            target=self._run_session,
            name="zara-remote-pairing",
            daemon=True,
        )
        self._worker.start()

    def cancel_session(self) -> None:
        # Only signal: the worker observes the stop event and rejects any
        # pending claim through the live broker before tearing it down.
        self._stop.set()

    def reject(self) -> None:  # noqa: N802 - Qt API
        self.cancel_session()
        super().reject()

    def closeEvent(self, event) -> None:  # noqa: N802 - Qt API
        self.cancel_session()
        super().closeEvent(event)

    def _close_broker(self) -> None:
        broker = self._broker
        if broker is not None:
            self._broker = None
            broker.close()

    def _copy_uri(self) -> None:
        from PySide6.QtWidgets import QApplication

        clipboard = QApplication.clipboard()
        if clipboard is not None:
            clipboard.setText(self.uri_input.text())
            self.status_label.setText("Pairing URI copied.")

    # ------------------------------------------------------------------
    # Worker thread

    def _run_session(self) -> None:
        try:
            plan = self._plan_factory(self.config)
        except Exception as error:
            self._finish("", str(error))
            return
        if self._stop.is_set():
            self._finish("", "Pairing cancelled")
            return
        self._plan = plan
        token = secrets.token_urlsafe(32)
        expires_at = int(time.time()) + self._session_seconds
        self._expires_at = float(expires_at)
        try:
            broker = self._broker_factory(plan, token, expires_at)
        except Exception as error:
            self._finish("", str(error))
            return
        self._broker = broker
        try:
            uri = broker.pairing_uri(advertise_host=plan.advertise_host)
        except (TypeError, ValueError, PairingError) as error:
            self._finish("", str(error))
            return
        self.session_ready.emit(uri, plan)
        try:
            claim = broker.wait_for_claim(stop=self._stop)
        except PairingCancelled:
            self._finish("", "Pairing cancelled")
            return
        except PairingExpired:
            self._finish("", "Pairing window expired")
            return
        except PairingError as error:
            self._finish("", str(error))
            return
        self.claim_observed.emit(
            ClaimView(
                device_id=claim.device_id,
                verification_code=claim.verification_code,
                fingerprint=fingerprint(claim.public_key),
                nickname_suggestion=nickname_suggestion(claim.device_id),
            )
        )
        self._await_decision(broker, claim)

    def _await_decision(self, broker: PairingBroker, claim) -> None:
        action: str | None = None
        while action is None:
            if self._stop.is_set():
                action = "cancel"
                break
            remaining = float(broker.expires_at) - time.time()
            if remaining <= 0:
                action = "expire"
                break
            try:
                action, _ = self._decision.get(timeout=min(0.25, remaining))
            except queue.Empty:
                continue
        if action == "approve":
            broker.approve(claim)
        elif action == "cancel":
            broker.reject(claim, code="cancelled")
        elif action == "expire":
            broker.reject(claim, code="expired")
        else:
            broker.reject(claim)
        try:
            result = broker.wait_for_result(timeout=5.0)
        except PairingError as error:
            self._finish(claim.device_id, str(error))
            return
        if not result.approved:
            message = "Pairing cancelled" if self._stop.is_set() else f"Pairing rejected ({result.code})"
            self._finish(claim.device_id, message)
            return
        nickname = self._pending_nickname.strip()
        if nickname:
            try:
                self._nickname_writer(self._plan, claim.device_id, nickname)
            except (SecurityStateError, OSError, TypeError, ValueError) as error:
                self._finish(
                    claim.device_id,
                    f"Paired, but the nickname was not saved: {error}",
                )
                return
        self._finish(claim.device_id, "")

    def _finish(self, device_id: str, error: str) -> None:
        self._finished = True
        self._close_broker()
        self.session_finished.emit(device_id, error)

    @staticmethod
    def _write_nickname(plan: PairingHostPlan, device_id: str, nickname: str) -> object:
        return PersistentSecurityState(plan.security_dir).rename_device(device_id, nickname)

    # ------------------------------------------------------------------
    # UI thread slots

    def _on_session_ready(self, uri: str, plan) -> None:
        self.uri_input.setText(uri)
        self.endpoint_label.setText(f"Endpoint: {plan.endpoint}")
        self.host_label.setText(f"Advertised host: {plan.advertise_host}")
        self.status_label.setText("Waiting for a device to connect…")
        self._timer.start()
        self._tick_countdown()
        try:
            pixmap = qr_pixmap(uri)
        except PairingHostError as error:
            self.status_label.setText(str(error))
            return
        self.qr_label.setPixmap(
            pixmap.scaled(
                220,
                220,
                Qt.AspectRatioMode.KeepAspectRatio,
                Qt.TransformationMode.SmoothTransformation,
            )
        )

    def _on_claim(self, claim: ClaimView) -> None:
        self._timer.stop()
        self.device_label.setText(f"Device: {claim.device_id} ({device_kind(claim.device_id)})")
        self.fingerprint_label.setText(f"Fingerprint: {claim.fingerprint}")
        self.code_label.setText(f"Verification code {claim.verification_code}")
        if not self.nickname_input.text().strip():
            self.nickname_input.setText(claim.nickname_suggestion)
        self.claim_panel.setVisible(True)
        self.approve_button.setEnabled(True)
        self.reject_button.setEnabled(True)
        self.status_label.setText("Compare the code, name the device, then approve it.")

    def _approve(self) -> None:
        if not self._decide("approve"):
            return
        self._pending_nickname = self.nickname_input.text()
        self.approve_button.setEnabled(False)
        self.reject_button.setEnabled(False)
        self.status_label.setText("Enrolling the device…")

    def _reject(self) -> None:
        if not self._decide("reject"):
            return
        self.approve_button.setEnabled(False)
        self.reject_button.setEnabled(False)
        self.status_label.setText("Rejecting the device…")

    def _decide(self, action: str) -> bool:
        try:
            self._decision.put_nowait((action, ""))
        except queue.Full:
            return False
        return True

    def _on_finished(self, device_id: str, error: str) -> None:
        self._timer.stop()
        self.claim_panel.setVisible(False)
        if error:
            self.status_label.setText(error)
            return
        name = self._pending_nickname.strip()
        shown = name or device_id
        self.status_label.setText(f"Paired {shown}. The device can now authenticate to Zara.")

    def _tick_countdown(self) -> None:
        remaining = int(self._expires_at - time.time())
        if remaining > 0:
            self.expires_label.setText(f"Pairing window: {remaining} s")


__all__ = [
    "ClaimView",
    "PairingHostError",
    "PairRemoteDeviceDialog",
    "SESSION_SECONDS",
    "device_kind",
    "fingerprint",
    "list_paired_devices",
    "nickname_suggestion",
    "paired_security_state",
    "qr_pixmap",
    "set_device_nickname",
]
