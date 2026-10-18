"""Source-wiring regressions for the #1413 integration deletions.

These complement Android compilation and the existing device diagnostics tests;
they are not a substitute for real-model or device acceptance.
"""

from pathlib import Path
import re


ROOT = Path(__file__).resolve().parents[1]
APP = ROOT / "android/app/src/main/java/ai/zara/app"


def test_model_controls_preserve_canonical_session_diagnostics():
    source = (APP / "AndroidAppSession.kt").read_text()
    for declaration in (
        "fun recordChatBreadcrumb(",
        "fun diagnosticsIncidentId(",
        "private val telemetry = SessionTelemetry()",
        "actor.setConnectionFailureObserver(::onClientConnectionFailure)",
        "restoreRemoteSession(restored?.profile)",
        "DiagnosticsV2.render(snapshot)",
    ):
        assert declaration in source
    assert source.count("private val telemetry = SessionTelemetry()") == 1


def test_model_controls_delegate_to_existing_session_owned_client():
    source = (APP / "AndroidAppSession.kt").read_text()
    assert source.count("LocalAiServiceClient(context)") == 1
    for delegation in (
        "localAi.models()",
        "localAi.installModel(source, metadata)",
        "localAi.selectModel(id, version)",
        "localAi.unloadModel()",
    ):
        assert delegation in source
    assert "LocalAiRuntime(" not in source


def test_chat_failure_card_has_one_real_implementation():
    declarations = []
    for path in (APP / "ui").rglob("*.kt"):
        source = path.read_text()
        if re.search(r"\bfun\s+TurnFailureCard\s*\(", source):
            declarations.append(source)
    assert len(declarations) == 1
    source = declarations[0]
    for behavior in (
        "failure.explanation",
        "failure.code",
        "failure.connectionState",
        "failure.recovery",
        "failure.incidentId",
        "failure.retryPossible",
        "failure.reconnectPossible",
        "onRetry(userText)",
        "onClick = onReconnect",
        "onClick = onOpenDiagnostics",
    ):
        assert behavior in source
    assert "TurnFailureCard(" in (APP / "ui/ZaraApp.kt").read_text()
