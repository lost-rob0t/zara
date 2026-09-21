package ai.zara.app.telemetry

import ai.zara.app.runtime.RuntimeEvent
import ai.zara.app.runtime.RuntimeState
import ai.zara.app.runtime.ServerConnection
import ai.zara.app.runtime.ServerProfile
import ai.zara.app.runtime.reduce
import org.junit.Assert.assertEquals
import org.junit.Assert.assertTrue
import org.junit.Test

class SessionTelemetryTest {

    @Test fun `connection transitions emit typed remote events with generation`() {
        val telemetry = SessionTelemetry()
        val profile = ServerProfile.create("tcp://127.0.0.1:5555")
        var state = RuntimeState.initial()
        state = reduce(state, RuntimeEvent.EnrollmentObserved(ai.zara.app.runtime.EnrollmentReadiness.Ready))
        state = reduce(state, RuntimeEvent.ServerConfigured(profile))
        state = reduce(state, RuntimeEvent.ConnectRequested)
        val connecting = state
        telemetry.onRuntimeStateChanged(RuntimeState.initial(), connecting)

        state = reduce(state, RuntimeEvent.HelloAccepted(1, "session-1"))
        telemetry.onRuntimeStateChanged(connecting, state)

        val names = telemetry.journal().snapshot().map { it.name }
        assertTrue(names.contains(ClientEventNames.REMOTE_CONNECT_BEGIN))
        assertTrue(names.contains(ClientEventNames.REMOTE_CONNECT_READY))
        assertEquals(1L, telemetry.journal().snapshot().last().connectionGeneration)
    }

    @Test fun `connection loss emits disconnected with typed reason`() {
        val telemetry = SessionTelemetry()
        val connected = ServerConnection.Connected(2)
        val connectedState = RuntimeState.initial().copy(server = connected, sessionId = "session-1")
        telemetry.onRuntimeStateChanged(RuntimeState.initial(), connectedState)
        telemetry.onRuntimeStateChanged(
            RuntimeState.initial().copy(server = connected, sessionId = "session-1"),
            RuntimeState.initial().copy(server = ServerConnection.Reconnecting(3, 1), generation = 3),
        )

        val last = telemetry.journal().snapshot().last()
        assertEquals(ClientEventNames.REMOTE_DISCONNECTED, last.name)
        assertEquals(ZaraFailureCodes.TRANSPORT_CLOSED, last.code)
    }

    @Test fun `client failures feed the incident tracker and journal`() {
        val telemetry = SessionTelemetry()
        telemetry.noteSuccess("voice.stt.complete")
        telemetry.onClientFailure(
            failure(
                code = ZaraFailureCodes.PROTOCOL_MALFORMED,
                operation = ZaraOperation.STREAM,
                connectionGeneration = 2,
            ),
            eventName = ClientEventNames.PROTOCOL_FAILED,
        )

        assertEquals(ZaraFailureCodes.PROTOCOL_MALFORMED, telemetry.primaryIncident()?.failure?.code)
        assertEquals("voice.stt.complete", telemetry.primaryIncident()?.lastSuccess)
        val last = telemetry.journal().snapshot().last()
        assertEquals(ClientEventNames.PROTOCOL_FAILED, last.name)
        assertEquals(ZaraFailureCodes.PROTOCOL_MALFORMED, last.code)
    }

    @Test fun `voice stages advance journal and stage snapshot`() {
        val telemetry = SessionTelemetry()
        telemetry.voiceStage(VoiceStage.CAPTURE, VoiceStageProgress.RUNNING)
        telemetry.voiceStage(VoiceStage.CAPTURE, VoiceStageProgress.COMPLETE)
        telemetry.voiceStage(VoiceStage.STT, VoiceStageProgress.COMPLETE)
        telemetry.voiceStage(VoiceStage.RESPONSE, VoiceStageProgress.FAILED, ZaraFailureCodes.PROTOCOL_UNEXPECTED_MESSAGE)

        val names = telemetry.journal().snapshot().map { it.name }
        assertTrue(names.contains(ClientEventNames.VOICE_CAPTURE_BEGIN))
        assertTrue(names.contains(ClientEventNames.VOICE_CAPTURE_COMPLETE))
        assertTrue(names.contains(ClientEventNames.VOICE_STT_COMPLETE))
        assertTrue(names.contains(ClientEventNames.VOICE_RESPONSE_FAILED))

        val stages = telemetry.voiceStages()
        assertEquals(ai.zara.app.diagnostics.StageState.COMPLETE, stages[VoiceStage.CAPTURE]?.state)
        assertEquals(
            ZaraFailureCodes.PROTOCOL_UNEXPECTED_MESSAGE,
            stages[VoiceStage.RESPONSE]?.code,
        )
    }

    @Test fun `remote context tracks connection timestamps and message metadata`() {
        val telemetry = SessionTelemetry()
        val connectingState = RuntimeState.initial().copy(server = ServerConnection.Connecting(1))
        val connectedState = RuntimeState.initial().copy(server = ServerConnection.Connected(1), sessionId = "session-1")
        telemetry.onRuntimeStateChanged(RuntimeState.initial(), connectingState)
        telemetry.onRuntimeStateChanged(connectingState, connectedState)
        telemetry.onProtocolMessage(direction = ClientEventJournal.Direction.RX, messageType = "assistant.delta", messageSequence = 3, messageBytes = 64, connectionGeneration = 1)

        val context = telemetry.remoteContext()
        assertTrue((context?.lastConnectedAtMillis ?: 0) > 0)
        assertEquals("assistant.delta", context?.lastRxMessageType)
        assertEquals(3L, context?.lastRxMessageSequence)
    }

    private fun failure(
        code: String,
        operation: ZaraOperation,
        connectionGeneration: Long?,
    ): ZaraFailure = ZaraFailure(
        subsystem = ZaraSubsystem.PROTOCOL,
        operation = operation,
        phase = null,
        code = code,
        message = "injected",
        causeClass = "fixture",
        serverCode = null,
        retryable = null,
        recovery = ZaraRecovery.RETRYABLE,
        connectionGeneration = connectionGeneration,
        requestId = null,
        turnId = null,
    )
}
