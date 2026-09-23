package ai.zara.app.diagnostics

import ai.zara.app.telemetry.ClientEventJournal
import ai.zara.app.telemetry.ClientEventNames
import ai.zara.app.telemetry.ClientEventOutcome
import ai.zara.app.telemetry.FailureIncident
import ai.zara.app.telemetry.ZaraFailure
import ai.zara.app.telemetry.ZaraFailureCodes
import ai.zara.app.telemetry.ZaraOperation
import ai.zara.app.telemetry.ZaraRecovery
import ai.zara.app.telemetry.ZaraSubsystem
import org.junit.Assert.assertEquals
import org.junit.Assert.assertFalse
import org.junit.Assert.assertTrue
import org.junit.Test

class DiagnosticsV2Test {

    @Test fun `contract header and primary failure block render deterministically`() {
        val bundle = DiagnosticsV2.render(
            snapshot = snapshot(
                incident = incident(
                    code = ZaraFailureCodes.PROTOCOL_UNEXPECTED_MESSAGE,
                    subsystem = ZaraSubsystem.PROTOCOL,
                    operation = ZaraOperation.STREAM,
                    recovery = ZaraRecovery.RETRYABLE,
                    turnId = "turn-42",
                ),
                lastSuccess = "voice.transcript.final",
            ),
        )

        val text = bundle.text
        assertTrue(text.startsWith("ZARA-LOCAL-DIAGNOSTICS/2\n"))
        for (required in listOf(
            "diagnostics_version=2",
            "diagnostic_id=",
            "captured_at=2025-09-20T00:00:00Z",
            "version=0.2.2-alpha",
            "version_code=4",
            "source_sha=36e6d48fa750f30b5493f0c2acc789f3e9300e97",
            "runtime_mode=remote",
            "session_generation=3",
            "connection_phase=disconnected",
            "enrollment_phase=ready",
            "primary_failure.present=true",
            "primary_failure.subsystem=protocol",
            "primary_failure.operation=stream",
            "primary_failure.phase=voice_stream",
            "primary_failure.code=protocol.unexpected_message",
            "primary_failure.last_success=voice.transcript.final",
            "primary_failure.recovery=retryable",
            "primary_failure.turn_id=turn-42",
        )) {
            assertTrue("missing header line: $required", text.containsLine(required))
        }
        assertEquals(bundle.json, DiagnosticsV2.render(snapshot = snapshot(
            incident = incident(
                code = ZaraFailureCodes.PROTOCOL_UNEXPECTED_MESSAGE,
                subsystem = ZaraSubsystem.PROTOCOL,
                operation = ZaraOperation.STREAM,
                recovery = ZaraRecovery.RETRYABLE,
                turnId = "turn-42",
            ),
            lastSuccess = "voice.transcript.final",
        )).json)
    }

    @Test fun `remote protocol context is complete without payloads`() {
        val bundle = DiagnosticsV2.render(
            snapshot = snapshot(
                remoteContext = RemoteProtocolContext(
                    transportKind = "zmq-dealer-tcp-curve",
                    connectGeneration = 3,
                    lastConnectedAtMillis = 1_000,
                    lastDisconnectedAtMillis = 2_000,
                    negotiatedProtocolVersion = 1,
                    clientProtocolVersions = "1",
                    serverProtocolVersion = 1,
                    handshakeState = "negotiated",
                    closeCode = ZaraFailureCodes.PROTOCOL_MALFORMED,
                    closeReason = "truncated ZARA/1 multipart",
                    lastRxMessageType = "audio.output.chunk",
                    lastRxMessageSequence = 7,
                    lastRxMessageBytes = 2_048,
                    lastTxMessageType = "audio.input.commit",
                    lastTxMessageSequence = null,
                    lastTxMessageBytes = 96,
                    expectedNextState = "reconnect_hello",
                    pendingRequestCount = 0,
                    lastCompletedTurnId = "turn-41",
                ),
            ),
        )
        for (required in listOf(
            "remote.transport=zmq-dealer-tcp-curve",
            "remote.connect_generation=3",
            "remote.negotiated_protocol_version=1",
            "remote.handshake_state=negotiated",
            "remote.close_code=protocol.malformed",
            "remote.last_rx_message_type=audio.output.chunk",
            "remote.last_rx_message_bytes=2048",
            "remote.last_tx_message_type=audio.input.commit",
            "remote.expected_next_state=reconnect_hello",
            "remote.last_completed_turn_id=turn-41",
        )) {
            assertTrue("missing remote context: $required", bundle.text.containsLine(required))
        }
        assertFalse(bundle.text.contains("token="))
    }

    @Test fun `voice pipeline stages expose explicit semantics`() {
        val bundle = DiagnosticsV2.render(
            snapshot = snapshot(
                voiceStages = listOf(
                    VoiceStageState(VoiceStageKind.CAPTURE, StageState.COMPLETE, null, 1_500),
                    VoiceStageState(VoiceStageKind.VAD, StageState.NOT_APPLICABLE, null, null),
                    VoiceStageState(VoiceStageKind.STT, StageState.COMPLETE, null, 1_700),
                    VoiceStageState(VoiceStageKind.SUBMIT, StageState.COMPLETE, null, 1_800),
                    VoiceStageState(VoiceStageKind.RESPONSE, StageState.FAILED, ZaraFailureCodes.PROTOCOL_UNEXPECTED_MESSAGE, 1_900),
                    VoiceStageState(VoiceStageKind.TTS, StageState.NOT_STARTED, null, null),
                    VoiceStageState(VoiceStageKind.PLAYBACK, StageState.NOT_STARTED, null, null),
                ),
            ),
        )
        for (required in listOf(
            "voice.capture=complete",
            "voice.vad=not_applicable",
            "voice.stt=complete",
            "voice.response=failed",
            "voice.response.code=protocol.unexpected_message",
            "voice.tts=not_started",
        )) {
            assertTrue("missing voice stage: $required", bundle.text.containsLine(required))
        }
    }

    @Test fun `local ai semantics distinguish not applicable from unknown`() {
        val bundle = DiagnosticsV2.render(
            snapshot = snapshot(localAiPhase = "not_applicable", localAiNote = "remote-only mode never starts the local model"),
        )
        assertTrue(bundle.text.containsLine("local_ai_phase=not_applicable"))
        assertTrue(bundle.text.containsLine("local_ai_generation=not_applicable"))
        assertTrue(bundle.text.containsLine("local_ai_model=not_applicable"))
    }

    @Test fun `timeline is ordered and correlated around the primary failure`() {
        val events = ClientEventJournal()
        events.record(ClientEventNames.REMOTE_CONNECT_BEGIN, connectionGeneration = 3)
        events.record(ClientEventNames.PROTOCOL_HANDSHAKE_NEGOTIATED, connectionGeneration = 3)
        events.record(ClientEventNames.VOICE_CAPTURE_BEGIN, turnId = "turn-42")
        events.record(ClientEventNames.VOICE_STT_COMPLETE, turnId = "turn-42")
        events.record(ClientEventNames.PROTOCOL_FAILED, code = ZaraFailureCodes.PROTOCOL_UNEXPECTED_MESSAGE, turnId = "turn-42")
        events.record(ClientEventNames.REMOTE_DISCONNECTED, code = ZaraFailureCodes.PROTOCOL_UNEXPECTED_MESSAGE)

        val bundle = DiagnosticsV2.render(snapshot = snapshot(events = events.snapshot()))
        val timelineStart = bundle.text.indexOf("--- timeline ---")
        assertTrue(timelineStart > 0)
        val timeline = bundle.text.substring(timelineStart)
        val order = listOf(
            "remote.connect.begin",
            "protocol.handshake.negotiated",
            "voice.capture.begin",
            "voice.stt.complete",
            "protocol.failed",
            "remote.disconnected",
        )
        var cursor = 0
        for (name in order) {
            val found = timeline.indexOf(name, cursor)
            assertTrue("timeline event $name missing or out of order", found >= 0)
            cursor = found
        }
        assertTrue(timeline.contains("seq="))
        assertTrue(timeline.contains("turn_id=turn-42"))
    }

    @Test fun `no primary failure is explicit and does not invent one`() {
        val bundle = DiagnosticsV2.render(snapshot = snapshot(incident = null))
        assertTrue(bundle.text.containsLine("primary_failure.present=false"))
        assertFalse(bundle.text.contains("primary_failure.code="))
    }

    @Test fun `golden scenarios identify subsystem operation code last-good and correlation`() {
        val scenarios = mapOf(
            "remote handshake version mismatch" to scenarioFailure(
                code = ZaraFailureCodes.PROTOCOL_VERSION_MISMATCH,
                subsystem = ZaraSubsystem.PROTOCOL,
                operation = ZaraOperation.HANDSHAKE,
                phase = "hello",
            ),
            "remote protocol malformed frame" to scenarioFailure(
                code = ZaraFailureCodes.PROTOCOL_MALFORMED,
                subsystem = ZaraSubsystem.PROTOCOL,
                operation = ZaraOperation.STREAM,
                phase = "voice_stream",
            ),
            "network disconnect mid stream" to scenarioFailure(
                code = ZaraFailureCodes.TRANSPORT_CLOSED,
                subsystem = ZaraSubsystem.TRANSPORT,
                operation = ZaraOperation.STREAM,
                phase = "audio_output",
            ),
            "auth enrollment rejection" to scenarioFailure(
                code = ZaraFailureCodes.AUTH_REJECTED,
                subsystem = ZaraSubsystem.AUTH,
                operation = ZaraOperation.CONNECT,
                phase = "enroll",
            ),
            "stt failure before submit" to scenarioFailure(
                code = ZaraFailureCodes.VOICE_STT,
                subsystem = ZaraSubsystem.STT,
                operation = ZaraOperation.VOICE_TURN,
                phase = "stt",
            ),
            "remote request timeout" to scenarioFailure(
                code = ZaraFailureCodes.TRANSPORT_TIMEOUT,
                subsystem = ZaraSubsystem.TRANSPORT,
                operation = ZaraOperation.SUBMIT,
                phase = "awaiting_first_reply",
            ),
            "tts failure after successful response" to scenarioFailure(
                code = ZaraFailureCodes.VOICE_TTS,
                subsystem = ZaraSubsystem.TTS,
                operation = ZaraOperation.SPEAK,
                phase = "audio_output",
            ),
            "local prolog boot failure" to scenarioFailure(
                code = "lifecycle.restore",
                subsystem = ZaraSubsystem.PROLOG,
                operation = ZaraOperation.RESTORE,
                phase = "prolog_boot",
            ),
        )
        for ((name, failure) in scenarios) {
            val bundle = DiagnosticsV2.render(
                snapshot = snapshot(incident = FailureIncident(failure, 1_000, 2_000, "prior.step.ok")),
            )
            val text = bundle.text
            assertTrue("$name: missing subsystem", text.containsLine("primary_failure.subsystem=${failure.subsystem.name.lowercase()}"))
            assertTrue("$name: missing operation", text.containsLine("primary_failure.operation=${failure.operation.name.lowercase()}"))
            assertTrue("$name: missing typed code", text.containsLine("primary_failure.code=${failure.code}"))
            assertTrue("$name: missing last success", text.containsLine("primary_failure.last_success=prior.step.ok"))
            assertTrue("$name: missing recovery", text.contains("primary_failure.recovery="))
            assertTrue("$name: missing correlation", text.contains("primary_failure.request_id=req-7"))
        }
    }

    @Test fun `secret canaries never leak into either form`() {
        val bundle = DiagnosticsV2.render(
            snapshot = snapshot(
                incident = incident(code = ZaraFailureCodes.TRANSPORT_CLOSED, subsystem = ZaraSubsystem.TRANSPORT, operation = ZaraOperation.STREAM),
                closeReason = "token=super-secret authorization=raw-value",
            ),
        )
        for (canary in listOf("super-secret", "raw-value")) {
            assertFalse(bundle.text.contains(canary))
            assertFalse(bundle.json.contains(canary))
        }
        assertTrue(bundle.text.contains("token=<redacted>"))
    }

    @Test fun `json form is canonical and agrees with the text version`() {
        val bundle = DiagnosticsV2.render(snapshot = snapshot())
        assertTrue(bundle.json.startsWith("{"))
        assertTrue(bundle.json.endsWith("}"))
        assertTrue(bundle.json.contains("\"diagnostics_version\":2"))
        assertTrue(bundle.json.contains("\"primary_failure\""))
        assertTrue(bundle.json.contains("\"runtime_mode\":\"remote\""))
        assertFalse(bundle.json.contains("primary_failure.present"))
        assertTrue(bundle.json.contains("\"diagnostic_id\":\"diag-0000000000000001\""))
    }

    private fun scenarioFailure(
        code: String,
        subsystem: ZaraSubsystem,
        operation: ZaraOperation,
        phase: String,
    ): ZaraFailure = ZaraFailure(
        subsystem = subsystem,
        operation = operation,
        phase = phase,
        code = code,
        message = "injected fixture",
        causeClass = "fixture.FixtureException",
        serverCode = null,
        retryable = null,
        recovery = ZaraRecovery.RETRYABLE,
        connectionGeneration = 3,
        requestId = "req-7",
        turnId = "turn-42",
    )

    private fun incident(
        code: String,
        subsystem: ZaraSubsystem,
        operation: ZaraOperation,
        recovery: ZaraRecovery = ZaraRecovery.RETRYABLE,
        turnId: String? = "turn-42",
    ): FailureIncident = FailureIncident(
        failure = ZaraFailure(
            subsystem = subsystem,
            operation = operation,
            phase = "voice_stream",
            code = code,
            message = "unexpected message while waiting for response stream",
            causeClass = "ai.zara.app.runtime.ZaraWireException",
            serverCode = null,
            retryable = null,
            recovery = recovery,
            connectionGeneration = 3,
            requestId = "req-7",
            turnId = turnId,
        ),
        firstSeenMillis = 1_900,
        lastSeenMillis = 1_950,
        lastSuccess = null,
    )

    private fun snapshot(
        incident: FailureIncident? = null,
        lastSuccess: String? = null,
        remoteContext: RemoteProtocolContext? = null,
        voiceStages: List<VoiceStageState> = emptyList(),
        localAiPhase: String = "loading",
        localAiNote: String? = null,
        closeReason: String = "peer closed",
        events: List<ai.zara.app.telemetry.ClientEvent> = emptyList(),
    ): DiagnosticsSnapshot = DiagnosticsSnapshot(
        version = "0.2.2-alpha",
        versionCode = 4,
        sourceSha = "36e6d48fa750f30b5493f0c2acc789f3e9300e97",
        runtimeMode = "remote",
        sessionId = "session-redacted-safe",
        sessionGeneration = 3,
        connectionPhase = "disconnected",
        enrollmentPhase = "ready",
        incident = incident?.let {
            if (lastSuccess != null) it.copy(lastSuccess = lastSuccess) else it
        },
        remoteContext = (remoteContext ?: RemoteProtocolContext(
            transportKind = "zmq-dealer-tcp-curve",
            connectGeneration = 3,
            lastConnectedAtMillis = 1_000,
            lastDisconnectedAtMillis = 2_000,
            negotiatedProtocolVersion = 1,
            clientProtocolVersions = "1",
            serverProtocolVersion = 1,
            handshakeState = "negotiated",
            closeCode = incident?.failure?.code,
            closeReason = closeReason,
            lastRxMessageType = "voice.transcript.final",
            lastRxMessageSequence = 6,
            lastRxMessageBytes = 512,
            lastTxMessageType = "audio.input.commit",
            lastTxMessageSequence = null,
            lastTxMessageBytes = 96,
            expectedNextState = "reconnect_hello",
            pendingRequestCount = 0,
            lastCompletedTurnId = "turn-41",
        )),
        voiceStages = voiceStages,
        localAiPhase = localAiPhase,
        localAiNote = localAiNote,
        localAiGeneration = if (localAiPhase == "not_applicable") null else -1,
        localAiModel = null,
        localServerPhase = "ready",
        localServerGeneration = 1,
        localServerFailure = null,
        events = events,
        diagnosticId = "diag-0000000000000001",
        capturedAtMillis = 1_758_326_400_000,
    )
}

private fun String.containsLine(line: String): Boolean =
    split('\n').any { it == line || it.startsWith(line) }
