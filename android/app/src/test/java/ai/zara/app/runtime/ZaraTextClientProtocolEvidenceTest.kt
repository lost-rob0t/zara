package ai.zara.app.runtime

import ai.zara.app.diagnostics.DiagnosticsSnapshot
import ai.zara.app.diagnostics.DiagnosticsV2
import ai.zara.app.telemetry.ClientEventNames
import ai.zara.app.telemetry.SessionTelemetry
import ai.zara.app.telemetry.ZaraFailure
import ai.zara.app.telemetry.ZaraFailureCodes
import ai.zara.app.telemetry.ZaraFailures
import ai.zara.app.telemetry.ZaraOperation
import java.util.ArrayDeque
import java.util.concurrent.TimeUnit
import org.junit.Assert.assertEquals
import org.junit.Assert.assertFalse
import org.junit.Assert.assertNotNull
import org.junit.Assert.assertNull
import org.junit.Assert.assertTrue
import org.junit.Test

class ZaraTextClientProtocolEvidenceTest {
    @Test fun `unexpected reply survives sparse callback reconnect and export without payloads`() {
        val unexpected = server("""{"type":"assistant.delta","id":"delta-1","session_id":"session-7","turn_id":"turn-7","seq":4,"timestamp_ns":3,"payload_count":0,"body":{"text":"PRIVATE_REPLY_SENTINEL"}}""")
        val telemetry = SessionTelemetry()
        val dealers = ArrayDeque(listOf(
            EvidenceDealer(handshake("session-7", "hello-7", "caps-7") + listOf(unexpected)),
            EvidenceDealer(handshake("session-8", "hello-8", "caps-8")),
        ))
        val client = client(TextDealerFactory { dealers.removeFirst() })
        client.setConnectionFailureObserver { telemetry.onClientFailure(it, ClientEventNames.PROTOCOL_FAILED) }
        try {
            client.connect(profile(), 7).get(5, TimeUnit.SECONDS)
            val error = failureOf { client.submitText(7, "session-7", null, "PRIVATE_PROMPT_SENTINEL").get(5, TimeUnit.SECONDS) }
            val first = requireNotNull(telemetry.primaryIncident())
            val evidence = requireNotNull(first.failure.protocolEvidence)
            assertEquals(ZaraFailureCodes.PROTOCOL_UNEXPECTED_MESSAGE, first.failure.code)
            assertEquals("awaiting_turn_accepted", first.failure.phase)
            assertEquals("submit-7", first.failure.requestId)
            assertEquals(7L, first.failure.connectionGeneration)
            assertEquals("assistant.delta", evidence.lastRx?.messageType)
            assertEquals("delta-1", evidence.lastRx?.messageId)
            assertEquals(4L, evidence.lastRx?.sequence)
            assertEquals(unexpected.sumOf { it.size.toLong() }, evidence.lastRx?.bytes)
            assertEquals("turn.submit", evidence.lastTx?.messageType)
            assertNull(first.failure.turnId)

            telemetry.onClientFailure(ZaraFailures.classify(error, ZaraOperation.SUBMIT), ClientEventNames.PROTOCOL_FAILED)
            assertEquals(first, telemetry.primaryIncident())
            assertEquals(1, telemetry.journal().snapshot().count { it.name == ClientEventNames.PROTOCOL_FAILED })

            client.connect(profile(), 8).get(5, TimeUnit.SECONDS)
            assertEquals(evidence, telemetry.primaryIncident()?.failure?.protocolEvidence)
            val bundle = DiagnosticsV2.render(DiagnosticsSnapshot(
                version = "0.2.2-alpha", versionCode = 4, sourceSha = "test-source",
                runtimeMode = "remote", sessionId = "session-8", sessionGeneration = 8,
                connectionPhase = "connected", enrollmentPhase = "ready",
                incident = telemetry.primaryIncident(), remoteContext = null, voiceStages = emptyList(),
                localAiPhase = "not_applicable", localAiNote = null, localAiGeneration = null,
                localAiModel = null, localServerPhase = "ready", localServerGeneration = 1,
                localServerFailure = null, events = telemetry.journal().snapshot(),
                diagnosticId = "diag-test", capturedAtMillis = 1_000,
            ))
            assertTrue(bundle.text.contains("primary_failure.expected_message_type=turn.accepted"))
            assertTrue(bundle.text.contains("primary_failure.actual_message_type=assistant.delta"))
            assertTrue(bundle.text.contains("primary_failure.failure_session_id=session-7"))
            assertTrue(bundle.text.contains("session_id=session-8"))
            assertTrue(bundle.json.contains("\"protocol_failure_trace\":["))
            assertFalse(bundle.text.contains("PRIVATE_REPLY_SENTINEL"))
            assertFalse(bundle.json.contains("PRIVATE_REPLY_SENTINEL"))
            assertFalse(bundle.text.contains("PRIVATE_PROMPT_SENTINEL"))
            assertFalse(bundle.json.contains("PRIVATE_PROMPT_SENTINEL"))
        } finally {
            client.close()
        }
    }

    @Test fun `malformed reply records frame sizes before decoding fails`() {
        val malformed = listOf("ZARA/1".encodeToByteArray(), byteArrayOf(0x7f))
        withFailure(listOf(malformed)) { failure ->
            assertEquals(ZaraFailureCodes.PROTOCOL_MALFORMED, failure.code)
            val lastRx = requireNotNull(failure.protocolEvidence?.lastRx)
            assertNull(lastRx.messageType)
            assertEquals("undecoded", lastRx.state)
            assertEquals(7L, lastRx.bytes)
            assertEquals(2, lastRx.frameCount)
            assertEquals("submit-7", failure.requestId)
        }
    }

    @Test fun `wrong receipt correlation retains both expected and actual request ids`() {
        withFailure(listOf(receipt("other-request"))) { failure ->
            assertEquals(ZaraFailureCodes.PROTOCOL_OUT_OF_ORDER, failure.code)
            assertEquals("submit-7", failure.requestId)
            assertEquals("other-request", failure.protocolEvidence?.lastRx?.replyTo)
            assertEquals("turn.accepted", failure.protocolEvidence?.expectedMessageType)
            assertEquals(1, failure.protocolEvidence?.pendingRequests)
            assertNull(failure.turnId)
        }
    }

    @Test fun `validated acceptance switches to completion phase and retains turn id`() {
        val fatal = server("""{"type":"runtime.error","id":"runtime-error","session_id":"session-7","timestamp_ns":5,"payload_count":0,"body":{"fatal":true,"reason":"fixture failure"}}""")
        withFailure(listOf(receipt("submit-7"), fatal)) { failure ->
            assertEquals(ZaraFailureCodes.PROTOCOL_RUNTIME_ERROR, failure.code)
            assertEquals("awaiting_turn_completion", failure.phase)
            assertEquals("turn-7", failure.turnId)
            assertEquals(0, failure.protocolEvidence?.pendingRequests)
            assertEquals("runtime.error", failure.protocolEvidence?.lastRx?.messageType)
        }
    }

    private fun withFailure(replies: List<List<ByteArray>>, verify: (ZaraFailure) -> Unit) {
        val dealer = EvidenceDealer(handshake("session-7", "hello-7", "caps-7") + replies)
        val client = client(TextDealerFactory { dealer })
        var reported: ZaraFailure? = null
        client.setConnectionFailureObserver { reported = it }
        try {
            client.connect(profile(), 7).get(5, TimeUnit.SECONDS)
            failureOf { client.submitText(7, "session-7", null, "hello").get(5, TimeUnit.SECONDS) }
            assertNotNull(reported)
            verify(requireNotNull(reported))
        } finally {
            client.close()
        }
    }

    private fun client(factory: TextDealerFactory) = ZaraTextClientActor(
        dealerFactory = factory,
        requestIds = sequenceOf("hello-7", "caps-7", "submit-7", "hello-8", "caps-8").iterator(),
        timestamps = generateSequence(1L) { it + 1L }.iterator(),
    )

    private fun profile() = ServerProfile.create("tcp://zara.example:7731")

    private fun failureOf(block: () -> Unit): Throwable {
        try {
            block()
        } catch (error: java.util.concurrent.ExecutionException) {
            return error
        }
        throw AssertionError("expected the request to fail")
    }

    private fun handshake(session: String, hello: String, caps: String): List<List<ByteArray>> = listOf(
        server("""{"type":"hello.ok","id":"hello-ok","reply_to":"$hello","session_id":"$session","timestamp_ns":1,"payload_count":0,"body":{"version":1,"max_payload_frames":16,"max_payload_frame_bytes":1048576,"max_payload_bytes":4194304}}"""),
        server("""{"type":"capability.snapshot.ok","id":"caps-ok","reply_to":"$caps","session_id":"$session","timestamp_ns":2,"payload_count":0,"body":{"capabilities":[]}}"""),
    )

    private fun receipt(reply: String) = server("""{"type":"turn.accepted","id":"receipt-7","reply_to":"$reply","session_id":"session-7","turn_id":"turn-7","timestamp_ns":3,"payload_count":0}""")

    private fun server(json: String) = listOf("ZARA/1".encodeToByteArray(), json.encodeToByteArray())
}

private class EvidenceDealer(responses: List<List<ByteArray>>) : TextDealer {
    private val responses = ArrayDeque(responses)
    override fun send(frames: List<ByteArray>) = Unit
    override fun receive(timeoutMillis: Int): List<ByteArray>? = responses.pollFirst()
    override fun close() = Unit
}
