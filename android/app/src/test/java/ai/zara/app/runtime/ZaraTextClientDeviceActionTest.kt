package ai.zara.app.runtime

import ai.zara.app.device.DeviceActionResult
import java.util.ArrayDeque
import java.util.concurrent.ExecutionException
import java.util.concurrent.TimeUnit
import org.junit.Assert.assertEquals
import org.junit.Assert.assertThrows
import org.junit.Assert.assertTrue
import org.junit.Test

class ZaraTextClientDeviceActionTest {
    @Test
    fun `interleaved device action is accepted executed and completed on the same dealer`() {
        val dealer = DeviceActionDealer(
            listOf(
                helloOk(),
                capabilityOk(),
                turnAccepted(),
                actionRequest(deadlineNs = 9_999_999_999),
                assistantCompleted(),
                turnCompleted(),
            )
        )
        val handler = RecordingDeviceActionHandler()
        val client = ZaraTextClientActor(
            dealerFactory = TextDealerFactory { dealer },
            requestIds = listOf("hello-1", "caps-1", "turn-1", "accepted-1", "result-1").iterator(),
            timestamps = listOf(1L, 2L, 3L, 4L, 5L).iterator(),
            deviceCapabilities = handler::availableCapabilities,
            deviceActionHandler = handler,
            epochNanoseconds = { 1L },
        )

        try {
            client.connect(ServerProfile.create("tcp://127.0.0.1:5555"), 1).get(1, TimeUnit.SECONDS)
            val result = client.submitText(1, "session-1", null, "open it").get(1, TimeUnit.SECONDS)

            assertEquals("done", result.text)
            assertEquals(listOf("action-1"), handler.executed)
            assertTrue(dealer.envelope(3).contains("\"type\":\"device.action.accepted\""))
            assertTrue(dealer.envelope(4).contains("\"type\":\"device.action.result\""))
        } finally {
            client.close()
        }
    }

    @Test
    fun `expired interleaved action fails closed before handler or acknowledgement`() {
        val dealer = DeviceActionDealer(
            listOf(
                helloOk(),
                capabilityOk(),
                turnAccepted(),
                actionRequest(deadlineNs = 10),
            )
        )
        val handler = RecordingDeviceActionHandler()
        val client = ZaraTextClientActor(
            dealerFactory = TextDealerFactory { dealer },
            requestIds = listOf("hello-1", "caps-1", "turn-1").iterator(),
            timestamps = listOf(1L, 2L, 3L).iterator(),
            deviceCapabilities = handler::availableCapabilities,
            deviceActionHandler = handler,
            epochNanoseconds = { 11L },
        )

        try {
            client.connect(ServerProfile.create("tcp://127.0.0.1:5555"), 1).get(1, TimeUnit.SECONDS)
            assertThrows(ExecutionException::class.java) {
                client.submitText(1, "session-1", null, "open it").get(1, TimeUnit.SECONDS)
            }
            assertEquals(emptyList<String>(), handler.executed)
            assertEquals(3, dealer.sent.size)
        } finally {
            client.close()
        }
    }

    private class RecordingDeviceActionHandler : DeviceActionHandler {
        val executed = mutableListOf<String>()

        override fun availableCapabilities(): Set<DeviceCapability> = setOf(DeviceCapability.OpenUri)

        override fun execute(request: DeviceServerMessage.Request): DeviceActionResult {
            executed += request.actionId
            return DeviceActionResult.Completed
        }

        override fun cancel(cancel: DeviceServerMessage.Cancel) = Unit
    }

    private class DeviceActionDealer(responses: List<List<ByteArray>>) : TextDealer {
        private val responses = ArrayDeque(responses)
        val sent = mutableListOf<List<ByteArray>>()

        override fun send(frames: List<ByteArray>) {
            sent += frames.map(ByteArray::copyOf)
        }

        override fun receive(timeoutMillis: Int): List<ByteArray>? =
            if (responses.isEmpty()) null else responses.removeFirst()

        override fun close() = Unit

        fun envelope(index: Int): String = sent[index][1].decodeToString()
    }

    companion object {
        private fun frame(envelope: String): List<ByteArray> =
            listOf("ZARA/1".encodeToByteArray(), envelope.encodeToByteArray())

        private fun helloOk() = frame(
            """{"body":{"max_payload_bytes":4194304,"max_payload_frame_bytes":1048576,"max_payload_frames":16,"version":1},"id":"hello-ok","payload_count":0,"reply_to":"hello-1","session_id":"session-1","timestamp_ns":1,"type":"hello.ok"}"""
        )

        private fun capabilityOk() = frame(
            """{"body":{"capabilities":[{"id":"open_uri","version":1}]},"id":"caps-ok","payload_count":0,"reply_to":"caps-1","session_id":"session-1","timestamp_ns":2,"type":"capability.snapshot.ok"}"""
        )

        private fun turnAccepted() = frame(
            """{"body":{},"conversation_id":"conversation-1","id":"turn-ok","payload_count":0,"reply_to":"turn-1","session_id":"session-1","timestamp_ns":3,"turn_id":"turn-id","type":"turn.accepted"}"""
        )

        private fun actionRequest(deadlineNs: Long) = frame(
            """{"body":{"action_id":"action-1","args":{"uri":"https://example.com"},"capability":"open_uri","deadline_ns":$deadlineNs,"idempotency":"at_most_once"},"id":"action-request","payload_count":0,"session_id":"session-1","timestamp_ns":4,"trace_id":"trace-1","type":"device.action.request"}"""
        )

        private fun assistantCompleted() = frame(
            """{"body":{"success":true,"text":"done"},"conversation_id":"conversation-1","id":"assistant-done","payload_count":0,"seq":1,"session_id":"session-1","timestamp_ns":5,"turn_id":"turn-id","type":"assistant.completed"}"""
        )

        private fun turnCompleted() = frame(
            """{"body":{"success":true},"conversation_id":"conversation-1","id":"turn-done","payload_count":0,"seq":2,"session_id":"session-1","timestamp_ns":6,"turn_id":"turn-id","type":"turn.completed"}"""
        )
    }
}
