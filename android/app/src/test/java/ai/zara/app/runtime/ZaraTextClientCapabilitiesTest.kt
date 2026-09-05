package ai.zara.app.runtime

import java.util.ArrayDeque
import java.util.concurrent.TimeUnit
import org.junit.Assert.assertFalse
import org.junit.Assert.assertTrue
import org.junit.Test

class ZaraTextClientCapabilitiesTest {
    @Test
    fun `connect advertises only currently available executable capabilities`() {
        val dealer = CapabilityDealer(
            listOf(
                server(
                    """{"body":{"max_payload_bytes":4194304,"max_payload_frame_bytes":1048576,"max_payload_frames":16,"version":1},"id":"hello-ok","payload_count":0,"reply_to":"hello-1","session_id":"session-1","timestamp_ns":1,"type":"hello.ok"}"""
                ),
                server(
                    """{"body":{"capabilities":[{"id":"open_uri","version":1}]},"id":"caps-ok","payload_count":0,"reply_to":"caps-1","session_id":"session-1","timestamp_ns":2,"type":"capability.snapshot.ok"}"""
                ),
            )
        )
        val client = ZaraTextClientActor(
            dealerFactory = TextDealerFactory { dealer },
            requestIds = listOf("hello-1", "caps-1").iterator(),
            timestamps = listOf(1L, 2L).iterator(),
            deviceCapabilities = { setOf(DeviceCapability.OpenUri) },
        )

        try {
            client.connect(ServerProfile.create("tcp://127.0.0.1:5555"), 1)
                .get(1, TimeUnit.SECONDS)

            val snapshot = dealer.sent[1][1].decodeToString()
            assertTrue(snapshot.contains("\"capabilities\":[{\"id\":\"open_uri\",\"version\":1}]"))
            assertFalse(snapshot.contains("open_app"))
        } finally {
            client.close()
        }
    }

    private fun server(envelope: String): List<ByteArray> =
        listOf("ZARA/1".encodeToByteArray(), envelope.encodeToByteArray())

    private class CapabilityDealer(responses: List<List<ByteArray>>) : TextDealer {
        private val responses = ArrayDeque(responses)
        val sent = mutableListOf<List<ByteArray>>()

        override fun send(frames: List<ByteArray>) {
            sent += frames.map(ByteArray::copyOf)
        }

        override fun receive(timeoutMillis: Int): List<ByteArray>? =
            if (responses.isEmpty()) null else responses.removeFirst()

        override fun close() = Unit
    }
}
