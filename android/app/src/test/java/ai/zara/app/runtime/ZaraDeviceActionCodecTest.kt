package ai.zara.app.runtime

import ai.zara.app.device.DeviceActionArguments
import ai.zara.app.device.DeviceActionErrorCode
import org.junit.Assert.assertEquals
import org.junit.Assert.assertThrows
import org.junit.Test

class ZaraDeviceActionCodecTest {
    @Test
    fun `decodes strict open_uri action request`() {
        val message = ZaraDeviceActionCodec.decodeServerMessage(
            frames(
                """{"body":{"action_id":"action-1","args":{"uri":"https://example.com"},"capability":"open_uri","deadline_ns":999999999999999999,"idempotency":"at_most_once"},"id":"request-1","payload_count":0,"session_id":"session-1","timestamp_ns":4,"trace_id":"trace-1","type":"device.action.request"}"""
            )
        )

        assertEquals(
            DeviceServerMessage.Request(
                id = "request-1",
                sessionId = "session-1",
                traceId = "trace-1",
                actionId = "action-1",
                capability = DeviceCapability.OpenUri,
                arguments = DeviceActionArguments.OpenUri("https://example.com"),
                deadlineNs = 999999999999999999,
                idempotency = DeviceActionIdempotency.AtMostOnce,
            ),
            message,
        )
    }

    @Test
    fun `decodes strict action cancel`() {
        val message = ZaraDeviceActionCodec.decodeServerMessage(
            frames(
                """{"body":{"action_id":"action-1","reason":"operator cancelled"},"id":"cancel-1","payload_count":0,"session_id":"session-1","timestamp_ns":5,"type":"device.action.cancel"}"""
            )
        )

        assertEquals(
            DeviceServerMessage.Cancel(
                id = "cancel-1",
                sessionId = "session-1",
                actionId = "action-1",
                reason = "operator cancelled",
            ),
            message,
        )
    }

    @Test
    fun `unknown fields executable args and unsupported capability fail closed`() {
        listOf(
            """{"body":{"action_id":"a","args":{"uri":"https://example.com","shell":"id"},"capability":"open_uri","deadline_ns":9,"idempotency":"at_most_once"},"id":"r","payload_count":0,"session_id":"s","timestamp_ns":1,"type":"device.action.request"}""",
            """{"body":{"action_id":"a","args":{},"capability":"admin","deadline_ns":9,"idempotency":"at_most_once"},"id":"r","payload_count":0,"session_id":"s","timestamp_ns":1,"type":"device.action.request"}""",
            """{"body":{"action_id":"a","args":{"uri":"https://example.com"},"capability":"open_uri","deadline_ns":9,"idempotency":"at_most_once"},"evil":true,"id":"r","payload_count":0,"session_id":"s","timestamp_ns":1,"type":"device.action.request"}""",
        ).forEach { envelope ->
            assertThrows(ZaraWireException::class.java) {
                ZaraDeviceActionCodec.decodeServerMessage(frames(envelope))
            }
        }
    }

    @Test
    fun `encodes accepted completed and typed error canonically`() {
        assertEquals(
            """{"body":{"action_id":"action-1"},"id":"accepted-1","payload_count":0,"session_id":"session-1","timestamp_ns":6,"type":"device.action.accepted"}""",
            envelope(
                ZaraDeviceActionCodec.encodeAccepted(
                    requestId = "accepted-1",
                    sessionId = "session-1",
                    actionId = "action-1",
                    timestampNs = 6,
                )
            ),
        )
        assertEquals(
            """{"body":{"action_id":"action-1","outcome":"completed"},"id":"result-1","payload_count":0,"session_id":"session-1","timestamp_ns":7,"type":"device.action.result"}""",
            envelope(
                ZaraDeviceActionCodec.encodeCompleted(
                    requestId = "result-1",
                    sessionId = "session-1",
                    actionId = "action-1",
                    timestampNs = 7,
                )
            ),
        )
        assertEquals(
            """{"body":{"action_id":"action-1","code":"permission_denied"},"id":"error-1","payload_count":0,"session_id":"session-1","timestamp_ns":8,"type":"device.action.error"}""",
            envelope(
                ZaraDeviceActionCodec.encodeError(
                    requestId = "error-1",
                    sessionId = "session-1",
                    actionId = "action-1",
                    code = DeviceActionErrorCode.PermissionDenied,
                    message = null,
                    timestampNs = 8,
                )
            ),
        )
    }

    private fun frames(envelope: String): List<ByteArray> =
        listOf("ZARA/1".encodeToByteArray(), envelope.encodeToByteArray())

    private fun envelope(frames: List<ByteArray>): String {
        assertEquals("ZARA/1", frames[0].decodeToString())
        return frames[1].decodeToString()
    }
}
