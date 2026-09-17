package ai.zara.app.runtime

import ai.zara.app.device.DeviceActionArguments
import org.junit.Assert.assertEquals
import org.junit.Assert.assertThrows
import org.junit.Test

class CalendarInsertWireContractTest {
    @Test
    fun `decodes closed calendar insert capability`() {
        val message = ZaraDeviceActionCodec.decodeServerMessage(
            frames(
                """{"body":{"action_id":"calendar-1","args":{"description":"Review roadmap","end_ms":1795096800000,"location":"Room 7","start_ms":1795093200000,"title":"Planning"},"capability":"calendar_insert","deadline_ns":999999999999999999,"idempotency":"at_most_once"},"id":"request-1","payload_count":0,"session_id":"session-1","timestamp_ns":4,"type":"device.action.request"}"""
            )
        )

        assertEquals(
            DeviceServerMessage.Request(
                id = "request-1",
                sessionId = "session-1",
                traceId = null,
                actionId = "calendar-1",
                capability = DeviceCapability.CalendarInsert,
                arguments = DeviceActionArguments.CalendarInsert(
                    title = "Planning",
                    startMillis = 1_795_093_200_000,
                    endMillis = 1_795_096_800_000,
                    location = "Room 7",
                    description = "Review roadmap",
                ),
                deadlineNs = 999999999999999999,
                idempotency = DeviceActionIdempotency.AtMostOnce,
            ),
            message,
        )
    }

    @Test
    fun `nullable calendar fields are data not implementation authority`() {
        val message = ZaraDeviceActionCodec.decodeServerMessage(
            frames(
                """{"body":{"action_id":"calendar-1","args":{"description":null,"end_ms":1795096800000,"location":null,"start_ms":1795093200000,"title":"Planning"},"capability":"calendar_insert","deadline_ns":999999999999999999,"idempotency":"at_most_once"},"id":"request-1","payload_count":0,"session_id":"session-1","timestamp_ns":4,"type":"device.action.request"}"""
            )
        ) as DeviceServerMessage.Request

        assertEquals(null, (message.arguments as DeviceActionArguments.CalendarInsert).location)
        assertEquals(null, message.arguments.description)
    }

    @Test
    fun `raw intent package component and unknown fields fail closed`() {
        listOf(
            """{"body":{"action_id":"a","args":{"description":null,"end_ms":1795096800000,"intent_action":"android.intent.action.DELETE","location":null,"start_ms":1795093200000,"title":"Planning"},"capability":"calendar_insert","deadline_ns":9,"idempotency":"at_most_once"},"id":"r","payload_count":0,"session_id":"s","timestamp_ns":1,"type":"device.action.request"}""",
            """{"body":{"action_id":"a","args":{"component":"evil/.Receiver","description":null,"end_ms":1795096800000,"location":null,"start_ms":1795093200000,"title":"Planning"},"capability":"calendar_insert","deadline_ns":9,"idempotency":"at_most_once"},"id":"r","payload_count":0,"session_id":"s","timestamp_ns":1,"type":"device.action.request"}""",
            """{"body":{"action_id":"a","args":{"description":null,"end_ms":1795096800000,"location":null,"package":"evil.app","start_ms":1795093200000,"title":"Planning"},"capability":"calendar_insert","deadline_ns":9,"idempotency":"at_most_once"},"id":"r","payload_count":0,"session_id":"s","timestamp_ns":1,"type":"device.action.request"}""",
        ).forEach { envelope ->
            assertThrows(ZaraWireException::class.java) {
                ZaraDeviceActionCodec.decodeServerMessage(frames(envelope))
            }
        }
    }

    @Test
    fun `calendar title and optional text are bounded before adapter dispatch`() {
        listOf(
            "x".repeat(513),
            "bad\u0000title",
        ).forEach { title ->
            assertThrows(ZaraWireException::class.java) {
                ZaraDeviceActionCodec.decodeServerMessage(
                    frames(
                        """{"body":{"action_id":"a","args":{"description":null,"end_ms":1795096800000,"location":null,"start_ms":1795093200000,"title":${json(title)}},"capability":"calendar_insert","deadline_ns":9,"idempotency":"at_most_once"},"id":"r","payload_count":0,"session_id":"s","timestamp_ns":1,"type":"device.action.request"}"""
                    )
                )
            }
        }
    }

    private fun json(value: String): String = buildString {
        append('"')
        value.forEach { character ->
            when (character) {
                '"' -> append("\\\"")
                '\\' -> append("\\\\")
                '\u0000' -> append("\\u0000")
                else -> append(character)
            }
        }
        append('"')
    }

    private fun frames(envelope: String): List<ByteArray> =
        listOf("ZARA/1".encodeToByteArray(), envelope.encodeToByteArray())
}
