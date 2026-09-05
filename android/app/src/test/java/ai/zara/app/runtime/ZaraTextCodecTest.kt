package ai.zara.app.runtime

import org.junit.Assert.assertEquals
import org.junit.Assert.assertThrows
import org.junit.Test

class ZaraTextCodecTest {

    @Test fun `hello is canonical ZARA1 multipart`() {
        val frames = ZaraTextCodec.encodeHello("hello-1", 1)
        assertEquals("ZARA/1", frames[0].decodeToString())
        assertEquals(
            "{\"body\":{\"versions\":[1]},\"id\":\"hello-1\",\"payload_count\":0,\"timestamp_ns\":1,\"type\":\"hello\"}",
            frames[1].decodeToString(),
        )
    }

    @Test fun `capability snapshot is closed sorted and session bound`() {
        val frames = ZaraCapabilityCodec.encodeSnapshot(
            requestId = "caps-1",
            sessionId = "session-1",
            capabilities = setOf(DeviceCapability.OpenUri, DeviceCapability.OpenApp),
            timestampNs = 2,
        )
        assertEquals(
            "{\"body\":{\"capabilities\":[{\"id\":\"open_app\",\"version\":1},{\"id\":\"open_uri\",\"version\":1}]},\"id\":\"caps-1\",\"payload_count\":0,\"session_id\":\"session-1\",\"timestamp_ns\":2,\"type\":\"capability.snapshot\"}",
            frames[1].decodeToString(),
        )

        val ack = ZaraCapabilityCodec.decodeSnapshotOk(
            frames(
                "{\"body\":{\"capabilities\":[{\"id\":\"open_app\",\"version\":1},{\"id\":\"open_uri\",\"version\":1}]},\"id\":\"caps-ok\",\"payload_count\":0,\"reply_to\":\"caps-1\",\"session_id\":\"session-1\",\"timestamp_ns\":3,\"type\":\"capability.snapshot.ok\"}"
            )
        )
        assertEquals(
            CapabilitySnapshotOk(
                id = "caps-ok",
                replyTo = "caps-1",
                sessionId = "session-1",
                capabilities = setOf(DeviceCapability.OpenApp, DeviceCapability.OpenUri),
            ),
            ack,
        )
    }

    @Test fun `capability snapshot rejects unknown ids versions fields and duplicates`() {
        listOf(
            "[{\"id\":\"admin\",\"version\":1}]",
            "[{\"id\":\"open_uri\",\"version\":2}]",
            "[{\"authority\":\"admin\",\"id\":\"open_uri\",\"version\":1}]",
            "[{\"id\":\"open_uri\",\"version\":1},{\"id\":\"open_uri\",\"version\":1}]",
        ).forEach { capabilities ->
            assertThrows(ZaraWireException::class.java) {
                ZaraCapabilityCodec.decodeSnapshotOk(
                    frames(
                        "{\"body\":{\"capabilities\":$capabilities},\"id\":\"caps-ok\",\"payload_count\":0,\"reply_to\":\"caps-1\",\"session_id\":\"session-1\",\"timestamp_ns\":3,\"type\":\"capability.snapshot.ok\"}"
                    )
                )
            }
        }
    }

    @Test fun `turn submit carries current session conversation and escaped text`() {
        val frames = ZaraTextCodec.encodeTurnSubmit(
            requestId = "req-1",
            sessionId = "session-1",
            conversationId = "conversation-1",
            text = "say \"hi\"\nnow",
            timestampNs = 9,
        )
        assertEquals(
            "{\"body\":{\"text\":\"say \\\"hi\\\"\\nnow\"},\"conversation_id\":\"conversation-1\",\"id\":\"req-1\",\"payload_count\":0,\"session_id\":\"session-1\",\"timestamp_ns\":9,\"type\":\"turn.submit\"}",
            frames[1].decodeToString(),
        )
    }

    @Test fun `hello ok decodes session and negotiated limits`() {
        val message = ZaraTextCodec.decode(
            frames(
                "{\"body\":{\"max_payload_bytes\":4194304,\"max_payload_frame_bytes\":1048576,\"max_payload_frames\":16,\"version\":1},\"id\":\"hello-ok\",\"payload_count\":0,\"reply_to\":\"hello-1\",\"session_id\":\"session-1\",\"timestamp_ns\":2,\"type\":\"hello.ok\"}"
            )
        )
        assertEquals(
            TextServerMessage.HelloOk(
                id = "hello-ok",
                replyTo = "hello-1",
                sessionId = "session-1",
                version = 1,
                maxPayloadFrames = 16,
                maxPayloadFrameBytes = 1048576,
                maxPayloadBytes = 4194304,
            ),
            message,
        )
    }

    @Test fun `turn acceptance and assistant text events preserve correlation`() {
        val accepted = ZaraTextCodec.decode(
            frames(
                "{\"conversation_id\":\"conversation-1\",\"id\":\"accepted-1\",\"payload_count\":0,\"reply_to\":\"req-1\",\"session_id\":\"session-1\",\"timestamp_ns\":3,\"turn_id\":\"turn-1\",\"type\":\"turn.accepted\"}"
            )
        )
        assertEquals(
            TextServerMessage.TurnAccepted(
                id = "accepted-1",
                replyTo = "req-1",
                sessionId = "session-1",
                conversationId = "conversation-1",
                turnId = "turn-1",
            ),
            accepted,
        )

        val delta = ZaraTextCodec.decode(
            frames(
                "{\"body\":{\"text\":\"hello\"},\"conversation_id\":\"conversation-1\",\"id\":\"delta-1\",\"payload_count\":0,\"seq\":4,\"session_id\":\"session-1\",\"timestamp_ns\":4,\"turn_id\":\"turn-1\",\"type\":\"assistant.delta\"}"
            )
        )
        assertEquals(
            TextServerMessage.AssistantDelta(
                id = "delta-1",
                sessionId = "session-1",
                conversationId = "conversation-1",
                turnId = "turn-1",
                sequence = 4,
                text = "hello",
            ),
            delta,
        )

        val completed = ZaraTextCodec.decode(
            frames(
                "{\"body\":{\"success\":true,\"text\":\"hello world\"},\"conversation_id\":\"conversation-1\",\"id\":\"complete-1\",\"payload_count\":0,\"seq\":5,\"session_id\":\"session-1\",\"timestamp_ns\":5,\"turn_id\":\"turn-1\",\"type\":\"assistant.completed\"}"
            )
        )
        assertEquals(
            TextServerMessage.AssistantCompleted(
                id = "complete-1",
                sessionId = "session-1",
                conversationId = "conversation-1",
                turnId = "turn-1",
                sequence = 5,
                text = "hello world",
                success = true,
            ),
            completed,
        )
    }

    @Test fun `protocol error is typed and content bounded`() {
        val message = ZaraTextCodec.decode(
            frames(
                "{\"body\":{\"code\":\"authentication_required\",\"message\":\"authentication required\",\"retryable\":false},\"id\":\"error-1\",\"payload_count\":0,\"reply_to\":\"req-1\",\"session_id\":\"session-1\",\"timestamp_ns\":6,\"type\":\"protocol.error\"}"
            )
        )
        assertEquals(
            TextServerMessage.ProtocolError(
                id = "error-1",
                replyTo = "req-1",
                sessionId = "session-1",
                code = "authentication_required",
                message = "authentication required",
                retryable = false,
            ),
            message,
        )
    }

    @Test fun `wrong marker payloads duplicate keys unknown fields and unknown types fail closed`() {
        assertThrows(ZaraWireException::class.java) {
            ZaraTextCodec.decode(listOf("NOPE".encodeToByteArray(), "{}".encodeToByteArray()))
        }
        assertThrows(ZaraWireException::class.java) {
            ZaraTextCodec.decode(listOf("ZARA/1".encodeToByteArray(), "{}".encodeToByteArray(), byteArrayOf(1)))
        }
        assertThrows(ZaraWireException::class.java) {
            ZaraTextCodec.decode(frames("{\"id\":\"a\",\"id\":\"b\",\"payload_count\":0,\"timestamp_ns\":1,\"type\":\"hello.ok\"}"))
        }
        assertThrows(ZaraWireException::class.java) {
            ZaraTextCodec.decode(frames("{\"evil\":true,\"id\":\"a\",\"payload_count\":0,\"timestamp_ns\":1,\"type\":\"hello.ok\"}"))
        }
        assertThrows(ZaraWireException::class.java) {
            ZaraTextCodec.decode(frames("{\"id\":\"a\",\"payload_count\":0,\"timestamp_ns\":1,\"type\":\"admin.shell\"}"))
        }
    }

    @Test fun `malformed utf8 oversized envelopes and nonzero payload count fail closed`() {
        assertThrows(ZaraWireException::class.java) {
            ZaraTextCodec.decode(listOf("ZARA/1".encodeToByteArray(), byteArrayOf(0xC3.toByte(), 0x28)))
        }
        assertThrows(ZaraWireException::class.java) {
            ZaraTextCodec.decode(frames("x".repeat(65 * 1024)))
        }
        assertThrows(ZaraWireException::class.java) {
            ZaraTextCodec.decode(frames("{\"id\":\"a\",\"payload_count\":1,\"timestamp_ns\":1,\"type\":\"hello.ok\"}"))
        }
    }

    private fun frames(json: String): List<ByteArray> =
        listOf("ZARA/1".encodeToByteArray(), json.encodeToByteArray())
}
