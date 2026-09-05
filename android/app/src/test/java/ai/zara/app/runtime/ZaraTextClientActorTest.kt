package ai.zara.app.runtime

import java.util.ArrayDeque
import org.junit.Assert.assertEquals
import org.junit.Assert.assertThrows
import org.junit.Test

class ZaraTextClientActorTest {

    @Test fun `connect negotiates an empty capability snapshot before becoming ready`() {
        val dealer = ScriptedTextDealer(
            responses = listOf(
                server("{\"body\":{\"max_payload_bytes\":4194304,\"max_payload_frame_bytes\":1048576,\"max_payload_frames\":16,\"version\":1},\"id\":\"hello-ok\",\"payload_count\":0,\"reply_to\":\"req-1\",\"session_id\":\"session-1\",\"timestamp_ns\":2,\"type\":\"hello.ok\"}"),
                capabilityAck("caps-ok", "req-caps", "session-1", 3),
            )
        )
        val client = ZaraTextClientActor(
            dealerFactory = TextDealerFactory { dealer },
            requestIds = sequenceOf("req-1", "req-caps").iterator(),
            timestamps = sequenceOf(1L, 2L).iterator(),
        )

        assertEquals(
            ConnectedTextSession(1, "session-1"),
            client.connect(ServerProfile.create("tcp://zara.example:7731"), 1).get(),
        )
        assertEquals(2, dealer.sent.size)
        assertEquals(true, dealer.sent[1][1].decodeToString().contains("\"type\":\"capability.snapshot\""))
        assertEquals(true, dealer.sent[1][1].decodeToString().contains("\"capabilities\":[]"))
        client.close()
    }

    @Test fun `actor performs hello then real typed text turn on one dealer owner`() {
        val dealer = ScriptedTextDealer(
            responses = listOf(
                server("{\"body\":{\"max_payload_bytes\":4194304,\"max_payload_frame_bytes\":1048576,\"max_payload_frames\":16,\"version\":1},\"id\":\"hello-ok\",\"payload_count\":0,\"reply_to\":\"req-1\",\"session_id\":\"session-1\",\"timestamp_ns\":2,\"type\":\"hello.ok\"}"),
                capabilityAck("caps-ok", "req-caps", "session-1", 3),
                server("{\"conversation_id\":\"conversation-1\",\"id\":\"accepted-1\",\"payload_count\":0,\"reply_to\":\"req-2\",\"session_id\":\"session-1\",\"timestamp_ns\":4,\"turn_id\":\"turn-1\",\"type\":\"turn.accepted\"}"),
                server("{\"body\":{\"text\":\"hel\"},\"conversation_id\":\"conversation-1\",\"id\":\"delta-1\",\"payload_count\":0,\"seq\":1,\"session_id\":\"session-1\",\"timestamp_ns\":5,\"turn_id\":\"turn-1\",\"type\":\"assistant.delta\"}"),
                server("{\"body\":{\"success\":true,\"text\":\"hello\"},\"conversation_id\":\"conversation-1\",\"id\":\"done-1\",\"payload_count\":0,\"seq\":2,\"session_id\":\"session-1\",\"timestamp_ns\":6,\"turn_id\":\"turn-1\",\"type\":\"assistant.completed\"}"),
                server("{\"body\":{\"success\":true},\"conversation_id\":\"conversation-1\",\"id\":\"turn-done-1\",\"payload_count\":0,\"seq\":3,\"session_id\":\"session-1\",\"timestamp_ns\":7,\"turn_id\":\"turn-1\",\"type\":\"turn.completed\"}"),
            )
        )
        val client = ZaraTextClientActor(
            dealerFactory = TextDealerFactory { endpoint ->
                assertEquals("tcp://zara.example:7731", endpoint)
                dealer
            },
            requestIds = sequenceOf("req-1", "req-caps", "req-2").iterator(),
            timestamps = sequenceOf(1L, 2L, 9L).iterator(),
        )

        val connected = client.connect(ServerProfile.create("tcp://zara.example:7731"), generation = 4).get()
        assertEquals(ConnectedTextSession(4, "session-1"), connected)

        val result = client.submitText(
            generation = 4,
            sessionId = "session-1",
            conversationId = "conversation-1",
            text = "hello",
        ).get()
        assertEquals(TextTurnResult("conversation-1", "turn-1", "hello", true), result)
        assertEquals(3, dealer.sent.size)
        assertEquals(true, dealer.sent[0][1].decodeToString().contains("\"type\":\"hello\""))
        assertEquals(true, dealer.sent[1][1].decodeToString().contains("\"type\":\"capability.snapshot\""))
        assertEquals(true, dealer.sent[2][1].decodeToString().contains("\"type\":\"turn.submit\""))
        client.close()
        assertEquals(true, dealer.closed)
    }

    @Test fun `stale session event fails closed instead of rendering assistant text`() {
        val dealer = ScriptedTextDealer(
            responses = listOf(
                server("{\"body\":{\"max_payload_bytes\":4194304,\"max_payload_frame_bytes\":1048576,\"max_payload_frames\":16,\"version\":1},\"id\":\"hello-ok\",\"payload_count\":0,\"reply_to\":\"req-1\",\"session_id\":\"session-1\",\"timestamp_ns\":2,\"type\":\"hello.ok\"}"),
                capabilityAck("caps-ok", "req-caps", "session-1", 3),
                server("{\"conversation_id\":\"conversation-1\",\"id\":\"accepted-1\",\"payload_count\":0,\"reply_to\":\"req-2\",\"session_id\":\"session-1\",\"timestamp_ns\":4,\"turn_id\":\"turn-1\",\"type\":\"turn.accepted\"}"),
                server("{\"body\":{\"success\":true,\"text\":\"stale\"},\"conversation_id\":\"conversation-1\",\"id\":\"done-1\",\"payload_count\":0,\"seq\":1,\"session_id\":\"old-session\",\"timestamp_ns\":5,\"turn_id\":\"turn-1\",\"type\":\"assistant.completed\"}"),
            )
        )
        val client = ZaraTextClientActor(
            dealerFactory = TextDealerFactory { dealer },
            requestIds = sequenceOf("req-1", "req-caps", "req-2").iterator(),
            timestamps = sequenceOf(1L, 2L, 9L).iterator(),
        )
        client.connect(ServerProfile.create("tcp://zara.example:7731"), 1).get()

        val error = assertThrows(Exception::class.java) {
            client.submitText(1, "session-1", "conversation-1", "hello").get()
        }
        assertEquals(true, rootCause(error) is ZaraWireException)
        client.close()
    }

    @Test fun `reconnect replaces dealer and rejects old generation calls`() {
        val first = ScriptedTextDealer(
            listOf(
                server("{\"body\":{\"max_payload_bytes\":4194304,\"max_payload_frame_bytes\":1048576,\"max_payload_frames\":16,\"version\":1},\"id\":\"hello-ok-1\",\"payload_count\":0,\"reply_to\":\"req-1\",\"session_id\":\"session-1\",\"timestamp_ns\":2,\"type\":\"hello.ok\"}"),
                capabilityAck("caps-ok-1", "req-caps-1", "session-1", 3),
            )
        )
        val second = ScriptedTextDealer(
            listOf(
                server("{\"body\":{\"max_payload_bytes\":4194304,\"max_payload_frame_bytes\":1048576,\"max_payload_frames\":16,\"version\":1},\"id\":\"hello-ok-2\",\"payload_count\":0,\"reply_to\":\"req-2\",\"session_id\":\"session-2\",\"timestamp_ns\":4,\"type\":\"hello.ok\"}"),
                capabilityAck("caps-ok-2", "req-caps-2", "session-2", 5),
            )
        )
        val dealers = ArrayDeque(listOf(first, second))
        val client = ZaraTextClientActor(
            dealerFactory = TextDealerFactory { dealers.removeFirst() },
            requestIds = sequenceOf("req-1", "req-caps-1", "req-2", "req-caps-2").iterator(),
            timestamps = sequenceOf(1L, 2L, 3L, 4L).iterator(),
        )
        client.connect(ServerProfile.create("tcp://zara.example:7731"), 1).get()
        client.connect(ServerProfile.create("tcp://zara.example:7731"), 2).get()
        assertEquals(true, first.closed)

        val error = assertThrows(Exception::class.java) {
            client.submitText(1, "session-1", "c", "old").get()
        }
        assertEquals(true, rootCause(error) is StaleTextSessionException)
        client.close()
    }

    private fun capabilityAck(id: String, replyTo: String, sessionId: String, timestamp: Long): List<ByteArray> =
        server(
            "{\"body\":{\"capabilities\":[]},\"id\":\"$id\",\"payload_count\":0," +
                "\"reply_to\":\"$replyTo\",\"session_id\":\"$sessionId\",\"timestamp_ns\":$timestamp," +
                "\"type\":\"capability.snapshot.ok\"}"
        )

    private fun server(json: String): List<ByteArray> =
        listOf("ZARA/1".encodeToByteArray(), json.encodeToByteArray())

    private fun rootCause(error: Throwable): Throwable {
        var current = error
        while (current.cause != null) current = current.cause!!
        return current
    }
}

private class ScriptedTextDealer(responses: List<List<ByteArray>>) : TextDealer {
    private val responses = ArrayDeque(responses)
    val sent = mutableListOf<List<ByteArray>>()
    var closed = false

    override fun send(frames: List<ByteArray>) {
        check(!closed)
        sent += frames.map(ByteArray::copyOf)
    }

    override fun receive(timeoutMillis: Int): List<ByteArray>? {
        check(timeoutMillis > 0)
        return if (responses.isEmpty()) null else responses.removeFirst()
    }

    override fun close() {
        closed = true
    }
}