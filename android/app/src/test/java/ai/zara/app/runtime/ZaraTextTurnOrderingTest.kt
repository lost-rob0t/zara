package ai.zara.app.runtime

import java.util.ArrayDeque
import org.junit.Assert.assertEquals
import org.junit.Test

class ZaraTextTurnOrderingTest {
    @Test fun `text actor drains turn progress through terminal completion`() {
        val dealer = OrderingDealer(
            listOf(
                wire("{\"body\":{\"max_payload_bytes\":4194304,\"max_payload_frame_bytes\":1048576,\"max_payload_frames\":16,\"version\":1},\"id\":\"h-ok\",\"payload_count\":0,\"reply_to\":\"r1\",\"session_id\":\"s1\",\"timestamp_ns\":2,\"type\":\"hello.ok\"}"),
                wire("{\"conversation_id\":\"c1\",\"id\":\"accepted\",\"payload_count\":0,\"reply_to\":\"r2\",\"session_id\":\"s1\",\"timestamp_ns\":3,\"turn_id\":\"t1\",\"type\":\"turn.accepted\"}"),
                wire("{\"body\":{},\"conversation_id\":\"c1\",\"id\":\"started\",\"payload_count\":0,\"seq\":1,\"session_id\":\"s1\",\"timestamp_ns\":4,\"turn_id\":\"t1\",\"type\":\"turn.started\"}"),
                wire("{\"body\":{},\"conversation_id\":\"c1\",\"id\":\"assistant-started\",\"payload_count\":0,\"seq\":2,\"session_id\":\"s1\",\"timestamp_ns\":5,\"turn_id\":\"t1\",\"type\":\"assistant.started\"}"),
                wire("{\"body\":{\"text\":\"hello\"},\"conversation_id\":\"c1\",\"id\":\"delta\",\"payload_count\":0,\"seq\":3,\"session_id\":\"s1\",\"timestamp_ns\":6,\"turn_id\":\"t1\",\"type\":\"assistant.delta\"}"),
                wire("{\"body\":{\"success\":true,\"text\":\"hello world\"},\"conversation_id\":\"c1\",\"id\":\"assistant-done\",\"payload_count\":0,\"seq\":4,\"session_id\":\"s1\",\"timestamp_ns\":7,\"turn_id\":\"t1\",\"type\":\"assistant.completed\"}"),
                wire("{\"body\":{\"success\":true},\"conversation_id\":\"c1\",\"id\":\"turn-done\",\"payload_count\":0,\"seq\":5,\"session_id\":\"s1\",\"timestamp_ns\":8,\"turn_id\":\"t1\",\"type\":\"turn.completed\"}"),
            )
        )
        val client = ZaraTextClientActor(
            dealerFactory = TextDealerFactory { dealer },
            requestIds = sequenceOf("r1", "r2").iterator(),
            timestamps = sequenceOf(1L, 9L).iterator(),
        )
        client.connect(ServerProfile.create("tcp://zara.example:7731"), 1).get()
        assertEquals(
            TextTurnResult("c1", "t1", "hello world", true),
            client.submitText(1, "s1", "c1", "hello").get(),
        )
        assertEquals(0, dealer.remaining)
        client.close()
    }

    private fun wire(json: String) = listOf("ZARA/1".encodeToByteArray(), json.encodeToByteArray())
}

private class OrderingDealer(responses: List<List<ByteArray>>) : TextDealer {
    private val responses = ArrayDeque(responses)
    val remaining: Int get() = responses.size
    override fun send(frames: List<ByteArray>) = Unit
    override fun receive(timeoutMillis: Int): List<ByteArray>? = if (responses.isEmpty()) null else responses.removeFirst()
    override fun close() = Unit
}
