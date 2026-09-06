package ai.zara.app.runtime

import org.junit.Assert.assertEquals
import org.junit.Assert.assertThrows
import org.junit.Test

class ZaraCapabilityCodecDepthTest {

    @Test
    fun `capability decoder permits 64 containers before schema rejection`() {
        val error = assertThrows(ZaraWireException::class.java) {
            ZaraCapabilityCodec.decodeSnapshotOk(frames(envelopeWithNestedBody(63)))
        }

        assertEquals("capability snapshot body has invalid fields", error.message)
    }

    @Test
    fun `capability decoder rejects the 65th container before descending`() {
        val error = assertThrows(ZaraWireException::class.java) {
            ZaraCapabilityCodec.decodeSnapshotOk(frames(envelopeWithNestedBody(64)))
        }

        assertEquals("JSON nesting exceeds depth limit", error.message)
    }

    private fun envelopeWithNestedBody(nestedContainers: Int): String =
        "{\"body\":${nestedObject(nestedContainers)}," +
            "\"id\":\"caps-ok\",\"payload_count\":0,\"reply_to\":\"caps-1\"," +
            "\"session_id\":\"session-1\",\"timestamp_ns\":3,\"type\":\"capability.snapshot.ok\"}"

    private fun nestedObject(depth: Int): String {
        require(depth >= 1)
        return buildString {
            repeat(depth) { append("{\"x\":") }
            append("null")
            repeat(depth) { append('}') }
        }
    }

    private fun frames(json: String): List<ByteArray> =
        listOf("ZARA/1".encodeToByteArray(), json.encodeToByteArray())
}
