package ai.zara.app.runtime

import org.junit.Assert.assertEquals
import org.junit.Assert.assertThrows
import org.junit.Test

class ZaraDeviceActionCodecDepthTest {

    @Test
    fun `device action decoder permits 64 containers before schema rejection`() {
        val error = assertThrows(ZaraWireException::class.java) {
            ZaraDeviceActionCodec.decodeServerMessage(frames(envelopeWithNestedBody(63)))
        }

        assertEquals("device action cancel body has invalid fields", error.message)
    }

    @Test
    fun `device action decoder rejects the 65th container before descending`() {
        val error = assertThrows(ZaraWireException::class.java) {
            ZaraDeviceActionCodec.decodeServerMessage(frames(envelopeWithNestedBody(64)))
        }

        assertEquals("JSON nesting exceeds depth limit", error.message)
    }

    private fun envelopeWithNestedBody(nestedContainers: Int): String =
        "{\"body\":${nestedObject(nestedContainers)}," +
            "\"id\":\"cancel-1\",\"payload_count\":0,\"session_id\":\"session-1\"," +
            "\"timestamp_ns\":3,\"type\":\"device.action.cancel\"}"

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
