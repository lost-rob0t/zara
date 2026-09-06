package ai.zara.app.runtime

import org.junit.Assert.assertFalse
import org.junit.Assert.assertTrue
import org.junit.Test

class ZaraTextCodecDepthTest {
    @Test
    fun `wire decoder permits exactly sixty four json containers`() {
        val error = decodeFailure(envelopeWithNestedBody(63))

        assertFalse(
            "exact parser depth limit must remain available to canonical envelopes",
            error.message?.contains("nesting") == true,
        )
        assertTrue(error.message?.contains("body must be an object") == true)
    }

    @Test
    fun `wire decoder rejects sixty fifth json container`() {
        val error = decodeFailure(envelopeWithNestedBody(64))

        assertTrue(
            "depth rejection must happen at the parser boundary, not later schema validation: ${error.message}",
            error.message?.contains("nesting") == true,
        )
    }

    private fun envelopeWithNestedBody(arrayDepth: Int): String {
        val nested = "[".repeat(arrayDepth) + "0" + "]".repeat(arrayDepth)
        return "{\"body\":$nested,\"id\":\"depth-1\",\"payload_count\":0,\"timestamp_ns\":1,\"type\":\"assistant.response\"}"
    }

    private fun decodeFailure(envelope: String): ZaraWireException = try {
        ZaraTextCodec.decode(frames(envelope))
        throw AssertionError("expected invalid test envelope to fail closed")
    } catch (error: ZaraWireException) {
        error
    }

    private fun frames(json: String): List<ByteArray> =
        listOf("ZARA/1".encodeToByteArray(), json.encodeToByteArray())
}
