package ai.zara.app.runtime

import org.junit.Assert.assertTrue
import org.junit.Test

class ZaraTextCodecDepthTest {
    @Test
    fun `wire decoder rejects json nesting beyond bounded depth`() {
        val nested = "[".repeat(65) + "0" + "]".repeat(65)
        val envelope = "{\"body\":$nested,\"id\":\"depth-1\",\"payload_count\":0,\"timestamp_ns\":1,\"type\":\"assistant.response\"}"

        val error = try {
            ZaraTextCodec.decode(frames(envelope))
            throw AssertionError("expected excessive JSON nesting to fail closed")
        } catch (error: ZaraWireException) {
            error
        }

        assertTrue(
            "depth rejection must happen at the parser boundary, not later schema validation: ${error.message}",
            error.message?.contains("nesting") == true,
        )
    }

    private fun frames(json: String): List<ByteArray> =
        listOf("ZARA/1".encodeToByteArray(), json.encodeToByteArray())
}
