package ai.zara.app.voice

import ai.zara.app.runtime.ZaraWireException
import org.junit.Assert.assertFalse
import org.junit.Assert.assertThrows
import org.junit.Assert.assertTrue
import org.junit.Test

class ZaraVoiceAckCodecDepthTest {
    @Test fun `voice acknowledgement JSON nesting is bounded at sixty four containers`() {
        val boundary = assertThrows(ZaraWireException::class.java) {
            ZaraVoiceAckCodec.decode(server(nestedEnvelope(64)))
        }
        assertFalse(boundary.message.orEmpty().contains("nesting depth"))

        val tooDeep = assertThrows(ZaraWireException::class.java) {
            ZaraVoiceAckCodec.decode(server(nestedEnvelope(65)))
        }
        assertTrue(tooDeep.message.orEmpty().contains("nesting depth"))
    }

    private fun nestedEnvelope(containers: Int): String {
        require(containers >= 1)
        val arrays = containers - 1
        return buildString {
            append("{\"extra\":")
            repeat(arrays) { append('[') }
            append("null")
            repeat(arrays) { append(']') }
            append('}')
        }
    }

    private fun server(json: String): List<ByteArray> =
        listOf("ZARA/1".encodeToByteArray(), json.encodeToByteArray())
}
