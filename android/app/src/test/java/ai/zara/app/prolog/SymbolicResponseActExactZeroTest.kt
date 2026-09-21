package ai.zara.app.prolog

import org.junit.Assert.assertThrows
import org.junit.Test

class SymbolicResponseActExactZeroTest {
    @Test
    fun `model budget accepts only an integer zero`() {
        listOf<Any?>(false, 0.0, "0", null).forEach { disguisedZero ->
            val value = baseEnvelope().toMutableMap().apply {
                this["max_model_calls"] = disguisedZero
            }
            assertThrows(IllegalArgumentException::class.java) {
                SymbolicResponseAct.fromEnvelope(value)
            }
        }
    }

    @Test
    fun `usage counters accept only integer zero`() {
        listOf<Any?>(false, 0.0, "0", null).forEach { disguisedZero ->
            val provider = baseEnvelope().toMutableMap().apply {
                this["usage"] = mapOf(
                    "provider_calls" to disguisedZero,
                    "model_calls" to 0,
                )
            }
            val model = baseEnvelope().toMutableMap().apply {
                this["usage"] = mapOf(
                    "provider_calls" to 0,
                    "model_calls" to disguisedZero,
                )
            }
            assertThrows(IllegalArgumentException::class.java) {
                SymbolicResponseAct.fromEnvelope(provider)
            }
            assertThrows(IllegalArgumentException::class.java) {
                SymbolicResponseAct.fromEnvelope(model)
            }
        }
    }

    @Test
    fun `protocol and renderer mismatches cannot enter Android symbolic state`() {
        val wrongProtocol = baseEnvelope().toMutableMap().apply {
            this["protocol"] = "ZARA-SYMBOLIC-DIALOGUE/2"
        }
        val modelRenderer = baseEnvelope().toMutableMap().apply {
            this["renderer"] = "model-fallback/v1"
        }

        assertThrows(IllegalArgumentException::class.java) {
            SymbolicResponseAct.fromEnvelope(wrongProtocol)
        }
        assertThrows(IllegalArgumentException::class.java) {
            SymbolicResponseAct.fromEnvelope(modelRenderer)
        }
    }

    private fun baseEnvelope(): Map<String, Any?> = mapOf(
        "protocol" to "ZARA-SYMBOLIC-DIALOGUE/1",
        "act" to "unsupported",
        "payload" to emptyMap<String, Any?>(),
        "renderer" to "symbolic-dcg/v1",
        "providers_enabled" to false,
        "max_model_calls" to 0,
        "usage" to mapOf("provider_calls" to 0, "model_calls" to 0),
    )
}
