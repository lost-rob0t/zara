package ai.zara.app.prolog

import org.junit.Assert.assertEquals
import org.junit.Assert.assertThrows
import org.junit.Test

class SymbolicSocialResponseActTest {
    @Test
    fun `help stays pure symbolic and payload-free`() {
        val act = SymbolicResponseAct.fromEnvelope(envelope("help", emptyMap()))

        assertEquals(SymbolicResponseActKind.HELP, act.kind)
        assertEquals(SymbolicResponsePayload.Empty, act.payload)
        assertEquals(0, act.usage.providerCalls)
        assertEquals(0, act.usage.modelCalls)
    }

    @Test
    fun `acknowledgement kind is closed and typed`() {
        val thanks = SymbolicResponseAct.fromEnvelope(
            envelope("acknowledgement", mapOf("kind" to "thanks")),
        )
        val acknowledged = SymbolicResponseAct.fromEnvelope(
            envelope("acknowledgement", mapOf("kind" to "acknowledged")),
        )

        assertEquals(
            SymbolicResponsePayload.Acknowledgement(SymbolicAcknowledgementKind.THANKS),
            thanks.payload,
        )
        assertEquals(
            SymbolicResponsePayload.Acknowledgement(SymbolicAcknowledgementKind.ACKNOWLEDGED),
            acknowledged.payload,
        )

        assertThrows(IllegalArgumentException::class.java) {
            SymbolicResponseAct.fromEnvelope(
                envelope("acknowledgement", mapOf("kind" to "maybe")),
            )
        }
        assertThrows(IllegalArgumentException::class.java) {
            SymbolicResponseAct.fromEnvelope(envelope("acknowledgement", emptyMap()))
        }
    }

    private fun envelope(
        act: String,
        payload: Map<String, Any?>,
    ): Map<String, Any?> = mapOf(
        "protocol" to "ZARA-SYMBOLIC-DIALOGUE/1",
        "act" to act,
        "payload" to payload,
        "renderer" to "symbolic-dcg/v1",
        "providers_enabled" to false,
        "max_model_calls" to 0,
        "usage" to mapOf("provider_calls" to 0, "model_calls" to 0),
    )
}
