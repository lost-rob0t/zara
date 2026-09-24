package ai.zara.app.prolog

import org.junit.Assert.assertEquals
import org.junit.Assert.assertThrows
import org.junit.Test

class SymbolicResponseActTest {
    @Test
    fun `accepts canonical zero-model greeting envelope`() {
        val act = SymbolicResponseAct.fromEnvelope(envelope("greeting", emptyMap()))

        assertEquals(SymbolicResponseActKind.GREETING, act.kind)
        assertEquals(SymbolicResponsePayload.Empty, act.payload)
        assertEquals(0, act.usage.providerCalls)
        assertEquals(0, act.usage.modelCalls)
    }

    @Test
    fun `preserves expert answer text and evidence exactly`() {
        val summary = "Path /Home/N545PY/.zara keeps Case + punctuation: Ω."
        val evidence = "expert:dotfiles/7#Result-A"

        val act = SymbolicResponseAct.fromEnvelope(
            envelope(
                "expert_answer",
                mapOf("summary" to summary, "evidence_ref" to evidence),
            ),
        )

        assertEquals(SymbolicResponseActKind.EXPERT_ANSWER, act.kind)
        assertEquals(
            SymbolicResponsePayload.ExpertAnswer(summary = summary, evidenceRef = evidence),
            act.payload,
        )
    }

    @Test
    fun `clarification accepts exactly one bounded discriminator`() {
        assertEquals(
            SymbolicResponsePayload.Clarification(slot = "duration"),
            SymbolicResponseAct.fromEnvelope(
                envelope("clarify", mapOf("slot" to "duration")),
            ).payload,
        )
        assertEquals(
            SymbolicResponsePayload.Clarification(reason = "ambiguous reference"),
            SymbolicResponseAct.fromEnvelope(
                envelope("clarify", mapOf("reason" to "ambiguous reference")),
            ).payload,
        )

        assertThrows(IllegalArgumentException::class.java) {
            SymbolicResponseAct.fromEnvelope(
                envelope("clarify", mapOf("slot" to "duration", "reason" to "missing")),
            )
        }
    }

    @Test
    fun `rejects every nonzero or enabled provider-model budget field`() {
        val providerEnabled = envelope("unsupported", emptyMap()).toMutableMap().apply {
            this["providers_enabled"] = true
        }
        val modelBudget = envelope("unsupported", emptyMap()).toMutableMap().apply {
            this["max_model_calls"] = 1
        }
        val providerUsage = envelope("unsupported", emptyMap()).toMutableMap().apply {
            this["usage"] = mapOf("provider_calls" to 1, "model_calls" to 0)
        }
        val modelUsage = envelope("unsupported", emptyMap()).toMutableMap().apply {
            this["usage"] = mapOf("provider_calls" to 0, "model_calls" to 1)
        }

        listOf(providerEnabled, modelBudget, providerUsage, modelUsage).forEach { invalid ->
            assertThrows(IllegalArgumentException::class.java) {
                SymbolicResponseAct.fromEnvelope(invalid)
            }
        }
    }

    @Test
    fun `rejects unknown fields and payload shape drift`() {
        val extraTopLevel = envelope("greeting", emptyMap()).toMutableMap().apply {
            this["fallback_provider"] = "cloud"
        }
        val extraPayload = envelope(
            "denied",
            mapOf("reason" to "not allowed", "provider" to "hidden"),
        )

        assertThrows(IllegalArgumentException::class.java) {
            SymbolicResponseAct.fromEnvelope(extraTopLevel)
        }
        assertThrows(IllegalArgumentException::class.java) {
            SymbolicResponseAct.fromEnvelope(extraPayload)
        }
    }

    @Test
    fun `choice payload is bounded and preserves ordering`() {
        val choices = listOf("first", "Second", "third")
        val payload = SymbolicResponseAct.fromEnvelope(
            envelope("choose", mapOf("choices" to choices)),
        ).payload

        assertEquals(SymbolicResponsePayload.Choices(choices), payload)

        assertThrows(IllegalArgumentException::class.java) {
            SymbolicResponseAct.fromEnvelope(
                envelope("choose", mapOf("choices" to emptyList<String>())),
            )
        }
        assertThrows(IllegalArgumentException::class.java) {
            SymbolicResponseAct.fromEnvelope(
                envelope("choose", mapOf("choices" to List(9) { "choice-$it" })),
            )
        }
    }

    @Test
    fun `verified and dispatch payloads are typed without execution`() {
        val verified = SymbolicResponseAct.fromEnvelope(
            envelope(
                "verified",
                mapOf("outcome" to "timer exists", "evidence_ref" to "receipt:timer/42"),
            ),
        )
        val dispatch = SymbolicResponseAct.fromEnvelope(
            envelope("dispatch_required", mapOf("frame_ref" to "frame:timer/42")),
        )

        assertEquals(
            SymbolicResponsePayload.Verified("timer exists", "receipt:timer/42"),
            verified.payload,
        )
        assertEquals(
            SymbolicResponsePayload.DispatchRequired("frame:timer/42"),
            dispatch.payload,
        )
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
