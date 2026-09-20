package ai.zara.app.history

import org.junit.Assert.assertTrue
import org.junit.Assert.fail
import org.junit.Test

class SymbolicPurePolicyTest {
    private fun projection(
        generation: Long = 1,
        runtimeGeneration: Long = 1,
        turnId: String = "turn-policy",
        providersEnabled: Boolean = false,
        maxModelCalls: Long = 0,
        providerCalls: Long = 0,
        modelCalls: Long = 0,
    ) = SymbolicConversationProjection(
        conversationId = "conv-policy",
        projectionGeneration = generation,
        runtimeGeneration = runtimeGeneration,
        turnId = turnId,
        outcome = "pending",
        dialogueAct = "clarify",
        rendererProvenance = "symbolic-dcg/v1",
        providersEnabled = providersEnabled,
        maxModelCalls = maxModelCalls,
        providerCalls = providerCalls,
        modelCalls = modelCalls,
    )

    @Test
    fun `pure symbolic assertion requires disabled providers zero budget and zero usage`() {
        projection().assertPureSymbolic()
        assertFails("providers enabled") {
            projection(providersEnabled = true).assertPureSymbolic()
        }
        assertFails("maxModelCalls=1") {
            projection(maxModelCalls = 1).assertPureSymbolic()
        }
        assertFails("providerCalls=1") {
            projection(providerCalls = 1).assertPureSymbolic()
        }
        assertFails("modelCalls=1") {
            projection(modelCalls = 1).assertPureSymbolic()
        }
    }

    @Test
    fun `omitted policy defaults fail closed`() {
        val omitted = SymbolicConversationProjection(
            conversationId = "conv-policy-omitted",
            projectionGeneration = 1,
            runtimeGeneration = 1,
        )
        assertFails("providers enabled") {
            omitted.assertPureSymbolic()
        }
    }

    @Test
    fun `policy cannot widen after a zero model symbolic projection`() {
        val current = projection()
        assertFails("provider policy widening") {
            SymbolicProjectionContract.validateWrite(
                current,
                projection(
                    generation = 2,
                    runtimeGeneration = 2,
                    turnId = "turn-next",
                    providersEnabled = true,
                ),
                expectedGeneration = 1,
            )
        }
        assertFails("model-call budget widening") {
            SymbolicProjectionContract.validateWrite(
                current,
                projection(
                    generation = 2,
                    runtimeGeneration = 2,
                    turnId = "turn-next",
                    maxModelCalls = 1,
                ),
                expectedGeneration = 1,
            )
        }
    }

    private fun assertFails(fragment: String, block: () -> Unit) {
        try {
            block()
            fail("expected failure containing: $fragment")
        } catch (error: RuntimeException) {
            assertTrue(error.message.orEmpty().contains(fragment))
        }
    }
}
