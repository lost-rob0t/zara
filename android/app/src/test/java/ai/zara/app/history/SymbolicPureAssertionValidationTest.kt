package ai.zara.app.history

import org.junit.Assert.assertTrue
import org.junit.Assert.fail
import org.junit.Test

class SymbolicPureAssertionValidationTest {
    private fun projection(
        outcome: String = "pending",
        dialogueAct: String = "clarify",
        maxModelCalls: Long = 0,
        modelCalls: Long = 0,
    ) = SymbolicConversationProjection(
        conversationId = "conv-pure-validation",
        projectionGeneration = 1,
        runtimeGeneration = 1,
        turnId = "turn-1",
        outcome = outcome,
        dialogueAct = dialogueAct,
        rendererProvenance = "symbolic-dcg/v1",
        providersEnabled = false,
        maxModelCalls = maxModelCalls,
        providerCalls = 0,
        modelCalls = modelCalls,
    )

    @Test
    fun `pure symbolic assertion validates full projection contract`() {
        assertFailsWithMessage("unsupported symbolic outcome") {
            projection(outcome = "provider_fallback").assertPureSymbolic()
        }
        assertFailsWithMessage("dialogueAct") {
            projection(dialogueAct = "Clarify Slot").assertPureSymbolic()
        }
    }

    @Test
    fun `payload rejects model usage above declared budget`() {
        assertFailsWithMessage("modelCalls must not exceed maxModelCalls") {
            SymbolicProjectionContract.validatePayload(
                projection(maxModelCalls = 1, modelCalls = 2)
            )
        }
    }

    private fun assertFailsWithMessage(fragment: String, block: () -> Unit) {
        try {
            block()
            fail("expected failure containing: $fragment")
        } catch (error: RuntimeException) {
            assertTrue(error.message.orEmpty().contains(fragment))
        }
    }
}
