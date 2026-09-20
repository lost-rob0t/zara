package ai.zara.app.history

import org.junit.Assert.assertTrue
import org.junit.Assert.fail
import org.junit.Test

class SymbolicTerminalRendererProvenanceTest {
    @Test
    fun `successful pure symbolic projection requires canonical renderer evidence`() {
        val missing = projection(outcome = "success", rendererProvenance = "")

        assertFailsWithMessage("successful projection requires canonical symbolic renderer") {
            SymbolicProjectionContract.validatePayload(missing)
        }
        assertFailsWithMessage("successful projection requires canonical symbolic renderer") {
            missing.assertPureSymbolic()
        }
    }

    @Test
    fun `verified pure symbolic projection requires postcondition evidence`() {
        val missing = projection(
            outcome = "success",
            rendererProvenance = "symbolic-dcg/v1",
        ).copy(
            dialogueAct = "verified",
            verifiedOutcomeRefs = emptyList(),
        )

        assertFailsWithMessage("verified projection requires verified outcome evidence") {
            SymbolicProjectionContract.validatePayload(missing)
        }
        assertFailsWithMessage("verified projection requires verified outcome evidence") {
            missing.assertPureSymbolic()
        }
    }

    @Test
    fun `prerender pending projection may leave renderer empty`() {
        val pending = projection(outcome = "pending", rendererProvenance = "")

        SymbolicProjectionContract.validatePayload(pending)
        pending.assertPureSymbolic()

        val rendered = pending.copy(rendererProvenance = "symbolic-dcg/v1")
        SymbolicProjectionContract.validatePayload(rendered)
        rendered.assertPureSymbolic()
    }

    private fun projection(
        outcome: String,
        rendererProvenance: String,
    ) = SymbolicConversationProjection(
        conversationId = "conv-renderer-fence",
        projectionGeneration = 1,
        runtimeGeneration = 7,
        turnId = "turn-7",
        outcome = outcome,
        dialogueAct = "acknowledgement",
        rendererProvenance = rendererProvenance,
        providersEnabled = false,
        maxModelCalls = 0,
        providerCalls = 0,
        modelCalls = 0,
    )

    private fun assertFailsWithMessage(fragment: String, block: () -> Unit) {
        try {
            block()
            fail("expected failure containing: $fragment")
        } catch (error: RuntimeException) {
            assertTrue(error.message.orEmpty().contains(fragment))
        }
    }
}
