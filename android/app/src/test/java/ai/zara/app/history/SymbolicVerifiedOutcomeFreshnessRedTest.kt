package ai.zara.app.history

import org.junit.Assert.assertTrue
import org.junit.Assert.fail
import org.junit.Test

class SymbolicVerifiedOutcomeFreshnessRedTest {
    @Test
    fun `fresh verified write cannot mint success from legacy v1 only`() {
        val proposed = projection(
            generation = 1,
            runtimeGeneration = 1,
            turnId = "turn-1",
            outcome = "success",
            dialogueAct = "verified",
            receipts = listOf(v1Receipt(1)),
        )

        assertRejected("verified projection requires fresh outcome evidence") {
            SymbolicProjectionContract.validateWrite(null, proposed, expectedGeneration = 0)
        }
    }

    @Test
    fun `same turn verified promotion requires current generation v2 evidence`() {
        val pending = projection(
            generation = 1,
            runtimeGeneration = 8,
            turnId = "turn-8",
            outcome = "pending",
            dialogueAct = "pending",
            receipts = listOf(v2Receipt(7, 7)),
        )
        SymbolicProjectionContract.validateWrite(null, pending, expectedGeneration = 0)

        val verified = projection(
            generation = 2,
            runtimeGeneration = 8,
            turnId = "turn-8",
            outcome = "success",
            dialogueAct = "verified",
            receipts = pending.verifiedOutcomeRefs,
        )

        assertRejected("verified projection requires fresh outcome evidence") {
            SymbolicProjectionContract.validateWrite(pending, verified, expectedGeneration = 1)
        }
    }

    private fun projection(
        generation: Long,
        runtimeGeneration: Long,
        turnId: String,
        outcome: String,
        dialogueAct: String,
        receipts: List<String>,
    ) = SymbolicConversationProjection(
        conversationId = "conv-verified-freshness-red",
        projectionGeneration = generation,
        runtimeGeneration = runtimeGeneration,
        turnId = turnId,
        outcome = outcome,
        dialogueAct = dialogueAct,
        dialogueStateJson = "{\"act\":\"$dialogueAct\"}",
        verifiedOutcomeRefs = receipts,
        rendererProvenance = "symbolic-dcg/v1",
        providersEnabled = false,
        maxModelCalls = 0,
        providerCalls = 0,
        modelCalls = 0,
    )

    private fun assertRejected(message: String, action: () -> Unit) {
        try {
            action()
            fail("expected symbolic projection contract rejection")
        } catch (error: RuntimeException) {
            assertTrue(error.message.orEmpty().contains(message))
        }
    }

    private fun v1Receipt(index: Int): String =
        "zara.verified-outcome/v1:outcome:postcondition/tool-run-$index"

    private fun v2Receipt(runtimeGeneration: Int, index: Int): String =
        "zara.verified-outcome/v2:$runtimeGeneration:outcome:postcondition/tool-run-$index"
}
