package ai.zara.app.history

import org.junit.Assert.assertTrue
import org.junit.Assert.fail
import org.junit.Test

class SymbolicVerifiedOutcomeFreshnessTest {
    @Test
    fun `new verified turn rejects reused postcondition receipt`() {
        val current = projection(
            generation = 1,
            runtimeGeneration = 7,
            turnId = "turn-7",
            receipts = listOf(STALE_RECEIPT),
        )
        val proposed = projection(
            generation = 2,
            runtimeGeneration = 8,
            turnId = "turn-8",
            receipts = listOf(STALE_RECEIPT),
        )

        try {
            SymbolicProjectionContract.validateWrite(current, proposed, expectedGeneration = 1)
            fail("new verified turn reused a stale postcondition receipt")
        } catch (error: RuntimeException) {
            assertTrue(
                error.message.orEmpty().contains(
                    "verified projection requires fresh outcome evidence"
                )
            )
        }
    }

    @Test
    fun `new verified turn accepts fresh receipt without dropping history`() {
        val current = projection(
            generation = 1,
            runtimeGeneration = 7,
            turnId = "turn-7",
            receipts = listOf(STALE_RECEIPT),
        )
        val proposed = projection(
            generation = 2,
            runtimeGeneration = 8,
            turnId = "turn-8",
            receipts = listOf(STALE_RECEIPT, FRESH_RECEIPT),
        )

        SymbolicProjectionContract.validateWrite(current, proposed, expectedGeneration = 1)
        proposed.assertPureSymbolic()
        assertTrue(proposed.verifiedOutcomeRefs == listOf(STALE_RECEIPT, FRESH_RECEIPT))
    }

    private fun projection(
        generation: Long,
        runtimeGeneration: Long,
        turnId: String,
        receipts: List<String>,
    ) = SymbolicConversationProjection(
        conversationId = "conv-verified-freshness",
        projectionGeneration = generation,
        runtimeGeneration = runtimeGeneration,
        turnId = turnId,
        outcome = "success",
        dialogueAct = "verified",
        dialogueStateJson = "{\"act\":\"verified\"}",
        verifiedOutcomeRefs = receipts,
        rendererProvenance = "symbolic-dcg/v1",
        providersEnabled = false,
        maxModelCalls = 0,
        providerCalls = 0,
        modelCalls = 0,
    )

    private companion object {
        const val STALE_RECEIPT = "zara.verified-outcome/v1:effect:tool-run-7"
        const val FRESH_RECEIPT =
            "zara.verified-outcome/v1:outcome:postcondition/tool-run-8"
    }
}
