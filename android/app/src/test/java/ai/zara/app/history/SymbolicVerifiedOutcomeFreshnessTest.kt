package ai.zara.app.history

import org.junit.Assert.assertTrue
import org.junit.Assert.fail
import org.junit.Test

class SymbolicVerifiedOutcomeFreshnessTest {
    @Test
    fun `fresh verified write rejects generation unbound v1 evidence`() {
        val proposed = projection(
            generation = 1,
            runtimeGeneration = 1,
            turnId = "turn-1",
            receipts = listOf(STALE_RECEIPT),
        )

        assertRejected("verified projection requires fresh outcome evidence") {
            SymbolicProjectionContract.validateWrite(null, proposed, expectedGeneration = 0)
        }
    }

    @Test
    fun `migrated legacy v1 projection remains continuable`() {
        val current = projection(
            generation = 1,
            runtimeGeneration = 7,
            turnId = "turn-7",
            receipts = listOf(STALE_RECEIPT),
        )
        val freshV2 = v2Receipt(8, 8)
        val proposed = projection(
            generation = 2,
            runtimeGeneration = 8,
            turnId = "turn-8",
            receipts = listOf(STALE_RECEIPT, freshV2),
        )

        SymbolicProjectionContract.validateWrite(current, proposed, expectedGeneration = 1)
        proposed.assertPureSymbolic()
        assertTrue(proposed.verifiedOutcomeRefs == listOf(STALE_RECEIPT, freshV2))
    }

    @Test
    fun `migrated legacy v1 projection rejects new generation unbound v1 evidence`() {
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

        assertRejected("verified projection requires fresh outcome evidence") {
            SymbolicProjectionContract.validateWrite(current, proposed, expectedGeneration = 1)
        }
    }

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

        assertRejected("verified projection requires fresh outcome evidence") {
            SymbolicProjectionContract.validateWrite(current, proposed, expectedGeneration = 1)
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
        val freshV2 = v2Receipt(8, 8)
        val proposed = projection(
            generation = 2,
            runtimeGeneration = 8,
            turnId = "turn-8",
            receipts = listOf(STALE_RECEIPT, freshV2),
        )

        SymbolicProjectionContract.validateWrite(current, proposed, expectedGeneration = 1)
        proposed.assertPureSymbolic()
        assertTrue(proposed.verifiedOutcomeRefs == listOf(STALE_RECEIPT, freshV2))
    }

    @Test
    fun `same turn verified promotion requires current generation v2 evidence`() {
        val pending = projection(
            generation = 1,
            runtimeGeneration = 8,
            turnId = "turn-8",
            receipts = listOf(v2Receipt(7, 7)),
            outcome = "pending",
            dialogueAct = "pending",
        )
        SymbolicProjectionContract.validateWrite(null, pending, expectedGeneration = 0)
        val verified = projection(
            generation = 2,
            runtimeGeneration = 8,
            turnId = "turn-8",
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
        receipts: List<String>,
        outcome: String = "success",
        dialogueAct: String = "verified",
    ) = SymbolicConversationProjection(
        conversationId = "conv-verified-freshness",
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

    private fun v2Receipt(runtimeGeneration: Int, index: Int): String =
        "zara.verified-outcome/v2:$runtimeGeneration:outcome:postcondition/tool-run-$index"

    private companion object {
        const val STALE_RECEIPT = "zara.verified-outcome/v1:effect:tool-run-7"
        const val FRESH_RECEIPT =
            "zara.verified-outcome/v1:outcome:postcondition/tool-run-8"
    }
}
