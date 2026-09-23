package ai.zara.app.history

import org.junit.Assert.assertEquals
import org.junit.Assert.assertTrue
import org.junit.Assert.fail
import org.junit.Test

class SymbolicVerifiedOutcomeBoundedWindowTest {
    @Test
    fun `verified effect conversation advances past bounded receipt window`() {
        val initialReceipts = (1..WINDOW).map(::receipt)
        val current = projection(
            generation = 1,
            runtimeGeneration = 1,
            turnId = "turn-64",
            receipts = initialReceipts,
        )
        val nextReceipts = initialReceipts.drop(1) + v2Receipt(2, WINDOW + 1)
        val proposed = projection(
            generation = 2,
            runtimeGeneration = 2,
            turnId = "turn-65",
            receipts = nextReceipts,
        )

        SymbolicProjectionContract.validateWrite(current, proposed, expectedGeneration = 1)
        proposed.assertPureSymbolic()

        assertEquals(WINDOW, proposed.verifiedOutcomeRefs.size)
        assertEquals(nextReceipts, proposed.verifiedOutcomeRefs)
        assertEquals(0L, proposed.maxModelCalls)
        assertEquals(0L, proposed.providerCalls)
        assertEquals(0L, proposed.modelCalls)
    }

    @Test
    fun `retired receipt cannot reenter as fresh after window compaction`() {
        val initialReceipts = (1..WINDOW).map(::receipt)
        val current = projection(
            generation = 1,
            runtimeGeneration = 1,
            turnId = "turn-64",
            receipts = initialReceipts,
        )
        val compactedReceipts = initialReceipts.drop(1) + v2Receipt(2, WINDOW + 1)
        val compacted = projection(
            generation = 2,
            runtimeGeneration = 2,
            turnId = "turn-65",
            receipts = compactedReceipts,
        )

        SymbolicProjectionContract.validateWrite(current, compacted, expectedGeneration = 1)

        val replayedReceipts = compactedReceipts.drop(1) + initialReceipts.first()
        val replayed = projection(
            generation = 3,
            runtimeGeneration = 3,
            turnId = "turn-66",
            receipts = replayedReceipts,
        )
        try {
            SymbolicProjectionContract.validateWrite(compacted, replayed, expectedGeneration = 2)
            fail("retired verified outcome receipt reentered the bounded window as fresh evidence")
        } catch (error: RuntimeException) {
            assertTrue(
                error.message.orEmpty().contains("retired verified outcome replay rejected")
            )
        }
    }

    private fun projection(
        generation: Long,
        runtimeGeneration: Long,
        turnId: String,
        receipts: List<String>,
    ) = SymbolicConversationProjection(
        conversationId = "conv-verified-window",
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

    private fun receipt(index: Int): String =
        "zara.verified-outcome/v1:outcome:postcondition/tool-run-$index"

    private fun v2Receipt(runtimeGeneration: Int, index: Int): String =
        "zara.verified-outcome/v2:$runtimeGeneration:outcome:postcondition/tool-run-$index"

    private companion object {
        const val WINDOW = 64
    }
}
