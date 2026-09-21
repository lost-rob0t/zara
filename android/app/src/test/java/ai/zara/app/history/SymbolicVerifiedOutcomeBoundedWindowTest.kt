package ai.zara.app.history

import org.junit.Assert.assertEquals
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
        val nextReceipts = initialReceipts.drop(1) + receipt(WINDOW + 1)
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

    private companion object {
        const val WINDOW = 64
    }
}
