package ai.zara.app.history

import org.junit.Assert.assertEquals
import org.junit.Assert.assertTrue
import org.junit.Assert.fail
import org.junit.Test

class SymbolicVerifiedOutcomeV2CutoverTest {
    @Test
    fun `full legacy window cuts over to generation bound v2`() {
        val legacy = (1..WINDOW).map(::legacyReceipt)
        val current = projection(
            generation = 1,
            runtimeGeneration = 1,
            turnId = "turn-64",
            receipts = legacy,
        )
        val cutoverReceipt = v2Receipt(runtimeGeneration = 2, index = WINDOW + 1)
        val cutover = projection(
            generation = 2,
            runtimeGeneration = 2,
            turnId = "turn-65",
            receipts = legacy.drop(1) + cutoverReceipt,
        )

        SymbolicProjectionContract.validateWrite(current, cutover, expectedGeneration = 1)
        cutover.assertPureSymbolic()
        assertEquals(WINDOW, cutover.verifiedOutcomeRefs.size)
        assertEquals(0L, cutover.maxModelCalls)
        assertEquals(0L, cutover.providerCalls)
        assertEquals(0L, cutover.modelCalls)

        val nextReceipt = v2Receipt(runtimeGeneration = 3, index = WINDOW + 2)
        val advanced = projection(
            generation = 3,
            runtimeGeneration = 3,
            turnId = "turn-66",
            receipts = cutover.verifiedOutcomeRefs.drop(1) + nextReceipt,
        )
        SymbolicProjectionContract.validateWrite(cutover, advanced, expectedGeneration = 2)
        advanced.assertPureSymbolic()
        assertEquals(WINDOW, advanced.verifiedOutcomeRefs.size)
    }

    @Test
    fun `v2 cutover rejects retired legacy receipt reentry`() {
        val legacy = (1..WINDOW).map(::legacyReceipt)
        val current = projection(
            generation = 1,
            runtimeGeneration = 1,
            turnId = "turn-64",
            receipts = legacy,
        )
        val cutover = projection(
            generation = 2,
            runtimeGeneration = 2,
            turnId = "turn-65",
            receipts = legacy.drop(1) + v2Receipt(2, WINDOW + 1),
        )
        SymbolicProjectionContract.validateWrite(current, cutover, expectedGeneration = 1)

        val replay = projection(
            generation = 3,
            runtimeGeneration = 3,
            turnId = "turn-66-replay",
            receipts = cutover.verifiedOutcomeRefs.drop(1) + legacy.first(),
        )
        assertRejected("retired verified outcome replay rejected") {
            SymbolicProjectionContract.validateWrite(cutover, replay, expectedGeneration = 2)
        }
    }

    @Test
    fun `v2 cutover rejects retired v2 receipt replay`() {
        val legacy = (1..WINDOW).map(::legacyReceipt)
        val current = projection(
            generation = 1,
            runtimeGeneration = 1,
            turnId = "turn-64",
            receipts = legacy,
        )
        val firstV2 = v2Receipt(2, WINDOW + 1)
        val cutover = projection(
            generation = 2,
            runtimeGeneration = 2,
            turnId = "turn-65",
            receipts = legacy.drop(1) + firstV2,
        )
        SymbolicProjectionContract.validateWrite(current, cutover, expectedGeneration = 1)

        val secondV2 = v2Receipt(3, WINDOW + 2)
        val advanced = projection(
            generation = 3,
            runtimeGeneration = 3,
            turnId = "turn-66",
            receipts = cutover.verifiedOutcomeRefs.drop(1) + secondV2,
        )
        SymbolicProjectionContract.validateWrite(cutover, advanced, expectedGeneration = 2)

        val replay = projection(
            generation = 4,
            runtimeGeneration = 4,
            turnId = "turn-67-replay",
            receipts = advanced.verifiedOutcomeRefs.drop(1) + firstV2,
        )
        assertRejected("retired verified outcome replay rejected") {
            SymbolicProjectionContract.validateWrite(advanced, replay, expectedGeneration = 3)
        }
    }

    @Test
    fun `new v2 receipt must bind to new runtime generation`() {
        val current = projection(
            generation = 1,
            runtimeGeneration = 7,
            turnId = "turn-7",
            receipts = listOf(v2Receipt(7, 7)),
        )
        val mismatch = projection(
            generation = 2,
            runtimeGeneration = 8,
            turnId = "turn-8",
            receipts = current.verifiedOutcomeRefs + v2Receipt(7, 8),
        )

        assertRejected("verified outcome generation mismatch rejected") {
            SymbolicProjectionContract.validateWrite(current, mismatch, expectedGeneration = 1)
        }
    }

    private fun projection(
        generation: Long,
        runtimeGeneration: Long,
        turnId: String,
        receipts: List<String>,
    ) = SymbolicConversationProjection(
        conversationId = "conv-verified-v2-cutover",
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

    private fun assertRejected(message: String, action: () -> Unit) {
        try {
            action()
            fail("expected symbolic projection contract rejection")
        } catch (error: RuntimeException) {
            assertTrue(error.message.orEmpty().contains(message))
        }
    }

    private fun legacyReceipt(index: Int): String =
        "zara.verified-outcome/v1:outcome:postcondition/tool-run-$index"

    private fun v2Receipt(runtimeGeneration: Int, index: Int): String =
        "zara.verified-outcome/v2:$runtimeGeneration:outcome:postcondition/tool-run-$index"

    private companion object {
        const val WINDOW = 64
    }
}
