package ai.zara.app.history

import org.junit.Assert.assertEquals
import org.junit.Assert.assertTrue
import org.junit.Assert.fail
import org.junit.Test

class SymbolicVerifiedOutcomeLegacyCutoverTest {
    @Test
    fun `migrated legacy projection cannot mint new v1 and cuts over to current v2`() {
        val legacyA = legacyReceipt("legacy-a")
        val legacyB = legacyReceipt("legacy-b")
        val current = projection(
            generation = 1,
            runtimeGeneration = 1,
            turnId = "turn-legacy",
            receipts = listOf(legacyA),
        )

        val illegalV1Advance = projection(
            generation = 2,
            runtimeGeneration = 2,
            turnId = "turn-v1-rejected",
            receipts = listOf(legacyA, legacyB),
        )
        assertRejected("retired verified outcome replay rejected") {
            SymbolicProjectionContract.validateWrite(
                current,
                illegalV1Advance,
                expectedGeneration = 1,
            )
        }

        val cutover = projection(
            generation = 2,
            runtimeGeneration = 2,
            turnId = "turn-v2-cutover",
            receipts = listOf(legacyA, v2Receipt(2, "fresh-b")),
        )
        SymbolicProjectionContract.validateWrite(current, cutover, expectedGeneration = 1)
        cutover.assertPureSymbolic()
        assertEquals(false, cutover.providersEnabled)
        assertEquals(0L, cutover.maxModelCalls)
        assertEquals(0L, cutover.providerCalls)
        assertEquals(0L, cutover.modelCalls)
    }

    private fun projection(
        generation: Long,
        runtimeGeneration: Long,
        turnId: String,
        receipts: List<String>,
    ) = SymbolicConversationProjection(
        conversationId = "conv-verified-legacy-cutover",
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

    private fun legacyReceipt(name: String): String =
        "zara.verified-outcome/v1:outcome:postcondition/$name"

    private fun v2Receipt(runtimeGeneration: Int, name: String): String =
        "zara.verified-outcome/v2:$runtimeGeneration:outcome:postcondition/$name"
}
