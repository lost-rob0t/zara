package ai.zara.app.history

import org.junit.Assert.assertTrue
import org.junit.Assert.fail
import org.junit.Test

class SymbolicVerifiedOutcomeAbaReplayTest {
    @Test
    fun `non adjacent verified receipt replay is rejected`() {
        val first = projection(
            generation = 1,
            runtimeGeneration = 1,
            turnId = "turn-1",
            receipts = listOf(R1),
        )
        val second = projection(
            generation = 2,
            runtimeGeneration = 2,
            turnId = "turn-2",
            receipts = listOf(R1, R2),
        )
        val third = projection(
            generation = 3,
            runtimeGeneration = 3,
            turnId = "turn-3",
            receipts = listOf(R1),
        )

        SymbolicProjectionContract.validateWrite(first, second, expectedGeneration = 1)
        try {
            SymbolicProjectionContract.validateWrite(second, third, expectedGeneration = 2)
            fail("later verified turn dropped durable receipts and replayed old R1 as fresh evidence")
        } catch (error: RuntimeException) {
            assertTrue(
                error.message.orEmpty().contains("verified outcome evidence rewind rejected")
            )
        }
    }

    @Test
    fun `verified evidence can advance monotonically with zero model accounting`() {
        val first = projection(
            generation = 1,
            runtimeGeneration = 1,
            turnId = "turn-1",
            receipts = listOf(R1),
        )
        val second = projection(
            generation = 2,
            runtimeGeneration = 2,
            turnId = "turn-2",
            receipts = listOf(R1, R2),
        )
        val third = projection(
            generation = 3,
            runtimeGeneration = 3,
            turnId = "turn-3",
            receipts = listOf(R1, R2, R3),
        )

        SymbolicProjectionContract.validateWrite(first, second, expectedGeneration = 1)
        SymbolicProjectionContract.validateWrite(second, third, expectedGeneration = 2)
        third.assertPureSymbolic()

        assertTrue(third.verifiedOutcomeRefs == listOf(R1, R2, R3))
        assertTrue(third.maxModelCalls == 0L)
        assertTrue(third.providerCalls == 0L)
        assertTrue(third.modelCalls == 0L)
    }

    private fun projection(
        generation: Long,
        runtimeGeneration: Long,
        turnId: String,
        receipts: List<String>,
    ) = SymbolicConversationProjection(
        conversationId = "conv-verified-aba",
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
        const val R1 = "zara.verified-outcome/v1:effect:tool-run-1"
        const val R2 = "zara.verified-outcome/v1:outcome:postcondition/tool-run-2"
        const val R3 = "zara.verified-outcome/v1:outcome:postcondition/tool-run-3"
    }
}
