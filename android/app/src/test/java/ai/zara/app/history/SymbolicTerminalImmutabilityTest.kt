package ai.zara.app.history

import org.junit.Assert.assertEquals
import org.junit.Assert.assertTrue
import org.junit.Assert.fail
import org.junit.Test

class SymbolicTerminalImmutabilityTest {
    private fun projection(
        generation: Long,
        outcome: String,
    ) = SymbolicConversationProjection(
        conversationId = "conv-terminal-immutable",
        projectionGeneration = generation,
        runtimeGeneration = 41,
        turnId = "turn-41",
        outcome = outcome,
        projectId = "project-a",
        projectGeneration = 1,
        dialogueStateJson = "{\"act\":\"clarify\",\"slot\":\"target\"}",
        discourseEntitiesJson = "[{\"entity_id\":\"file:flake.nix\"}]",
        unresolvedQuestionsJson = "[{\"slot\":\"target\"}]",
        expertEvidenceJson = "[{\"evidence_id\":\"ev-before\"}]",
        verifiedFactsJson = "[{\"fact_id\":\"fact-before\"}]",
        rendererProvenance = "symbolic-dcg/v1",
        providerCalls = 0,
        modelCalls = 0,
    )

    @Test
    fun `terminal same turn projection rejects late semantic mutation`() {
        val pending = projection(generation = 1, outcome = "pending")
        val cancelled = projection(generation = 2, outcome = "cancelled")
        SymbolicProjectionContract.validateWrite(pending, cancelled, expectedGeneration = 1)
        cancelled.assertPureSymbolic()

        val lateCancelledCallback = cancelled.copy(
            projectionGeneration = 3,
            dialogueStateJson = "{\"act\":\"effect_completed\",\"late\":true}",
            expertEvidenceJson = "[{\"evidence_id\":\"ev-late\"}]",
            verifiedFactsJson = "[{\"fact_id\":\"fact-late\",\"value\":\"should-not-persist\"}]",
        )

        try {
            SymbolicProjectionContract.validateWrite(
                cancelled,
                lateCancelledCallback,
                expectedGeneration = 2,
            )
            fail("expected terminal turn projection immutability rejection")
        } catch (error: RuntimeException) {
            assertTrue(error.message.orEmpty().contains("terminal turn projection is immutable"))
        }

        assertEquals(0L, cancelled.providerCalls)
        assertEquals(0L, cancelled.modelCalls)
    }
}
