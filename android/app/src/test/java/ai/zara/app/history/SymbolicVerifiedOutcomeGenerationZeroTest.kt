package ai.zara.app.history

import org.junit.Assert.assertThrows
import org.junit.Test

class SymbolicVerifiedOutcomeGenerationZeroTest {
    @Test
    fun `initial runtime generation zero accepts generation bound verified outcome`() {
        val projection = projection(
            runtimeGeneration = 0,
            verifiedOutcomeRef = "zara.verified-outcome/v2:0:outcome:startup-ok",
        )

        SymbolicProjectionContract.validateWrite(
            current = null,
            proposed = projection,
            expectedGeneration = 0,
        )
        projection.assertPureSymbolic()
    }

    @Test
    fun `generation zero syntax stays canonical and rejects leading zero aliases`() {
        val projection = projection(
            runtimeGeneration = 0,
            verifiedOutcomeRef = "zara.verified-outcome/v2:00:outcome:startup-ok",
        )

        assertThrows(IllegalArgumentException::class.java) {
            SymbolicProjectionContract.validateWrite(
                current = null,
                proposed = projection,
                expectedGeneration = 0,
            )
        }
    }

    private fun projection(
        runtimeGeneration: Long,
        verifiedOutcomeRef: String,
    ) = SymbolicConversationProjection(
        conversationId = "conv-generation-zero",
        projectionGeneration = 1,
        runtimeGeneration = runtimeGeneration,
        turnId = "turn-generation-zero",
        outcome = "success",
        projectGeneration = 0,
        dialogueAct = "verified",
        dialogueStateJson = "{}",
        discourseEntitiesJson = "[]",
        unresolvedQuestionsJson = "[]",
        expertEvidenceJson = "[]",
        verifiedFactsJson = "[]",
        verifiedOutcomeRefs = listOf(verifiedOutcomeRef),
        rendererProvenance = "symbolic-dcg/v1",
        providersEnabled = false,
        maxModelCalls = 0,
        providerCalls = 0,
        modelCalls = 0,
    )
}
