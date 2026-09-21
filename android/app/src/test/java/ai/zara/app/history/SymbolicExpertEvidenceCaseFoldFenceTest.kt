package ai.zara.app.history

import org.junit.Assert.assertTrue
import org.junit.Assert.fail
import org.junit.Test

class SymbolicExpertEvidenceCaseFoldFenceTest {
    private fun projection(expertEvidenceJson: String) = SymbolicConversationProjection(
        conversationId = "conversation:expert-casefold-fence",
        projectionGeneration = 1,
        runtimeGeneration = 1,
        turnId = "turn:expert-casefold:1",
        outcome = "pending",
        projectId = "project:zara",
        projectGeneration = 1,
        dialogueAct = "expert.answer",
        expertEvidenceJson = expertEvidenceJson,
        rendererProvenance = "symbolic-dcg/v1",
        providersEnabled = false,
        maxModelCalls = 0,
        providerCalls = 0,
        modelCalls = 0,
    )

    @Test
    fun `normalized provider and model metadata aliases fail closed`() {
        val poisoned = listOf(
            """[{"expert_id":"zara:expert/python","model_calls":0,"MODEL_CALLS":7}]""",
            """[{"expert_id":"zara:expert/python","model_calls":0,"explanation":{"MoDeL_CaLlS":7}}]""",
            """[{"expert_id":"zara:expert/python","model_calls":0,"providerCalls":1}]""",
            """[{"expert_id":"zara:expert/python","model_calls":0,"explanation":{"max-model-calls":1}}]""",
            """[{"expert_id":"zara:expert/python","model_calls":0,"explanation":{"TokenUsage":{"inputTokens":12}}}]""",
        )

        poisoned.forEach { encoded ->
            assertFailsWithMessage("expertEvidenceJson") {
                SymbolicProjectionContract.validatePayload(projection(encoded))
            }
        }
    }

    private fun assertFailsWithMessage(fragment: String, block: () -> Unit) {
        try {
            block()
            fail("expected failure containing: $fragment")
        } catch (error: RuntimeException) {
            assertTrue(error.message.orEmpty().contains(fragment))
        }
    }
}
