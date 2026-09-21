package ai.zara.app.history

import org.junit.Assert.assertTrue
import org.junit.Assert.fail
import org.junit.Test

class SymbolicExpertEvidenceProjectionContractTest {
    @Test
    fun `expert answer cannot claim pure symbolic success without canonical expert evidence`() {
        assertFailsWithMessage("expert_answer projection requires expert evidence") {
            projection(expertEvidenceJson = "[]").assertPureSymbolic()
        }
    }

    @Test
    fun `expert answer with canonical evidence remains valid`() {
        projection(
            expertEvidenceJson = "[{\"ref\":\"expert:dotfiles:1\"}]",
        ).assertPureSymbolic()
    }

    private fun projection(expertEvidenceJson: String) = SymbolicConversationProjection(
        conversationId = "expert-evidence-contract",
        projectionGeneration = 1L,
        runtimeGeneration = 1L,
        turnId = "turn-expert-1",
        outcome = "success",
        projectId = "project-a",
        projectGeneration = 1L,
        dialogueAct = "expert_answer",
        expertEvidenceJson = expertEvidenceJson,
        rendererProvenance = "symbolic-dcg/v1",
        providersEnabled = false,
        maxModelCalls = 0L,
        providerCalls = 0L,
        modelCalls = 0L,
    )

    private fun assertFailsWithMessage(fragment: String, block: () -> Unit) {
        try {
            block()
            fail("expected failure containing: $fragment")
        } catch (error: RuntimeException) {
            assertTrue(error.message.orEmpty().contains(fragment))
        }
    }
}
