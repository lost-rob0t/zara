package ai.zara.app.history

import org.junit.Assert.assertTrue
import org.junit.Assert.fail
import org.junit.Test

class SymbolicExpertEvidenceTrustEnvelopeTest {
    private fun projection(expertEvidenceJson: String) = SymbolicConversationProjection(
        conversationId = "conversation:expert-trust",
        projectionGeneration = 1,
        runtimeGeneration = 1,
        turnId = "turn:expert:1",
        outcome = "pending",
        projectId = "project:zara",
        projectGeneration = 1,
        dialogueAct = "expert.answer",
        dialogueStateJson = "{\"active_project\":\"project:zara\"}",
        expertEvidenceJson = expertEvidenceJson,
        providersEnabled = false,
        maxModelCalls = 0,
        providerCalls = 0,
        modelCalls = 0,
    )

    @Test
    fun `canonical typed symbolic expert evidence is accepted`() {
        SymbolicProjectionContract.validatePayload(
            projection(
                """[{"expert_id":"zara:expert/python","invocation_id":"invocation:python:1","evidence_refs":["evidence:python:1"],"verdict":"succeeded","model_calls":0,"explanation":{"symbolic_terms":["python","inspect"],"trace":["expert.invoke","expert.complete"]}}]"""
            )
        )
    }

    @Test
    fun `provider shaped metadata inside expert evidence is rejected`() {
        assertFailsWithMessage("expertEvidenceJson") {
            SymbolicProjectionContract.validatePayload(
                projection(
                    """[{"expert_id":"zara:expert/python","evidence_refs":["evidence:python:1"],"model_calls":0,"usage":{"provider_calls":1}}]"""
                )
            )
        }
    }

    @Test
    fun `expert model calls must remain exact zero`() {
        assertFailsWithMessage("expertEvidenceJson") {
            SymbolicProjectionContract.validatePayload(
                projection(
                    """[{"expert_id":"zara:expert/python","evidence_refs":["evidence:python:1"],"model_calls":1}]"""
                )
            )
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
