package ai.zara.wear

import ai.zara.ui.continuity.SymbolicConversationEdgeCodec
import ai.zara.ui.continuity.SymbolicConversationEdgeSnapshot
import org.junit.Assert.assertEquals
import org.junit.Assert.assertNotNull
import org.junit.Test

class SymbolicConversationEdgeWindowTest {
    @Test
    fun acceptsLatestBoundedCanonicalKnowledgeWindowWithoutProviderAuthority() {
        val discourseRefs = (3..18).map { "entity:$it" }
        val questionRefs = (3..18).map { "question:$it" }
        val evidenceRefs = (3..18).map { "expert:evidence:$it" }
        val incoming = SymbolicConversationEdgeSnapshot(
            principalId = EXPECTED_PRINCIPAL_ID,
            conversationId = EXPECTED_CONVERSATION_ID,
            projectionGeneration = 5,
            runtimeGeneration = 8,
            projectId = "dotfiles",
            projectGeneration = 2,
            dialogueAct = "expert_answer",
            discourseEntityRefs = discourseRefs,
            unresolvedQuestionRefs = questionRefs,
            expertEvidenceRefs = evidenceRefs,
            verifiedOutcomeRefs = emptyList(),
            rendererProvenance = "symbolic-dcg/v1",
            providersEnabled = false,
            maxModelCalls = 0,
            modelCalls = 0,
            providerCalls = 0,
        )

        incoming.assertPureSymbolic()
        val accepted = SymbolicConversationContinuityGate.decodeAccepted(
            EXPECTED_PRINCIPAL_ID,
            EXPECTED_CONVERSATION_ID,
            null,
            SymbolicConversationEdgeCodec.encode(incoming),
        )

        assertNotNull(accepted)
        val snapshot = requireNotNull(accepted)
        snapshot.assertPureSymbolic()
        assertEquals(16, snapshot.discourseEntityRefs.size)
        assertEquals(16, snapshot.unresolvedQuestionRefs.size)
        assertEquals(16, snapshot.expertEvidenceRefs.size)
        assertEquals("entity:3", snapshot.discourseEntityRefs.first())
        assertEquals("entity:18", snapshot.discourseEntityRefs.last())
        assertEquals("question:3", snapshot.unresolvedQuestionRefs.first())
        assertEquals("question:18", snapshot.unresolvedQuestionRefs.last())
        assertEquals("expert:evidence:3", snapshot.expertEvidenceRefs.first())
        assertEquals("expert:evidence:18", snapshot.expertEvidenceRefs.last())
        assertEquals(0, snapshot.maxModelCalls)
        assertEquals(0, snapshot.modelCalls)
        assertEquals(0, snapshot.providerCalls)
    }

    private companion object {
        const val EXPECTED_PRINCIPAL_ID = "principal:alice"
        const val EXPECTED_CONVERSATION_ID = "chat-1"
    }
}
