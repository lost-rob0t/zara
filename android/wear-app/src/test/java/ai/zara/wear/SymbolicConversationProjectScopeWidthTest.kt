package ai.zara.wear

import ai.zara.ui.continuity.SymbolicConversationEdgeCodec
import ai.zara.ui.continuity.SymbolicConversationEdgeSnapshot
import org.junit.Assert.assertEquals
import org.junit.Assert.assertNotNull
import org.junit.Test

class SymbolicConversationProjectScopeWidthTest {
    @Test
    fun wearAcceptsCanonicalProjectScopeAtPortableLimit() {
        val projectId = "project:" + "x".repeat(504)
        val incoming = SymbolicConversationEdgeSnapshot(
            principalId = PRINCIPAL_ID,
            conversationId = CONVERSATION_ID,
            projectionGeneration = 1,
            runtimeGeneration = 1,
            projectId = projectId,
            projectGeneration = 1,
            dialogueAct = "greeting",
            rendererProvenance = "symbolic-dcg/v1",
            providersEnabled = false,
            maxModelCalls = 0,
            modelCalls = 0,
            providerCalls = 0,
        )

        val accepted = SymbolicConversationContinuityGate.decodeAccepted(
            expectedPrincipalId = PRINCIPAL_ID,
            expectedConversationId = CONVERSATION_ID,
            current = null,
            encoded = SymbolicConversationEdgeCodec.encode(incoming),
        )

        assertNotNull(accepted)
        accepted!!.assertPureSymbolic()
        assertEquals(projectId, accepted.projectId)
        assertEquals(0L, accepted.maxModelCalls)
        assertEquals(0L, accepted.modelCalls)
        assertEquals(0L, accepted.providerCalls)
    }

    private companion object {
        const val PRINCIPAL_ID = "principal:local"
        const val CONVERSATION_ID = "conversation:project-scope-width"
    }
}
