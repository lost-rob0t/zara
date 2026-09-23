package ai.zara.ui.continuity

import org.junit.Assert.assertEquals
import org.junit.Test

class SymbolicConversationEdgeProjectScopeWidthTest {
    @Test
    fun roundTripsProjectScopeAtCanonicalPortableLimit() {
        val projectId = "project:" + "x".repeat(504)
        val snapshot = SymbolicConversationEdgeSnapshot(
            principalId = "principal:alice",
            conversationId = "chat-project-scope-width",
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

        val decoded = SymbolicConversationEdgeCodec.decode(
            SymbolicConversationEdgeCodec.encode(snapshot),
        )

        decoded.assertPureSymbolic()
        assertEquals(512, projectId.length)
        assertEquals(projectId, decoded.projectId)
    }
}
