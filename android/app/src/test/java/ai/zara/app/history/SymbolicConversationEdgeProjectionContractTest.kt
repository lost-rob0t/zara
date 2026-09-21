package ai.zara.app.history

import org.junit.Assert.assertEquals
import org.junit.Test

class SymbolicConversationEdgeProjectionContractTest {
    @Test
    fun repeatedCanonicalReferencesNormalizeToOneStableEdgeReference() {
        val projection = SymbolicConversationProjection(
            conversationId = "edge-dedup",
            projectionGeneration = 1,
            runtimeGeneration = 7,
            turnId = "turn-7",
            outcome = "pending",
            projectId = "project:dotfiles",
            projectGeneration = 2,
            dialogueAct = "greeting",
            dialogueStateJson = "{\"act\":\"greeting\"}",
            discourseEntitiesJson = "[" +
                "{\"entity_id\":\"file:flake.nix\"}," +
                "{\"entity_id\":\"file:flake.nix\"}]",
            unresolvedQuestionsJson = "[" +
                "{\"question_id\":\"question:target\"}," +
                "{\"question_id\":\"question:target\"}]",
            expertEvidenceJson = "[" +
                "{\"evidence_id\":\"evidence:resolver:7\"}," +
                "{\"evidence_id\":\"evidence:resolver:7\"}]",
            verifiedFactsJson = "[]",
            verifiedOutcomeRefs = emptyList(),
            rendererProvenance = "symbolic-dcg/v1",
            providersEnabled = false,
            maxModelCalls = 0,
            providerCalls = 0,
            modelCalls = 0,
        )

        val edge = projection.toSymbolicEdgeSnapshot("principal:local")

        edge.assertPureSymbolic()
        assertEquals(listOf("file:flake.nix"), edge.discourseEntityRefs)
        assertEquals(listOf("question:target"), edge.unresolvedQuestionRefs)
        assertEquals(listOf("evidence:resolver:7"), edge.expertEvidenceRefs)
        assertEquals(0L, edge.maxModelCalls)
        assertEquals(0L, edge.providerCalls)
        assertEquals(0L, edge.modelCalls)
    }
}
