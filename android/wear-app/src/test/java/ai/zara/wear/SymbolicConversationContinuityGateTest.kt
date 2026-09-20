package ai.zara.wear

import ai.zara.ui.continuity.SymbolicConversationEdgeCodec
import ai.zara.ui.continuity.SymbolicConversationEdgeSnapshot
import org.junit.Assert.assertEquals
import org.junit.Assert.assertFalse
import org.junit.Assert.assertNull
import org.junit.Assert.assertTrue
import org.junit.Test

class SymbolicConversationContinuityGateTest {
    @Test
    fun decodesSharedWireAndAcceptsFreshScopedTruth() {
        val current = fixture(projectionGeneration = 4, runtimeGeneration = 7)
        val incoming = fixture(projectionGeneration = 5, runtimeGeneration = 8)
        val encoded = SymbolicConversationEdgeCodec.encode(incoming)

        assertEquals(incoming, SymbolicConversationContinuityGate.decodeAccepted(current, encoded))
        assertNull(
            SymbolicConversationContinuityGate.decodeAccepted(
                current,
                encoded + byteArrayOf(0x01),
            ),
        )
    }

    @Test
    fun acceptsFreshPureSymbolicProjectionForSameScope() {
        val current = fixture(projectionGeneration = 4, runtimeGeneration = 7)
        val incoming = fixture(projectionGeneration = 5, runtimeGeneration = 8)

        assertTrue(SymbolicConversationContinuityGate.accepts(current, incoming))
    }

    @Test
    fun rejectsWrongConversationAndStaleGenerations() {
        val current = fixture(projectionGeneration = 4, runtimeGeneration = 7)

        assertFalse(
            SymbolicConversationContinuityGate.accepts(
                current,
                fixture(conversationId = "chat-other", projectionGeneration = 5, runtimeGeneration = 8),
            ),
        )
        assertFalse(
            SymbolicConversationContinuityGate.accepts(
                current,
                fixture(projectionGeneration = 4, runtimeGeneration = 8),
            ),
        )
        assertFalse(
            SymbolicConversationContinuityGate.accepts(
                current,
                fixture(projectionGeneration = 5, runtimeGeneration = 6),
            ),
        )
    }

    @Test
    fun projectSwitchRequiresAdvancedProjectGeneration() {
        val current = fixture(projectId = "one", projectGeneration = 3, projectionGeneration = 4)

        assertFalse(
            SymbolicConversationContinuityGate.accepts(
                current,
                fixture(projectId = "two", projectGeneration = 3, projectionGeneration = 5),
            ),
        )
        assertTrue(
            SymbolicConversationContinuityGate.accepts(
                current,
                fixture(projectId = "two", projectGeneration = 4, projectionGeneration = 5),
            ),
        )
    }

    @Test
    fun refusesAnyModelOrProviderUsageInPureSymbolicEdgePath() {
        assertFalse(
            SymbolicConversationContinuityGate.accepts(
                null,
                fixture(modelCalls = 1),
            ),
        )
        assertFalse(
            SymbolicConversationContinuityGate.accepts(
                null,
                fixture(providerCalls = 1),
            ),
        )
    }

    private fun fixture(
        conversationId: String = "chat-1",
        projectionGeneration: Long = 1,
        runtimeGeneration: Long = 1,
        projectId: String? = "dotfiles",
        projectGeneration: Long = 1,
        modelCalls: Long = 0,
        providerCalls: Long = 0,
    ) = SymbolicConversationEdgeSnapshot(
        conversationId = conversationId,
        projectionGeneration = projectionGeneration,
        runtimeGeneration = runtimeGeneration,
        projectId = projectId,
        projectGeneration = projectGeneration,
        dialogueAct = "answer",
        discourseEntityRefs = listOf("entity:dotfiles"),
        unresolvedQuestionRefs = emptyList(),
        expertEvidenceRefs = listOf("expert:dotfiles:1"),
        verifiedOutcomeRefs = listOf("outcome:verified:1"),
        rendererProvenance = "symbolic-nlg/v1",
        modelCalls = modelCalls,
        providerCalls = providerCalls,
    )
}
