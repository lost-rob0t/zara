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

        assertEquals(incoming, decodeAccepted(current, encoded))
        assertNull(
            decodeAccepted(
                current,
                encoded + byteArrayOf(0x01),
            ),
        )
    }

    @Test
    fun initialProjectionRequiresExplicitSelectedScope() {
        val incoming = fixture()
        assertTrue(accepts(null, incoming))

        val wrongPrincipal = fixture(principalId = "principal:bob")
        assertFalse(
            SymbolicConversationContinuityGate.accepts(
                EXPECTED_PRINCIPAL_ID,
                EXPECTED_CONVERSATION_ID,
                null,
                wrongPrincipal,
            ),
        )
        assertNull(
            SymbolicConversationContinuityGate.decodeAccepted(
                EXPECTED_PRINCIPAL_ID,
                EXPECTED_CONVERSATION_ID,
                null,
                SymbolicConversationEdgeCodec.encode(wrongPrincipal),
            ),
        )

        val wrongConversation = fixture(conversationId = "chat-other")
        assertFalse(
            SymbolicConversationContinuityGate.accepts(
                EXPECTED_PRINCIPAL_ID,
                EXPECTED_CONVERSATION_ID,
                null,
                wrongConversation,
            ),
        )
        assertNull(
            SymbolicConversationContinuityGate.decodeAccepted(
                EXPECTED_PRINCIPAL_ID,
                EXPECTED_CONVERSATION_ID,
                null,
                SymbolicConversationEdgeCodec.encode(wrongConversation),
            ),
        )
    }

    @Test
    fun acceptsFreshPureSymbolicProjectionForSameScope() {
        val current = fixture(projectionGeneration = 4, runtimeGeneration = 7)
        val incoming = fixture(projectionGeneration = 5, runtimeGeneration = 8)
        assertTrue(accepts(current, incoming))
    }

    @Test
    fun rejectsWrongPrincipalConversationAndStaleGenerations() {
        val current = fixture(projectionGeneration = 4, runtimeGeneration = 7)

        assertFalse(
            accepts(
                current,
                fixture(principalId = "principal:bob", projectionGeneration = 5, runtimeGeneration = 8),
            ),
        )
        assertFalse(
            accepts(
                current,
                fixture(conversationId = "chat-other", projectionGeneration = 5, runtimeGeneration = 8),
            ),
        )
        assertFalse(
            accepts(
                current,
                fixture(projectionGeneration = 4, runtimeGeneration = 8),
            ),
        )
        assertFalse(
            accepts(
                current,
                fixture(projectionGeneration = 5, runtimeGeneration = 6),
            ),
        )
    }

    @Test
    fun rejectsCurrentProjectionOutsideSelectedScope() {
        val incoming = fixture(projectionGeneration = 5, runtimeGeneration = 8)
        assertFalse(
            SymbolicConversationContinuityGate.accepts(
                EXPECTED_PRINCIPAL_ID,
                EXPECTED_CONVERSATION_ID,
                fixture(principalId = "principal:bob", projectionGeneration = 4, runtimeGeneration = 7),
                incoming,
            ),
        )
        assertFalse(
            SymbolicConversationContinuityGate.accepts(
                EXPECTED_PRINCIPAL_ID,
                EXPECTED_CONVERSATION_ID,
                fixture(conversationId = "chat-other", projectionGeneration = 4, runtimeGeneration = 7),
                incoming,
            ),
        )
    }

    @Test
    fun projectSwitchRequiresAdvancedProjectGeneration() {
        val current = fixture(projectId = "one", projectGeneration = 3, projectionGeneration = 4)

        assertFalse(
            accepts(
                current,
                fixture(projectId = "two", projectGeneration = 3, projectionGeneration = 5),
            ),
        )
        assertTrue(
            accepts(
                current,
                fixture(projectId = "two", projectGeneration = 4, projectionGeneration = 5),
            ),
        )
    }

    @Test
    fun refusesAnyProviderOrModelAuthorityInPureSymbolicEdgePath() {
        assertFalse(accepts(null, fixture(providersEnabled = true)))
        assertFalse(accepts(null, fixture(maxModelCalls = 1)))
        assertFalse(accepts(null, fixture(maxModelCalls = 1, modelCalls = 1)))
        assertFalse(accepts(null, fixture(providerCalls = 1)))
    }

    @Test
    fun refusesNonCanonicalDialogueOrRendererProvenance() {
        assertFalse(accepts(null, fixture(dialogueAct = "answer")))
        assertFalse(
            accepts(
                null,
                fixture(rendererProvenance = "model-fallback/v1"),
            ),
        )
    }

    private fun accepts(
        current: SymbolicConversationEdgeSnapshot?,
        incoming: SymbolicConversationEdgeSnapshot,
    ): Boolean = SymbolicConversationContinuityGate.accepts(
        EXPECTED_PRINCIPAL_ID,
        EXPECTED_CONVERSATION_ID,
        current,
        incoming,
    )

    private fun decodeAccepted(
        current: SymbolicConversationEdgeSnapshot?,
        encoded: ByteArray,
    ): SymbolicConversationEdgeSnapshot? = SymbolicConversationContinuityGate.decodeAccepted(
        EXPECTED_PRINCIPAL_ID,
        EXPECTED_CONVERSATION_ID,
        current,
        encoded,
    )

    private fun fixture(
        principalId: String = EXPECTED_PRINCIPAL_ID,
        conversationId: String = EXPECTED_CONVERSATION_ID,
        projectionGeneration: Long = 1,
        runtimeGeneration: Long = 1,
        projectId: String? = "dotfiles",
        projectGeneration: Long = 1,
        dialogueAct: String = "expert_answer",
        rendererProvenance: String = "symbolic-dcg/v1",
        providersEnabled: Boolean = false,
        maxModelCalls: Long = 0,
        modelCalls: Long = 0,
        providerCalls: Long = 0,
    ) = SymbolicConversationEdgeSnapshot(
        principalId = principalId,
        conversationId = conversationId,
        projectionGeneration = projectionGeneration,
        runtimeGeneration = runtimeGeneration,
        projectId = projectId,
        projectGeneration = projectGeneration,
        dialogueAct = dialogueAct,
        discourseEntityRefs = listOf("entity:dotfiles"),
        unresolvedQuestionRefs = emptyList(),
        expertEvidenceRefs = listOf("expert:dotfiles:1"),
        verifiedOutcomeRefs = listOf("outcome:verified:1"),
        rendererProvenance = rendererProvenance,
        providersEnabled = providersEnabled,
        maxModelCalls = maxModelCalls,
        modelCalls = modelCalls,
        providerCalls = providerCalls,
    )

    private companion object {
        const val EXPECTED_PRINCIPAL_ID = "principal:alice"
        const val EXPECTED_CONVERSATION_ID = "chat-1"
    }
}
