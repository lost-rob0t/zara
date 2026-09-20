package ai.zara.app.history

import android.content.Context
import androidx.test.platform.app.InstrumentationRegistry
import org.junit.After
import org.junit.Assert.assertEquals
import org.junit.Assert.assertFalse
import org.junit.Assert.assertTrue
import org.junit.Before
import org.junit.Test

class SymbolicConversationEdgeProjectionInstrumentedTest {
    private lateinit var context: Context

    @Before
    fun setUp() {
        context = InstrumentationRegistry.getInstrumentation().targetContext
        context.deleteDatabase(ConversationHistoryContract.databaseName)
    }

    @After
    fun tearDown() {
        context.deleteDatabase(ConversationHistoryContract.databaseName)
    }

    @Test
    fun canonicalStoreProjectsSameScopedPureSymbolicTruthForEdgeConsumers() {
        val store = PortableConversationStore(context)
        try {
            store.createConversation("Edge continuity", conversationId = CONVERSATION_ID)
            store.saveSymbolicProjection(
                projection(),
                expectedGeneration = 0,
            )

            val edge = checkNotNull(store.loadSymbolicEdgeSnapshot(CONVERSATION_ID))
            edge.assertPureSymbolic()

            assertEquals(ConversationHistoryContract.localPrincipalId, edge.principalId)
            assertEquals(CONVERSATION_ID, edge.conversationId)
            assertEquals(1L, edge.projectionGeneration)
            assertEquals(9L, edge.runtimeGeneration)
            assertEquals("project:dotfiles", edge.projectId)
            assertEquals(3L, edge.projectGeneration)
            assertEquals("expert_answer", edge.dialogueAct)
            assertEquals(listOf("file:flake.nix"), edge.discourseEntityRefs)
            assertEquals(listOf("target"), edge.unresolvedQuestionRefs)
            assertEquals(listOf("ev-1"), edge.expertEvidenceRefs)
            assertEquals(listOf(VERIFIED_OUTCOME_REF), edge.verifiedOutcomeRefs)
            assertFalse(edge.providersEnabled)
            assertEquals(0L, edge.maxModelCalls)
            assertEquals(0L, edge.providerCalls)
            assertEquals(0L, edge.modelCalls)
        } finally {
            store.close()
        }
    }

    @Test
    fun projectionFailsClosedWhenExpertAnswerHasNoCanonicalEvidenceRef() {
        val projection = projection(expertEvidenceJson = "[]")

        val failure = runCatching {
            projection.toSymbolicEdgeSnapshot(ConversationHistoryContract.localPrincipalId)
        }.exceptionOrNull()

        requireNotNull(failure)
        assertTrue(failure.message.orEmpty().contains("expert evidence"))
    }

    @Test
    fun projectionFailsClosedWhenPersistedObjectCannotYieldStableReference() {
        val projection = projection(
            discourseEntitiesJson = "[{\"label\":\"flake.nix\"}]",
        )

        val failure = runCatching {
            projection.toSymbolicEdgeSnapshot(ConversationHistoryContract.localPrincipalId)
        }.exceptionOrNull()

        requireNotNull(failure)
        assertTrue(failure.message.orEmpty().contains("discourse entity reference"))
    }

    private fun projection(
        discourseEntitiesJson: String = "[{\"entity_id\":\"file:flake.nix\"}]",
        expertEvidenceJson: String = "[{\"evidence_id\":\"ev-1\"}]",
    ) = SymbolicConversationProjection(
        conversationId = CONVERSATION_ID,
        projectionGeneration = 1,
        runtimeGeneration = 9,
        turnId = "turn-9",
        outcome = "pending",
        projectId = "project:dotfiles",
        projectGeneration = 3,
        dialogueAct = "expert_answer",
        dialogueStateJson = "{\"act\":\"expert_answer\"}",
        discourseEntitiesJson = discourseEntitiesJson,
        unresolvedQuestionsJson = "[{\"slot\":\"target\"}]",
        expertEvidenceJson = expertEvidenceJson,
        verifiedFactsJson = "[{\"fact_id\":\"fact-1\"}]",
        verifiedOutcomeRefs = listOf(VERIFIED_OUTCOME_REF),
        rendererProvenance = "symbolic-dcg/v1",
        providersEnabled = false,
        maxModelCalls = 0,
        providerCalls = 0,
        modelCalls = 0,
    )

    private companion object {
        const val CONVERSATION_ID = "edge-continuity"
        const val VERIFIED_OUTCOME_REF = "zara.verified-outcome/v1:outcome:edge-1"
    }
}
