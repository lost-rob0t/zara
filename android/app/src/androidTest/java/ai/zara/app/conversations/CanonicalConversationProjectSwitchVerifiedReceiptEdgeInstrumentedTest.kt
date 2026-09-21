package ai.zara.app.conversations

import ai.zara.app.history.ConversationHistoryContract
import ai.zara.app.history.HistoryMessageRole
import ai.zara.app.history.PortableConversationStore
import ai.zara.app.history.SymbolicConversationProjection
import ai.zara.app.history.loadSymbolicEdgeSnapshot
import ai.zara.app.history.loadSymbolicProjection
import ai.zara.app.history.saveSymbolicProjection
import android.content.Context
import androidx.test.platform.app.InstrumentationRegistry
import java.io.File
import org.junit.After
import org.junit.Assert.assertEquals
import org.junit.Assert.assertFalse
import org.junit.Assert.assertTrue
import org.junit.Before
import org.junit.Test

/**
 * Project-scoped conversational knowledge must be cleared when a conversation moves projects, but
 * the canonical verified-outcome receipt window is an anti-replay ledger and must survive unchanged.
 * Phone and Wear/edge must observe that same persisted truth before and after process recreation.
 */
class CanonicalConversationProjectSwitchVerifiedReceiptEdgeInstrumentedTest {
    private lateinit var context: Context
    private lateinit var metadataFile: File

    @Before
    fun setUp() {
        context = InstrumentationRegistry.getInstrumentation().targetContext
        context.deleteDatabase(ConversationHistoryContract.databaseName)
        metadataFile = File(context.cacheDir, "project-switch-verified-edge-conversation-ui.bin")
        metadataFile.delete()
    }

    @After
    fun tearDown() {
        context.deleteDatabase(ConversationHistoryContract.databaseName)
        metadataFile.delete()
    }

    @Test
    fun projectSwitchClearsProjectKnowledgeButRetainsVerifiedReceiptForWearAcrossRecreation() {
        val firstStore = PortableConversationStore(context)
        val conversations = CanonicalConversationStore(
            history = firstStore,
            metadataFile = metadataFile,
            legacyFile = null,
            idFactory = { CONVERSATION_ID },
        )
        assertEquals(CONVERSATION_ID, conversations.create(PROJECT_A).id)
        conversations.beginTurn(CONVERSATION_ID, "continue")

        val turnId = checkNotNull(
            firstStore.loadMessages(CONVERSATION_ID)
                .single { it.role == HistoryMessageRole.Assistant }
                .turnId,
        )
        firstStore.saveSymbolicProjection(
            projection = SymbolicConversationProjection(
                conversationId = CONVERSATION_ID,
                projectionGeneration = 1L,
                runtimeGeneration = RUNTIME_GENERATION,
                turnId = turnId,
                outcome = "pending",
                projectId = PROJECT_A,
                projectGeneration = 1L,
                dialogueAct = "clarify",
                dialogueStateJson = "{\"context\":\"project-a\"}",
                discourseEntitiesJson = "[{\"id\":\"project-a-entity\"}]",
                unresolvedQuestionsJson = "[{\"slot\":\"duration\"}]",
                expertEvidenceJson = "[{\"ref\":\"project-a-expert\"}]",
                verifiedFactsJson = "[{\"fact\":\"project-a-fact\"}]",
                verifiedOutcomeRefs = listOf(RETAINED_RECEIPT),
                providersEnabled = false,
                maxModelCalls = 0L,
                providerCalls = 0L,
                modelCalls = 0L,
            ),
            expectedGeneration = 0L,
        ).assertPureSymbolic()

        conversations.moveToProject(CONVERSATION_ID, PROJECT_B)
        assertCanonicalAndEdgeTruth(firstStore)
        firstStore.close()

        val reopenedStore = PortableConversationStore(context)
        try {
            assertCanonicalAndEdgeTruth(reopenedStore)
        } finally {
            reopenedStore.close()
        }
    }

    private fun assertCanonicalAndEdgeTruth(store: PortableConversationStore) {
        val projection = checkNotNull(store.loadSymbolicProjection(CONVERSATION_ID))
        projection.assertPureSymbolic()
        assertEquals(PROJECT_B, projection.projectId)
        assertEquals(2L, projection.projectGeneration)
        assertEquals("cancelled", projection.outcome)
        assertEquals("cancelled", projection.dialogueAct)
        assertEquals("{}", projection.dialogueStateJson)
        assertEquals("[]", projection.discourseEntitiesJson)
        assertEquals("[]", projection.unresolvedQuestionsJson)
        assertEquals("[]", projection.expertEvidenceJson)
        assertEquals("[]", projection.verifiedFactsJson)
        assertEquals(listOf(RETAINED_RECEIPT), projection.verifiedOutcomeRefs)
        assertFalse(projection.providersEnabled)
        assertEquals(0L, projection.maxModelCalls)
        assertEquals(0L, projection.providerCalls)
        assertEquals(0L, projection.modelCalls)

        val edge = checkNotNull(store.loadSymbolicEdgeSnapshot(CONVERSATION_ID))
        edge.assertPureSymbolic()
        assertEquals(PROJECT_B, edge.projectId)
        assertEquals(2L, edge.projectGeneration)
        assertEquals("cancelled", edge.dialogueAct)
        assertTrue(edge.discourseEntityRefs.isEmpty())
        assertTrue(edge.unresolvedQuestionRefs.isEmpty())
        assertTrue(edge.expertEvidenceRefs.isEmpty())
        assertEquals(listOf(RETAINED_RECEIPT), edge.verifiedOutcomeRefs)
        assertFalse(edge.providersEnabled)
        assertEquals(0L, edge.maxModelCalls)
        assertEquals(0L, edge.providerCalls)
        assertEquals(0L, edge.modelCalls)
    }

    private companion object {
        const val CONVERSATION_ID = "android-project-switch-verified-edge"
        const val PROJECT_A = "project-a"
        const val PROJECT_B = "project-b"
        const val RUNTIME_GENERATION = 7L
        const val RETAINED_RECEIPT =
            "zara.verified-outcome/v2:7:outcome:postcondition/project-a-effect"
    }
}
