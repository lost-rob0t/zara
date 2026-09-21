package ai.zara.app.history

import android.content.Context
import androidx.test.platform.app.InstrumentationRegistry
import org.junit.After
import org.junit.Assert.assertEquals
import org.junit.Before
import org.junit.Test

class SymbolicConversationEdgeDuplicateReferenceInstrumentedTest {
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
    fun repeatedCanonicalRefsRemainReadableByEdgeAfterProcessRecreation() {
        val first = PortableConversationStore(context)
        first.createConversation("Repeated refs", conversationId = CONVERSATION_ID)
        first.saveSymbolicProjection(
            SymbolicConversationProjection(
                conversationId = CONVERSATION_ID,
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
                rendererProvenance = "symbolic-dcg/v1",
                providersEnabled = false,
                maxModelCalls = 0,
                providerCalls = 0,
                modelCalls = 0,
            ),
            expectedGeneration = 0,
        )
        first.close()

        val reopened = PortableConversationStore(context)
        try {
            val edge = checkNotNull(reopened.loadSymbolicEdgeSnapshot(CONVERSATION_ID))
            edge.assertPureSymbolic()

            assertEquals(listOf("file:flake.nix"), edge.discourseEntityRefs)
            assertEquals(listOf("question:target"), edge.unresolvedQuestionRefs)
            assertEquals(listOf("evidence:resolver:7"), edge.expertEvidenceRefs)
            assertEquals(false, edge.providersEnabled)
            assertEquals(0L, edge.maxModelCalls)
            assertEquals(0L, edge.providerCalls)
            assertEquals(0L, edge.modelCalls)
        } finally {
            reopened.close()
        }
    }

    private companion object {
        const val CONVERSATION_ID = "edge-duplicate-reference-continuity"
    }
}
