package ai.zara.app.history

import android.content.Context
import androidx.test.platform.app.InstrumentationRegistry
import org.junit.After
import org.junit.Assert.assertEquals
import org.junit.Assert.assertTrue
import org.junit.Before
import org.junit.Test

class PortableConversationRestartFenceInstrumentedTest {
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
    fun processRecreationInterruptsPendingProjectionAndRejectsLateCompletion() {
        val first = PortableConversationStore(context)
        val stamp = PortableConversationStore.nowIso()
        first.createConversation("restart fence", conversationId = CONVERSATION_ID)
        first.saveMessage(
            HistoryMessage(
                id = "assistant-pending",
                conversationId = CONVERSATION_ID,
                sequence = 1,
                role = HistoryMessageRole.Assistant,
                content = "Working on it",
                status = HistoryMessageStatus.Streaming,
                createdAt = stamp,
                updatedAt = stamp,
                turnId = TURN_ID,
            )
        )
        first.saveSymbolicProjection(
            pendingProjection(CONVERSATION_ID, TURN_ID),
            expectedGeneration = 0,
        ).assertPureSymbolic()
        first.close()

        val reopened = PortableConversationStore(context)
        try {
            val state = reopened.loadState(CONVERSATION_ID)
            val message = state.messages.single()
            assertEquals(HistoryMessageStatus.Cancelled, message.status)
            assertEquals(ConversationHistoryContract.interruptedError, message.error)

            assertRestartFenced(reopened, CONVERSATION_ID, TURN_ID)
        } finally {
            reopened.close()
        }
    }

    @Test
    fun processRecreationInterruptsOrphanPendingProjectionWithoutMessage() {
        val first = PortableConversationStore(context)
        first.createConversation("restart orphan", conversationId = ORPHAN_CONVERSATION_ID)
        first.saveSymbolicProjection(
            pendingProjection(ORPHAN_CONVERSATION_ID, ORPHAN_TURN_ID),
            expectedGeneration = 0,
        ).assertPureSymbolic()
        assertTrue(first.loadMessages(ORPHAN_CONVERSATION_ID).isEmpty())
        first.close()

        val reopened = PortableConversationStore(context)
        try {
            val state = reopened.loadState(ORPHAN_CONVERSATION_ID)
            assertTrue(state.messages.isEmpty())

            assertRestartFenced(reopened, ORPHAN_CONVERSATION_ID, ORPHAN_TURN_ID)
        } finally {
            reopened.close()
        }
    }

    private fun pendingProjection(
        conversationId: String,
        turnId: String,
    ): SymbolicConversationProjection = SymbolicConversationProjection(
        conversationId = conversationId,
        projectionGeneration = 1,
        runtimeGeneration = RUNTIME_GENERATION,
        turnId = turnId,
        outcome = "pending",
        projectId = "project-restart",
        projectGeneration = 1,
        dialogueAct = "clarify",
        dialogueStateJson = "{\"slot\":\"target\"}",
        unresolvedQuestionsJson = "[{\"slot\":\"target\"}]",
        rendererProvenance = "symbolic-dcg/v1",
        providersEnabled = false,
        maxModelCalls = 0,
        providerCalls = 0,
        modelCalls = 0,
    )

    private fun assertRestartFenced(
        store: PortableConversationStore,
        conversationId: String,
        turnId: String,
    ) {
        val recovered = checkNotNull(store.loadSymbolicProjection(conversationId))
        recovered.assertPureSymbolic()
        assertEquals("interrupted", recovered.outcome)
        assertEquals(2L, recovered.projectionGeneration)
        assertEquals(RUNTIME_GENERATION, recovered.runtimeGeneration)
        assertEquals(turnId, recovered.turnId)
        assertEquals(0L, recovered.maxModelCalls)
        assertEquals(0L, recovered.providerCalls)
        assertEquals(0L, recovered.modelCalls)

        val lateCompletion = recovered.copy(
            projectionGeneration = recovered.projectionGeneration + 1,
            outcome = "success",
            verifiedFactsJson = "[{\"fact_id\":\"late\"}]",
        )
        val rejected = runCatching {
            store.saveSymbolicProjection(
                lateCompletion,
                expectedGeneration = recovered.projectionGeneration,
            )
        }
        assertTrue(
            "late same-turn completion must be rejected after restart interruption",
            rejected.isFailure,
        )
    }

    private companion object {
        const val CONVERSATION_ID = "restart-fence-conversation"
        const val TURN_ID = "turn-restart-fence"
        const val ORPHAN_CONVERSATION_ID = "restart-fence-orphan-conversation"
        const val ORPHAN_TURN_ID = "turn-restart-fence-orphan"
        const val RUNTIME_GENERATION = 23L
    }
}
