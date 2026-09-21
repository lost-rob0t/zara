package ai.zara.app.conversations

import ai.zara.app.history.ConversationHistoryContract
import ai.zara.app.history.HistoryMessageRole
import ai.zara.app.history.HistoryMessageStatus
import ai.zara.app.history.PortableConversationStore
import ai.zara.app.history.SymbolicConversationProjection
import ai.zara.app.history.completeSymbolicTurnAtomically
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
 * Regression for the second-turn pre-projection project-switch race.
 *
 * A conversation can already have a terminal symbolic projection from an earlier turn while the
 * next canonical assistant row is running and the next pending projection has not been installed
 * yet. A project switch in that window must fence the new turn and advance the existing projection
 * generation. Returning the prior terminal projection unchanged lets the losing project-A preflight
 * install a stale pending projection after project B is already visible.
 */
class CanonicalConversationPriorProjectionProjectSwitchFenceInstrumentedTest {
    private lateinit var context: Context
    private lateinit var metadataFile: File

    @Before
    fun setUp() {
        context = InstrumentationRegistry.getInstrumentation().targetContext
        context.deleteDatabase(ConversationHistoryContract.databaseName)
        metadataFile = File(context.cacheDir, "prior-projection-project-switch-fence.bin")
        metadataFile.delete()
    }

    @After
    fun tearDown() {
        context.deleteDatabase(ConversationHistoryContract.databaseName)
        metadataFile.delete()
    }

    @Test
    fun projectSwitchFencesNextRunningTurnWhenPriorTerminalProjectionAlreadyExists() {
        val firstStore = PortableConversationStore(context)
        val conversations = CanonicalConversationStore(
            history = firstStore,
            metadataFile = metadataFile,
            legacyFile = null,
            idFactory = { CONVERSATION_ID },
        )
        assertEquals(CONVERSATION_ID, conversations.create(PROJECT_A).id)

        conversations.beginTurn(CONVERSATION_ID, "first symbolic turn")
        val firstTurnId = checkNotNull(
            firstStore.loadMessages(CONVERSATION_ID)
                .last { it.role == HistoryMessageRole.Assistant }
                .turnId,
        )
        val firstPending = SymbolicConversationProjection(
            conversationId = CONVERSATION_ID,
            projectionGeneration = 1L,
            runtimeGeneration = 1L,
            turnId = firstTurnId,
            outcome = "pending",
            projectId = PROJECT_A,
            projectGeneration = 1L,
            dialogueAct = "clarify",
            dialogueStateJson = "{}",
            rendererProvenance = "",
            providersEnabled = false,
            maxModelCalls = 0L,
            providerCalls = 0L,
            modelCalls = 0L,
        )
        firstStore.saveSymbolicProjection(firstPending, expectedGeneration = 0L)
        val firstTerminal = firstPending.copy(
            projectionGeneration = 2L,
            outcome = "success",
            rendererProvenance = "symbolic-dcg/v1",
        )
        firstStore.completeSymbolicTurnAtomically(
            projection = firstTerminal,
            expectedGeneration = 1L,
            turnId = firstTurnId,
            assistantContent = "first turn complete",
            assistantStatus = HistoryMessageStatus.Complete,
        )

        conversations.beginTurn(CONVERSATION_ID, "second turn before pending projection")
        val secondAssistant = firstStore.loadMessages(CONVERSATION_ID)
            .last { it.role == HistoryMessageRole.Assistant }
        val secondTurnId = checkNotNull(secondAssistant.turnId)
        assertTrue(secondAssistant.status.isRunning())
        assertTrue(secondTurnId != firstTurnId)

        val priorTerminal = checkNotNull(firstStore.loadSymbolicProjection(CONVERSATION_ID))
        assertEquals("success", priorTerminal.outcome)
        assertEquals(firstTurnId, priorTerminal.turnId)
        assertEquals(2L, priorTerminal.projectionGeneration)

        val staleProjectAPending = priorTerminal.copy(
            projectionGeneration = 3L,
            runtimeGeneration = 2L,
            turnId = secondTurnId,
            outcome = "pending",
            projectId = PROJECT_A,
            projectGeneration = 1L,
            dialogueAct = "conversation",
            rendererProvenance = "",
        )

        val moved = conversations.moveToProject(CONVERSATION_ID, PROJECT_B)
        assertEquals(PROJECT_B, checkNotNull(moved.conversation(CONVERSATION_ID)).projectId)

        val fenced = checkNotNull(firstStore.loadSymbolicProjection(CONVERSATION_ID))
        fenced.assertPureSymbolic()
        assertEquals("cancelled", fenced.outcome)
        assertEquals(secondTurnId, fenced.turnId)
        assertEquals(3L, fenced.projectionGeneration)
        assertEquals(2L, fenced.runtimeGeneration)
        assertEquals(PROJECT_B, fenced.projectId)
        assertEquals(2L, fenced.projectGeneration)
        assertEquals("{}", fenced.dialogueStateJson)
        assertEquals("[]", fenced.discourseEntitiesJson)
        assertEquals("[]", fenced.unresolvedQuestionsJson)
        assertEquals("[]", fenced.expertEvidenceJson)
        assertEquals("[]", fenced.verifiedFactsJson)
        assertFalse(fenced.providersEnabled)
        assertEquals(0L, fenced.maxModelCalls)
        assertEquals(0L, fenced.providerCalls)
        assertEquals(0L, fenced.modelCalls)

        val secondTerminal = firstStore.loadMessages(CONVERSATION_ID)
            .single { it.role == HistoryMessageRole.Assistant && it.turnId == secondTurnId }
        assertEquals(HistoryMessageStatus.Cancelled, secondTerminal.status)
        assertFalse(
            firstStore.loadMessages(CONVERSATION_ID).any { message ->
                message.role == HistoryMessageRole.Assistant && message.status.isRunning()
            },
        )

        val lateInstall = runCatching {
            firstStore.saveSymbolicProjection(
                projection = staleProjectAPending,
                expectedGeneration = 2L,
            )
        }
        assertTrue(
            "project-B fence must reject the losing project-A pending install",
            lateInstall.isFailure,
        )
        firstStore.close()

        val reopenedStore = PortableConversationStore(context)
        try {
            val reopenedConversations = CanonicalConversationStore(
                history = reopenedStore,
                metadataFile = metadataFile,
                legacyFile = null,
                idFactory = { "unused" },
            )
            assertEquals(
                PROJECT_B,
                checkNotNull(reopenedConversations.state().conversation(CONVERSATION_ID)).projectId,
            )
            val recovered = checkNotNull(reopenedStore.loadSymbolicProjection(CONVERSATION_ID))
            recovered.assertPureSymbolic()
            assertEquals("cancelled", recovered.outcome)
            assertEquals(secondTurnId, recovered.turnId)
            assertEquals(3L, recovered.projectionGeneration)
            assertEquals(2L, recovered.runtimeGeneration)
            assertEquals(PROJECT_B, recovered.projectId)
            assertEquals(2L, recovered.projectGeneration)
            assertFalse(recovered.providersEnabled)
            assertEquals(0L, recovered.maxModelCalls)
            assertEquals(0L, recovered.providerCalls)
            assertEquals(0L, recovered.modelCalls)
        } finally {
            reopenedStore.close()
        }
    }

    private companion object {
        const val CONVERSATION_ID = "android-prior-projection-project-switch"
        const val PROJECT_A = "project-a"
        const val PROJECT_B = "project-b"
    }
}
