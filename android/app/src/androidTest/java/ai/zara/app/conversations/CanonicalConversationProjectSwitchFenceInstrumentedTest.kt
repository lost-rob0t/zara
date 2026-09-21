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
 * Acceptance for the Android conversation -> project stale-generation fence.
 *
 * A project switch is a logical context switch, not UI-only decoration. If a pure-symbolic turn is
 * still pending under project A when the canonical conversation is moved to project B, the project
 * switch must advance the existing symbolic projection before a late project-A callback can publish
 * output or Context1. The fence must remain durable across process/store recreation and must preserve
 * the exact zero-provider / zero-model policy.
 *
 * This test intentionally uses the existing CanonicalConversationStore + PortableConversationStore
 * owners. It must not be satisfied by a second project, conversation, or symbolic state store.
 */
class CanonicalConversationProjectSwitchFenceInstrumentedTest {
    private lateinit var context: Context
    private lateinit var metadataFile: File

    @Before
    fun setUp() {
        context = InstrumentationRegistry.getInstrumentation().targetContext
        context.deleteDatabase(ConversationHistoryContract.databaseName)
        metadataFile = File(context.cacheDir, "project-switch-fence-conversation-ui.bin")
        metadataFile.delete()
    }

    @After
    fun tearDown() {
        context.deleteDatabase(ConversationHistoryContract.databaseName)
        metadataFile.delete()
    }

    @Test
    fun projectSwitchFencesPendingProjectACompletionAndSurvivesRecreation() {
        val firstStore = PortableConversationStore(context)
        val conversations = CanonicalConversationStore(
            history = firstStore,
            metadataFile = metadataFile,
            legacyFile = null,
            idFactory = { CONVERSATION_ID },
        )
        assertEquals(CONVERSATION_ID, conversations.create(PROJECT_A).id)
        conversations.beginTurn(CONVERSATION_ID, "timer")

        val assistant = firstStore.loadMessages(CONVERSATION_ID).single { message ->
            message.role == HistoryMessageRole.Assistant
        }
        val turnId = checkNotNull(assistant.turnId)
        assertEquals(HistoryMessageStatus.Pending, assistant.status)

        val pendingA = pendingProjection(turnId)
        firstStore.saveSymbolicProjection(
            projection = pendingA,
            expectedGeneration = 0L,
        ).assertPureSymbolic()

        val moved = conversations.moveToProject(CONVERSATION_ID, PROJECT_B)
        assertEquals(PROJECT_B, checkNotNull(moved.conversation(CONVERSATION_ID)).projectId)

        val fencedB = checkNotNull(firstStore.loadSymbolicProjection(CONVERSATION_ID))
        fencedB.assertPureSymbolic()
        assertEquals("pending", fencedB.outcome)
        assertEquals(turnId, fencedB.turnId)
        assertEquals(PROJECT_B, fencedB.projectId)
        assertEquals(2L, fencedB.projectGeneration)
        assertEquals(2L, fencedB.projectionGeneration)
        assertEquals(RUNTIME_GENERATION, fencedB.runtimeGeneration)
        assertZeroModel(fencedB)

        val staleProjectACompletion = pendingA.copy(
            projectionGeneration = 2L,
            outcome = "success",
            dialogueStateJson = "{\"context\":\"late-project-a\"}",
            rendererProvenance = "symbolic-dcg/v1",
        )
        val staleBeforeRestart = runCatching {
            firstStore.completeSymbolicTurnAtomically(
                projection = staleProjectACompletion,
                expectedGeneration = 1L,
                turnId = turnId,
                assistantContent = "stale project A output",
                assistantStatus = HistoryMessageStatus.Complete,
            )
        }
        assertTrue(
            "project switch must reject a late project-A terminal commit before recreation",
            staleBeforeRestart.isFailure,
        )
        assertEquals(
            HistoryMessageStatus.Pending,
            firstStore.loadMessages(CONVERSATION_ID).single { it.role == HistoryMessageRole.Assistant }.status,
        )
        assertEquals(PROJECT_B, checkNotNull(firstStore.loadSymbolicProjection(CONVERSATION_ID)).projectId)
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
            assertEquals("interrupted", recovered.outcome)
            assertEquals(turnId, recovered.turnId)
            assertEquals(PROJECT_B, recovered.projectId)
            assertEquals(2L, recovered.projectGeneration)
            assertEquals(3L, recovered.projectionGeneration)
            assertEquals(RUNTIME_GENERATION, recovered.runtimeGeneration)
            assertZeroModel(recovered)

            val recoveredAssistant = reopenedStore.loadMessages(CONVERSATION_ID).single { message ->
                message.role == HistoryMessageRole.Assistant
            }
            assertEquals(HistoryMessageStatus.Cancelled, recoveredAssistant.status)

            val staleAfterRestart = runCatching {
                reopenedStore.completeSymbolicTurnAtomically(
                    projection = staleProjectACompletion,
                    expectedGeneration = 1L,
                    turnId = turnId,
                    assistantContent = "stale project A output after recreation",
                    assistantStatus = HistoryMessageStatus.Complete,
                )
            }
            assertTrue(
                "recreation must preserve rejection of the stale project-A terminal commit",
                staleAfterRestart.isFailure,
            )
            assertFalse(
                reopenedStore.loadMessages(CONVERSATION_ID).any { message ->
                    message.content.contains("stale project A output")
                },
            )
        } finally {
            reopenedStore.close()
        }
    }

    private fun pendingProjection(turnId: String): SymbolicConversationProjection =
        SymbolicConversationProjection(
            conversationId = CONVERSATION_ID,
            projectionGeneration = 1L,
            runtimeGeneration = RUNTIME_GENERATION,
            turnId = turnId,
            outcome = "pending",
            projectId = PROJECT_A,
            projectGeneration = 1L,
            dialogueAct = "clarify",
            dialogueStateJson = "{\"context\":\"project-a\"}",
            discourseEntitiesJson = "[]",
            unresolvedQuestionsJson = "[{\"slot\":\"duration\"}]",
            expertEvidenceJson = "[]",
            verifiedFactsJson = "[]",
            rendererProvenance = "",
            providersEnabled = false,
            maxModelCalls = 0L,
            providerCalls = 0L,
            modelCalls = 0L,
        )

    private fun assertZeroModel(projection: SymbolicConversationProjection) {
        assertFalse(projection.providersEnabled)
        assertEquals(0L, projection.maxModelCalls)
        assertEquals(0L, projection.providerCalls)
        assertEquals(0L, projection.modelCalls)
    }

    private companion object {
        const val CONVERSATION_ID = "android-project-switch-fence"
        const val PROJECT_A = "project-a"
        const val PROJECT_B = "project-b"
        const val RUNTIME_GENERATION = 7L
    }
}
