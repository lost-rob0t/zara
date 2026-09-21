package ai.zara.app.conversations

import ai.zara.app.history.ConversationHistoryContract
import ai.zara.app.history.HistoryMessageRole
import ai.zara.app.history.HistoryMessageStatus
import ai.zara.app.history.PortableConversationStore
import android.content.Context
import androidx.test.platform.app.InstrumentationRegistry
import java.io.File
import org.junit.After
import org.junit.Assert.assertEquals
import org.junit.Assert.assertFalse
import org.junit.Assert.assertNotEquals
import org.junit.Assert.assertNull
import org.junit.Assert.assertTrue
import org.junit.Before
import org.junit.Test

/**
 * Proves a late UI callback from a pre-recreation turn cannot settle a newer canonical turn.
 *
 * The turn identity comes from the existing zara.db history owner. No generation cache or second
 * conversation store is introduced for the UI layer.
 */
class CanonicalConversationStaleUiCompletionFenceInstrumentedTest {
    private lateinit var context: Context
    private lateinit var metadataFile: File

    @Before
    fun setUp() {
        context = InstrumentationRegistry.getInstrumentation().targetContext
        context.deleteDatabase(ConversationHistoryContract.databaseName)
        metadataFile = File(context.cacheDir, "stale-ui-completion-fence.bin")
        metadataFile.delete()
    }

    @After
    fun tearDown() {
        context.deleteDatabase(ConversationHistoryContract.databaseName)
        metadataFile.delete()
    }

    @Test
    fun recreatedConversationRejectsLateOldCompletionWithoutTouchingNewPendingTurn() {
        val firstHistory = PortableConversationStore(context)
        val first = CanonicalConversationStore(
            history = firstHistory,
            metadataFile = metadataFile,
            legacyFile = null,
            idFactory = { CONVERSATION_ID },
        )
        first.create()
        first.beginTurn(CONVERSATION_ID, "timer")
        val oldTurnId = checkNotNull(first.runningTurnId(CONVERSATION_ID))
        firstHistory.close()

        val reopenedHistory = PortableConversationStore(context)
        try {
            val reopened = CanonicalConversationStore(
                history = reopenedHistory,
                metadataFile = metadataFile,
                legacyFile = null,
                idFactory = { "unused" },
            )
            val interruptedOld = reopenedHistory.loadMessages(CONVERSATION_ID).single { message ->
                message.role == HistoryMessageRole.Assistant && message.turnId == oldTurnId
            }
            assertEquals(HistoryMessageStatus.Cancelled, interruptedOld.status)

            reopened.beginTurn(CONVERSATION_ID, "5 minutes")
            val newTurnId = checkNotNull(reopened.runningTurnId(CONVERSATION_ID))
            assertNotEquals(oldTurnId, newTurnId)

            val stale = runCatching {
                reopened.completeTurn(
                    conversationId = CONVERSATION_ID,
                    assistantText = "late old generation output",
                    success = true,
                    expectedTurnId = oldTurnId,
                )
            }
            assertTrue("late completion for the interrupted turn must fail closed", stale.isFailure)
            assertEquals(
                "the stale callback must not select the newer running assistant row",
                newTurnId,
                reopened.runningTurnId(CONVERSATION_ID),
            )

            val messagesAfterStale = reopenedHistory.loadMessages(CONVERSATION_ID)
            val oldAfterStale = messagesAfterStale.single { message ->
                message.role == HistoryMessageRole.Assistant && message.turnId == oldTurnId
            }
            val newAfterStale = messagesAfterStale.single { message ->
                message.role == HistoryMessageRole.Assistant && message.turnId == newTurnId
            }
            assertEquals(HistoryMessageStatus.Cancelled, oldAfterStale.status)
            assertEquals(HistoryMessageStatus.Pending, newAfterStale.status)
            assertFalse(messagesAfterStale.any { it.content == "late old generation output" })

            reopened.completeTurn(
                conversationId = CONVERSATION_ID,
                assistantText = "new generation output",
                success = true,
                expectedTurnId = newTurnId,
            )
            val finalMessages = reopenedHistory.loadMessages(CONVERSATION_ID)
            val newTerminal = finalMessages.single { message ->
                message.role == HistoryMessageRole.Assistant && message.turnId == newTurnId
            }
            assertEquals(HistoryMessageStatus.Complete, newTerminal.status)
            assertEquals("new generation output", newTerminal.content)
            assertNull(reopened.runningTurnId(CONVERSATION_ID))
        } finally {
            reopenedHistory.close()
        }
    }

    @Test
    fun failedExactTurnRemainsTerminalAcrossRecreationAndRejectsLateSuccess() {
        val firstHistory = PortableConversationStore(context)
        val first = CanonicalConversationStore(
            history = firstHistory,
            metadataFile = metadataFile,
            legacyFile = null,
            idFactory = { CONVERSATION_ID },
        )
        first.create()
        first.beginTurn(CONVERSATION_ID, "timer")
        val failedTurnId = checkNotNull(first.runningTurnId(CONVERSATION_ID))
        first.failTurn(
            conversationId = CONVERSATION_ID,
            message = FAILURE_TEXT,
            expectedTurnId = failedTurnId,
        )
        assertNull(first.runningTurnId(CONVERSATION_ID))
        firstHistory.close()

        val reopenedHistory = PortableConversationStore(context)
        try {
            val reopened = CanonicalConversationStore(
                history = reopenedHistory,
                metadataFile = metadataFile,
                legacyFile = null,
                idFactory = { "unused" },
            )
            val persisted = reopenedHistory.loadMessages(CONVERSATION_ID).single { message ->
                message.role == HistoryMessageRole.Assistant && message.turnId == failedTurnId
            }
            assertEquals(HistoryMessageStatus.Error, persisted.status)
            assertEquals(FAILURE_TEXT, persisted.content)
            assertEquals(FAILURE_TEXT, persisted.error)
            assertNull(reopened.runningTurnId(CONVERSATION_ID))
            assertEquals(
                ConversationStatus.Failed,
                checkNotNull(reopened.state().conversation(CONVERSATION_ID)).status,
            )

            val staleSuccess = runCatching {
                reopened.completeTurn(
                    conversationId = CONVERSATION_ID,
                    assistantText = "late success must not replace durable failure",
                    success = true,
                    expectedTurnId = failedTurnId,
                )
            }
            assertTrue("a durable failed turn must reject late success", staleSuccess.isFailure)

            val afterStale = reopenedHistory.loadMessages(CONVERSATION_ID).single { message ->
                message.role == HistoryMessageRole.Assistant && message.turnId == failedTurnId
            }
            assertEquals(HistoryMessageStatus.Error, afterStale.status)
            assertEquals(FAILURE_TEXT, afterStale.content)
            assertEquals(FAILURE_TEXT, afterStale.error)
            assertFalse(
                reopenedHistory.loadMessages(CONVERSATION_ID).any {
                    it.content == "late success must not replace durable failure"
                },
            )
        } finally {
            reopenedHistory.close()
        }
    }

    @Test
    fun fullWidthProjectIdRoundTripsAcrossConversationStoreRecreation() {
        val fullWidthProjectId = "p".repeat(512)
        val firstHistory = PortableConversationStore(context)
        val first = CanonicalConversationStore(
            history = firstHistory,
            metadataFile = metadataFile,
            legacyFile = null,
            idFactory = { CONVERSATION_ID },
        )
        assertEquals(
            fullWidthProjectId,
            first.create(projectId = fullWidthProjectId).projectId,
        )
        firstHistory.close()

        val reopenedHistory = PortableConversationStore(context)
        try {
            val reopened = CanonicalConversationStore(
                history = reopenedHistory,
                metadataFile = metadataFile,
                legacyFile = null,
                idFactory = { "unused" },
            )
            assertEquals(
                fullWidthProjectId,
                checkNotNull(reopened.state().conversation(CONVERSATION_ID)).projectId,
            )
        } finally {
            reopenedHistory.close()
        }
    }

    private companion object {
        const val CONVERSATION_ID = "android-stale-ui-completion-fence"
        const val FAILURE_TEXT = "symbolic turn failed durably"
    }
}
