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
import org.junit.Assert.assertNull
import org.junit.Assert.assertTrue
import org.junit.Before
import org.junit.Test

/**
 * P0 acceptance for Android UI conversation selection/history durability.
 *
 * The canonical SQLite history owner and the UI-only metadata sidecar must survive process/store
 * recreation together. Reopening the stores must not silently select another conversation, lose
 * either transcript, or create a second history owner.
 */
class CanonicalConversationSelectionRecreationInstrumentedTest {
    private lateinit var context: Context
    private lateinit var metadataFile: File

    @Before
    fun setUp() {
        context = InstrumentationRegistry.getInstrumentation().targetContext
        context.deleteDatabase(ConversationHistoryContract.databaseName)
        metadataFile = File(context.cacheDir, "selection-recreation-conversation-ui.bin")
        metadataFile.delete()
    }

    @After
    fun tearDown() {
        context.deleteDatabase(ConversationHistoryContract.databaseName)
        metadataFile.delete()
    }

    @Test
    fun selectedConversationAndCanonicalHistorySurviveStoreRecreation() {
        var nextId = 0
        val ids = listOf(FIRST_CONVERSATION_ID, SECOND_CONVERSATION_ID)
        val firstHistory = PortableConversationStore(context)
        val firstUi = CanonicalConversationStore(
            history = firstHistory,
            metadataFile = metadataFile,
            legacyFile = null,
            idFactory = { ids[nextId++] },
        )

        val first = firstUi.create()
        assertEquals(FIRST_CONVERSATION_ID, first.id)
        firstUi.beginTurn(first.id, "timer")
        val firstTurnId = checkNotNull(firstUi.runningTurnId(first.id))
        firstUi.completeTurn(
            conversationId = first.id,
            assistantText = "How long should I set the timer for?",
            success = true,
            expectedTurnId = firstTurnId,
        )

        val second = firstUi.create()
        assertEquals(SECOND_CONVERSATION_ID, second.id)
        firstUi.beginTurn(second.id, "thanks")
        val secondTurnId = checkNotNull(firstUi.runningTurnId(second.id))
        firstUi.completeTurn(
            conversationId = second.id,
            assistantText = "You're welcome.",
            success = true,
            expectedTurnId = secondTurnId,
        )

        val selected = firstUi.select(first.id)
        assertNull(selected.loadFailure)
        assertEquals(FIRST_CONVERSATION_ID, selected.selectedConversationId)
        assertEquals(2, selected.conversations.size)
        firstHistory.close()

        val reopenedHistory = PortableConversationStore(context)
        try {
            val reopenedUi = CanonicalConversationStore(
                history = reopenedHistory,
                metadataFile = metadataFile,
                legacyFile = null,
                idFactory = { "unused" },
            )
            val reopened = reopenedUi.state()

            assertNull(reopened.loadFailure)
            assertEquals(FIRST_CONVERSATION_ID, reopened.selectedConversationId)
            assertEquals(2, reopened.conversations.size)
            assertEquals(
                "How long should I set the timer for?",
                checkNotNull(reopened.conversation(FIRST_CONVERSATION_ID))
                    .turns.single()
                    .assistantText,
            )
            assertEquals(
                "You're welcome.",
                checkNotNull(reopened.conversation(SECOND_CONVERSATION_ID))
                    .turns.single()
                    .assistantText,
            )

            val firstMessages = reopenedHistory.loadMessages(FIRST_CONVERSATION_ID)
            val secondMessages = reopenedHistory.loadMessages(SECOND_CONVERSATION_ID)
            assertTrue(firstMessages.any { message ->
                message.role == HistoryMessageRole.Assistant &&
                    message.status == HistoryMessageStatus.Complete &&
                    message.turnId == firstTurnId &&
                    message.content == "How long should I set the timer for?"
            })
            assertTrue(secondMessages.any { message ->
                message.role == HistoryMessageRole.Assistant &&
                    message.status == HistoryMessageStatus.Complete &&
                    message.turnId == secondTurnId &&
                    message.content == "You're welcome."
            })
        } finally {
            reopenedHistory.close()
        }
    }

    @Test
    fun selectionChangedAfterRecreationSurvivesAnotherRecreationWithoutLosingHistory() {
        var nextId = 0
        val ids = listOf(FIRST_CONVERSATION_ID, SECOND_CONVERSATION_ID)

        PortableConversationStore(context).use { history ->
            val ui = CanonicalConversationStore(
                history = history,
                metadataFile = metadataFile,
                legacyFile = null,
                idFactory = { ids[nextId++] },
            )
            ui.create()
            ui.beginTurn(FIRST_CONVERSATION_ID, "timer")
            val firstTurnId = checkNotNull(ui.runningTurnId(FIRST_CONVERSATION_ID))
            ui.completeTurn(
                conversationId = FIRST_CONVERSATION_ID,
                assistantText = "How long should I set the timer for?",
                success = true,
                expectedTurnId = firstTurnId,
            )

            ui.create()
            ui.beginTurn(SECOND_CONVERSATION_ID, "thanks")
            val secondTurnId = checkNotNull(ui.runningTurnId(SECOND_CONVERSATION_ID))
            ui.completeTurn(
                conversationId = SECOND_CONVERSATION_ID,
                assistantText = "You're welcome.",
                success = true,
                expectedTurnId = secondTurnId,
            )
            assertEquals(FIRST_CONVERSATION_ID, ui.select(FIRST_CONVERSATION_ID).selectedConversationId)
        }

        PortableConversationStore(context).use { history ->
            val ui = CanonicalConversationStore(
                history = history,
                metadataFile = metadataFile,
                legacyFile = null,
                idFactory = { "unused" },
            )
            assertEquals(FIRST_CONVERSATION_ID, ui.state().selectedConversationId)
            assertEquals(SECOND_CONVERSATION_ID, ui.select(SECOND_CONVERSATION_ID).selectedConversationId)
        }

        PortableConversationStore(context).use { history ->
            val ui = CanonicalConversationStore(
                history = history,
                metadataFile = metadataFile,
                legacyFile = null,
                idFactory = { "unused" },
            )
            val reopened = ui.state()

            assertNull(reopened.loadFailure)
            assertEquals(SECOND_CONVERSATION_ID, reopened.selectedConversationId)
            assertEquals(2, reopened.conversations.size)
            assertEquals(
                "How long should I set the timer for?",
                checkNotNull(reopened.conversation(FIRST_CONVERSATION_ID))
                    .turns.single()
                    .assistantText,
            )
            assertEquals(
                "You're welcome.",
                checkNotNull(reopened.conversation(SECOND_CONVERSATION_ID))
                    .turns.single()
                    .assistantText,
            )
            assertEquals(2, history.listConversations(limit = 10).size)
        }
    }

    private companion object {
        const val FIRST_CONVERSATION_ID = "selection-recreation-a"
        const val SECOND_CONVERSATION_ID = "selection-recreation-b"
    }
}
