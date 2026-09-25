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
import org.junit.Assert.assertTrue
import org.junit.Before
import org.junit.Test

/**
 * Remote transport identity is durable conversation metadata, not turn-local output.
 *
 * A terminal replay may repair missing UI metadata after a crash, but neither a terminal replay nor
 * completion of a currently pending turn may replace an already-durable remote conversation binding.
 * Otherwise a late callback from stale transport state can mutate logical conversation identity.
 */
class CanonicalConversationRemoteIdStaleCompletionFenceInstrumentedTest {
    private lateinit var context: Context
    private lateinit var metadataFile: File

    @Before
    fun setUp() {
        context = InstrumentationRegistry.getInstrumentation().targetContext
        context.deleteDatabase(ConversationHistoryContract.databaseName)
        metadataFile = File(context.cacheDir, "remote-id-stale-completion-fence.bin")
        metadataFile.delete()
    }

    @After
    fun tearDown() {
        context.deleteDatabase(ConversationHistoryContract.databaseName)
        metadataFile.delete()
    }

    @Test
    fun terminalReplayCannotReplaceDurableRemoteConversationBinding() {
        val history = PortableConversationStore(context)
        try {
            val store = CanonicalConversationStore(
                history = history,
                metadataFile = metadataFile,
                legacyFile = null,
                idFactory = { CONVERSATION_ID },
            )
            store.create()
            store.beginTurn(CONVERSATION_ID, "inspect alex")
            val turnId = checkNotNull(store.runningTurnId(CONVERSATION_ID))

            store.completeTurn(
                conversationId = CONVERSATION_ID,
                assistantText = TERMINAL_TEXT,
                success = true,
                expectedTurnId = turnId,
                remoteConversationId = CURRENT_REMOTE_ID,
            )
            assertEquals(
                CURRENT_REMOTE_ID,
                checkNotNull(store.state().conversation(CONVERSATION_ID)).remoteConversationId,
            )

            val staleReplay = runCatching {
                store.completeTurn(
                    conversationId = CONVERSATION_ID,
                    assistantText = TERMINAL_TEXT,
                    success = true,
                    expectedTurnId = turnId,
                    remoteConversationId = STALE_REMOTE_ID,
                )
            }
            assertTrue(
                "an idempotent terminal replay must fail closed before replacing durable remote identity",
                staleReplay.isFailure,
            )

            val terminal = history.loadMessages(CONVERSATION_ID).single { message ->
                message.role == HistoryMessageRole.Assistant && message.turnId == turnId
            }
            assertEquals(HistoryMessageStatus.Complete, terminal.status)
            assertEquals(TERMINAL_TEXT, terminal.content)
            assertEquals(
                CURRENT_REMOTE_ID,
                checkNotNull(store.state().conversation(CONVERSATION_ID)).remoteConversationId,
            )
        } finally {
            history.close()
        }

        val reopenedHistory = PortableConversationStore(context)
        try {
            val reopened = CanonicalConversationStore(
                history = reopenedHistory,
                metadataFile = metadataFile,
                legacyFile = null,
                idFactory = { "unused" },
            )
            assertEquals(
                CURRENT_REMOTE_ID,
                checkNotNull(reopened.state().conversation(CONVERSATION_ID)).remoteConversationId,
            )
        } finally {
            reopenedHistory.close()
        }
    }

    @Test
    fun pendingCompletionCannotReplaceDurableRemoteConversationBindingBeforeTurnMutation() {
        val history = PortableConversationStore(context)
        var secondTurnId = ""
        try {
            val store = CanonicalConversationStore(
                history = history,
                metadataFile = metadataFile,
                legacyFile = null,
                idFactory = { CONVERSATION_ID },
            )
            store.create()
            store.beginTurn(CONVERSATION_ID, "inspect alex")
            val firstTurnId = checkNotNull(store.runningTurnId(CONVERSATION_ID))
            store.completeTurn(
                conversationId = CONVERSATION_ID,
                assistantText = TERMINAL_TEXT,
                success = true,
                expectedTurnId = firstTurnId,
                remoteConversationId = CURRENT_REMOTE_ID,
            )

            store.beginTurn(CONVERSATION_ID, "why?")
            secondTurnId = checkNotNull(store.runningTurnId(CONVERSATION_ID))
            val staleCompletion = runCatching {
                store.completeTurn(
                    conversationId = CONVERSATION_ID,
                    assistantText = STALE_PENDING_TEXT,
                    success = true,
                    expectedTurnId = secondTurnId,
                    remoteConversationId = STALE_REMOTE_ID,
                )
            }
            assertTrue(
                "pending completion must fail closed before stale transport identity mutates the turn",
                staleCompletion.isFailure,
            )

            val pendingAfterStale = history.loadMessages(CONVERSATION_ID).single { message ->
                message.role == HistoryMessageRole.Assistant && message.turnId == secondTurnId
            }
            assertEquals(HistoryMessageStatus.Pending, pendingAfterStale.status)
            assertEquals("", pendingAfterStale.content)
            assertEquals("", pendingAfterStale.error)
            assertEquals(
                CURRENT_REMOTE_ID,
                checkNotNull(store.state().conversation(CONVERSATION_ID)).remoteConversationId,
            )

            store.completeTurn(
                conversationId = CONVERSATION_ID,
                assistantText = VALID_PENDING_TEXT,
                success = true,
                expectedTurnId = secondTurnId,
                remoteConversationId = CURRENT_REMOTE_ID,
            )
            val completed = history.loadMessages(CONVERSATION_ID).single { message ->
                message.role == HistoryMessageRole.Assistant && message.turnId == secondTurnId
            }
            assertEquals(HistoryMessageStatus.Complete, completed.status)
            assertEquals(VALID_PENDING_TEXT, completed.content)
        } finally {
            history.close()
        }

        val reopenedHistory = PortableConversationStore(context)
        try {
            val reopened = CanonicalConversationStore(
                history = reopenedHistory,
                metadataFile = metadataFile,
                legacyFile = null,
                idFactory = { "unused" },
            )
            assertEquals(
                CURRENT_REMOTE_ID,
                checkNotNull(reopened.state().conversation(CONVERSATION_ID)).remoteConversationId,
            )
            val durable = reopenedHistory.loadMessages(CONVERSATION_ID).single { message ->
                message.role == HistoryMessageRole.Assistant && message.turnId == secondTurnId
            }
            assertEquals(HistoryMessageStatus.Complete, durable.status)
            assertEquals(VALID_PENDING_TEXT, durable.content)
        } finally {
            reopenedHistory.close()
        }
    }

    private companion object {
        const val CONVERSATION_ID = "android-remote-id-stale-completion-fence"
        const val CURRENT_REMOTE_ID = "remote-current"
        const val STALE_REMOTE_ID = "remote-stale"
        const val TERMINAL_TEXT = "Alex is already registered."
        const val STALE_PENDING_TEXT = "stale transport output"
        const val VALID_PENDING_TEXT = "The evidence still points to Alex."
    }
}
