package ai.zara.app.conversations

import ai.zara.app.history.ConversationHistoryContract
import ai.zara.app.history.PortableConversationStore
import android.content.Context
import androidx.test.platform.app.InstrumentationRegistry
import java.io.DataInputStream
import java.io.File
import java.io.FileInputStream
import org.junit.After
import org.junit.Assert.assertEquals
import org.junit.Assert.assertNotNull
import org.junit.Assert.assertTrue
import org.junit.Before
import org.junit.Test

/**
 * P0 compensation fence for the split canonical-history / UI-metadata completion boundary.
 *
 * The remote id is staged durably before terminal history commit so metadata failures cannot expose
 * terminal output. If the canonical history write then fails, that staged metadata must be rolled
 * back durably and the exact same running turn must remain retryable.
 */
class CanonicalConversationRemoteCompletionCompensationInstrumentedTest {
    private lateinit var context: Context
    private lateinit var metadataRoot: File
    private lateinit var metadataFile: File

    @Before
    fun setUp() {
        context = InstrumentationRegistry.getInstrumentation().targetContext
        context.deleteDatabase(ConversationHistoryContract.databaseName)
        metadataRoot = File(context.cacheDir, "conversation-remote-completion-compensation")
        metadataRoot.deleteRecursively()
        assertTrue(metadataRoot.mkdirs())
        metadataFile = File(metadataRoot, "conversation-ui.bin")
    }

    @After
    fun tearDown() {
        context.deleteDatabase(ConversationHistoryContract.databaseName)
        metadataRoot.deleteRecursively()
    }

    @Test
    fun terminalHistoryFailureRollsBackStagedRemoteIdAndPreservesRetryableTurn() {
        PortableConversationStore(context).use { history ->
            val ui = CanonicalConversationStore(
                history = history,
                metadataFile = metadataFile,
                legacyFile = null,
                idFactory = { CONVERSATION_ID },
            )
            ui.create(projectId = "project-a")
            ui.beginTurn(CONVERSATION_ID, "continue the symbolic investigation")
            val turnId = requireNotNull(ui.runningTurnId(CONVERSATION_ID))

            history.writableDatabase.execSQL(
                """
                CREATE TEMP TRIGGER fail_terminal_message_update
                BEFORE UPDATE OF status, content ON desktop_messages
                WHEN OLD.status IN ('pending', 'streaming')
                  AND NEW.status IN ('complete', 'error')
                BEGIN
                    SELECT RAISE(ABORT, 'forced terminal commit failure');
                END
                """.trimIndent(),
            )

            val failure = runCatching {
                ui.completeTurn(
                    conversationId = CONVERSATION_ID,
                    assistantText = RESULT_TEXT,
                    success = true,
                    expectedTurnId = turnId,
                    remoteConversationId = REMOTE_CONVERSATION_ID,
                )
            }.exceptionOrNull()

            assertNotNull("forced canonical history failure must surface", failure)
            val failed = requireNotNull(ui.state().conversation(CONVERSATION_ID))
            assertEquals(ConversationStatus.Running, failed.status)
            assertEquals(null, failed.remoteConversationId)
            assertEquals(null, failed.turns.single().assistantText)
            assertEquals(turnId, ui.runningTurnId(CONVERSATION_ID))
            assertEquals(
                "staged remote id must be absent from the freshly-read sidecar after compensation",
                null,
                readSingleRemoteConversationId(),
            )

            history.writableDatabase.execSQL("DROP TRIGGER fail_terminal_message_update")
            val retried = ui.completeTurn(
                conversationId = CONVERSATION_ID,
                assistantText = RESULT_TEXT,
                success = true,
                expectedTurnId = turnId,
                remoteConversationId = REMOTE_CONVERSATION_ID,
            )
            val completed = requireNotNull(retried.conversation(CONVERSATION_ID))
            assertEquals(ConversationStatus.Success, completed.status)
            assertEquals(REMOTE_CONVERSATION_ID, completed.remoteConversationId)
            assertEquals(RESULT_TEXT, completed.turns.single().assistantText)
            assertEquals(REMOTE_CONVERSATION_ID, readSingleRemoteConversationId())
        }

        PortableConversationStore(context).use { reopenedHistory ->
            val reopenedUi = CanonicalConversationStore(
                history = reopenedHistory,
                metadataFile = metadataFile,
                legacyFile = null,
            )
            val reopened = requireNotNull(reopenedUi.state().conversation(CONVERSATION_ID))
            assertEquals(ConversationStatus.Success, reopened.status)
            assertEquals(REMOTE_CONVERSATION_ID, reopened.remoteConversationId)
            assertEquals(RESULT_TEXT, reopened.turns.single().assistantText)
        }
    }

    private fun readSingleRemoteConversationId(): String? =
        DataInputStream(FileInputStream(metadataFile).buffered()).use { input ->
            assertEquals("ZARA-CONVERSATION-UI/1", input.readUTF())
            readBoundedString(input)
            assertEquals(1, input.readInt())
            assertEquals(CONVERSATION_ID, readBoundedString(input))
            input.readBoolean()
            readBoundedString(input)
            readBoundedString(input).ifEmpty { null }
        }

    private fun readBoundedString(input: DataInputStream): String {
        val size = input.readInt()
        require(size >= 0)
        val bytes = ByteArray(size)
        input.readFully(bytes)
        return bytes.toString(Charsets.UTF_8)
    }

    private companion object {
        const val CONVERSATION_ID = "remote-completion-compensation"
        const val REMOTE_CONVERSATION_ID = "remote-compensated-id"
        const val RESULT_TEXT = "deterministic symbolic result"
    }
}
