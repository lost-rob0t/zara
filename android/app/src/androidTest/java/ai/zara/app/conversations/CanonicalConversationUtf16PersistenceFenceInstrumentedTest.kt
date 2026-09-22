package ai.zara.app.conversations

import ai.zara.app.history.ConversationHistoryContract
import ai.zara.app.history.PortableConversationStore
import android.content.Context
import androidx.test.platform.app.InstrumentationRegistry
import java.io.File
import org.junit.After
import org.junit.Assert.assertEquals
import org.junit.Assert.assertNull
import org.junit.Assert.assertThrows
import org.junit.Before
import org.junit.Test

/**
 * Android strings can contain lone UTF-16 surrogate code units even though those values cannot be
 * represented canonically on Zara's UTF-8 persistence/wire surfaces. Reject them before any
 * conversation/history mutation so process recreation cannot silently replace logical identity or
 * user-visible text with encoder replacement characters.
 */
class CanonicalConversationUtf16PersistenceFenceInstrumentedTest {
    private lateinit var context: Context
    private lateinit var metadataFile: File

    @Before
    fun setUp() {
        context = InstrumentationRegistry.getInstrumentation().targetContext
        context.deleteDatabase(ConversationHistoryContract.databaseName)
        metadataFile = File(context.cacheDir, "conversation-utf16-fence.bin")
        metadataFile.delete()
    }

    @After
    fun tearDown() {
        context.deleteDatabase(ConversationHistoryContract.databaseName)
        metadataFile.delete()
    }

    @Test
    fun malformedUtf16FailsBeforeConversationIdentityOrTurnPersistence() {
        PortableConversationStore(context).use { history ->
            val malformedId = "conversation-${LONE_HIGH_SURROGATE}"
            val malformedProject = "project:${LONE_LOW_SURROGATE}"

            val malformedIdStore = CanonicalConversationStore(
                history = history,
                metadataFile = metadataFile,
                legacyFile = null,
                idFactory = { malformedId },
            )
            assertThrows(IllegalArgumentException::class.java) {
                malformedIdStore.create()
            }
            assertNull(history.getConversation(malformedId))

            val store = CanonicalConversationStore(
                history = history,
                metadataFile = metadataFile,
                legacyFile = null,
                idFactory = { CONVERSATION_ID },
            )
            assertThrows(IllegalArgumentException::class.java) {
                store.create(projectId = malformedProject)
            }
            assertNull(history.getConversation(CONVERSATION_ID))

            store.create(projectId = "project:stable")
            assertThrows(IllegalArgumentException::class.java) {
                store.beginTurn(CONVERSATION_ID, "hello ${LONE_HIGH_SURROGATE}")
            }
            assertEquals(0, history.loadMessages(CONVERSATION_ID).size)

            store.beginTurn(CONVERSATION_ID, "hello")
            val turnId = requireNotNull(store.runningTurnId(CONVERSATION_ID))
            assertThrows(IllegalArgumentException::class.java) {
                store.completeTurn(
                    conversationId = CONVERSATION_ID,
                    assistantText = "ok",
                    success = true,
                    expectedTurnId = turnId,
                    remoteConversationId = "remote:${LONE_LOW_SURROGATE}",
                )
            }
            assertEquals(turnId, store.runningTurnId(CONVERSATION_ID))
        }
    }

    private companion object {
        const val CONVERSATION_ID = "android-utf16-persistence-fence"
        const val LONE_HIGH_SURROGATE = '\uD800'
        const val LONE_LOW_SURROGATE = '\uDC00'
    }
}
