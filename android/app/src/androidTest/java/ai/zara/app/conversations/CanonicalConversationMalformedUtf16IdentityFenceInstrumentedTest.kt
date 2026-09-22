package ai.zara.app.conversations

import ai.zara.app.history.ConversationHistoryContract
import ai.zara.app.history.PortableConversationStore
import android.content.Context
import androidx.test.platform.app.InstrumentationRegistry
import java.io.File
import org.junit.After
import org.junit.Assert.assertThrows
import org.junit.Before
import org.junit.Test

/**
 * Android/JVM strings can contain lone UTF-16 surrogate code units even though those values are
 * not portable Unicode scalar sequences. Conversation/project/remote identities must reject them
 * before persistence; otherwise UTF-8 encoding can silently replace the invalid code unit and
 * restart recovery observes a different logical identity.
 */
class CanonicalConversationMalformedUtf16IdentityFenceInstrumentedTest {
    private lateinit var context: Context
    private lateinit var metadataFile: File

    @Before
    fun setUp() {
        context = InstrumentationRegistry.getInstrumentation().targetContext
        context.deleteDatabase(ConversationHistoryContract.databaseName)
        metadataFile = File(context.cacheDir, "malformed-utf16-conversation-ui-metadata.bin")
        metadataFile.delete()
    }

    @After
    fun tearDown() {
        context.deleteDatabase(ConversationHistoryContract.databaseName)
        metadataFile.delete()
    }

    @Test
    fun malformedUtf16ConversationIdentityFailsBeforeHistoryCreation() {
        PortableConversationStore(context).use { history ->
            val store = CanonicalConversationStore(
                history = history,
                metadataFile = metadataFile,
                legacyFile = null,
                idFactory = { "conversation:\uD800" },
            )

            assertThrows(IllegalArgumentException::class.java) {
                store.create(projectId = "project:valid")
            }
        }
    }

    @Test
    fun malformedUtf16ProjectIdentityFailsBeforeMetadataPersistence() {
        PortableConversationStore(context).use { history ->
            val store = CanonicalConversationStore(
                history = history,
                metadataFile = metadataFile,
                legacyFile = null,
                idFactory = { CONVERSATION_ID },
            )

            assertThrows(IllegalArgumentException::class.java) {
                store.create(projectId = "project:\uDC00")
            }
        }
    }

    @Test
    fun malformedUtf16RemoteIdentityCannotBindCompletedTurn() {
        PortableConversationStore(context).use { history ->
            val store = CanonicalConversationStore(
                history = history,
                metadataFile = metadataFile,
                legacyFile = null,
                idFactory = { CONVERSATION_ID },
            )
            store.create(projectId = "project:valid")
            val running = store.beginTurn(CONVERSATION_ID, "hello")
            val turnId = requireNotNull(running.conversation(CONVERSATION_ID))
                .turns
                .last()
                .let { store.runningTurnId(CONVERSATION_ID) }

            assertThrows(IllegalArgumentException::class.java) {
                store.completeTurn(
                    conversationId = CONVERSATION_ID,
                    assistantText = "hi",
                    success = true,
                    expectedTurnId = turnId,
                    remoteConversationId = "remote:\uD800",
                )
            }
        }
    }

    private companion object {
        const val CONVERSATION_ID = "android-malformed-utf16-identity-fence"
    }
}
