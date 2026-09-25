package ai.zara.app.conversations

import ai.zara.app.history.ConversationHistoryContract
import ai.zara.app.history.PortableConversationStore
import android.content.Context
import androidx.test.platform.app.InstrumentationRegistry
import java.io.File
import org.junit.After
import org.junit.Assert.assertNotNull
import org.junit.Assert.assertNull
import org.junit.Assert.assertTrue
import org.junit.Before
import org.junit.Test

/**
 * P0 durability fence for canonical conversation creation.
 *
 * A failed UI-metadata commit must not leave a newly-created canonical history row behind. A
 * caller that observes create() failure must be able to retry after process recreation without
 * discovering a ghost conversation that was never durably selected/bound by the UI sidecar.
 */
class CanonicalConversationCreatePersistenceAtomicityInstrumentedTest {
    private lateinit var context: Context
    private lateinit var blockingParent: File
    private lateinit var metadataFile: File

    @Before
    fun setUp() {
        context = InstrumentationRegistry.getInstrumentation().targetContext
        context.deleteDatabase(ConversationHistoryContract.databaseName)
        blockingParent = File(context.cacheDir, "conversation-create-atomicity-parent")
        blockingParent.deleteRecursively()
        blockingParent.writeText("not-a-directory")
        metadataFile = File(blockingParent, "conversation-ui.bin")
    }

    @After
    fun tearDown() {
        context.deleteDatabase(ConversationHistoryContract.databaseName)
        blockingParent.deleteRecursively()
    }

    @Test
    fun metadataCommitFailureDoesNotLeaveGhostConversationInCanonicalHistory() {
        PortableConversationStore(context).use { history ->
            val ui = CanonicalConversationStore(
                history = history,
                metadataFile = metadataFile,
                legacyFile = null,
                idFactory = { CONVERSATION_ID },
            )

            val failure = runCatching {
                ui.create(projectId = "project-a")
            }.exceptionOrNull()

            assertNotNull("metadata persistence must fail for a non-directory parent", failure)
            assertNull(history.getConversation(CONVERSATION_ID))
            assertTrue(history.listConversations(limit = 10).none { it.id == CONVERSATION_ID })
        }

        PortableConversationStore(context).use { reopened ->
            assertNull(reopened.getConversation(CONVERSATION_ID))
            assertTrue(reopened.listConversations(limit = 10).none { it.id == CONVERSATION_ID })
        }
    }

    private companion object {
        const val CONVERSATION_ID = "create-persistence-atomicity"
    }
}
