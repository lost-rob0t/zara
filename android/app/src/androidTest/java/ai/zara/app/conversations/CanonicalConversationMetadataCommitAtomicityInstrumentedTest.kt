package ai.zara.app.conversations

import ai.zara.app.history.ConversationHistoryContract
import ai.zara.app.history.PortableConversationStore
import android.content.Context
import androidx.test.platform.app.InstrumentationRegistry
import java.io.File
import org.junit.After
import org.junit.Assert.assertEquals
import org.junit.Assert.assertFalse
import org.junit.Assert.assertNotNull
import org.junit.Assert.assertTrue
import org.junit.Before
import org.junit.Test

/**
 * P0 durability fence for UI-side metadata mutations.
 *
 * A failed sidecar commit must not leak the uncommitted selection/pin mutation through the live
 * process. Callers may retry after storage recovers, and the retry must become the durable state.
 */
class CanonicalConversationMetadataCommitAtomicityInstrumentedTest {
    private lateinit var context: Context
    private lateinit var metadataRoot: File
    private lateinit var metadataFile: File

    @Before
    fun setUp() {
        context = InstrumentationRegistry.getInstrumentation().targetContext
        context.deleteDatabase(ConversationHistoryContract.databaseName)
        metadataRoot = File(context.cacheDir, "conversation-metadata-commit-atomicity")
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
    fun failedPinCommitDoesNotLeakInMemoryAndCanBeRetriedDurably() {
        PortableConversationStore(context).use { history ->
            val ui = CanonicalConversationStore(
                history = history,
                metadataFile = metadataFile,
                legacyFile = null,
                idFactory = { CONVERSATION_A },
            )
            ui.create(projectId = "project-a")
            assertFalse(ui.state().conversations.single { it.id == CONVERSATION_A }.pinned)

            blockMetadataParent()
            val failure = runCatching {
                ui.setPinned(CONVERSATION_A, true)
            }.exceptionOrNull()

            assertNotNull("metadata persistence must fail for a non-directory parent", failure)
            assertFalse(ui.state().conversations.single { it.id == CONVERSATION_A }.pinned)

            restoreMetadataParent()
            val retried = ui.setPinned(CONVERSATION_A, true)
            assertTrue(retried.conversations.single { it.id == CONVERSATION_A }.pinned)
        }

        PortableConversationStore(context).use { reopenedHistory ->
            val reopenedUi = CanonicalConversationStore(
                history = reopenedHistory,
                metadataFile = metadataFile,
                legacyFile = null,
            )
            assertTrue(reopenedUi.state().conversations.single { it.id == CONVERSATION_A }.pinned)
        }
    }

    @Test
    fun failedSelectionCommitRestoresPreviousSelectionAndCanBeRetriedDurably() {
        val ids = listOf(CONVERSATION_A, CONVERSATION_B).iterator()
        PortableConversationStore(context).use { history ->
            val ui = CanonicalConversationStore(
                history = history,
                metadataFile = metadataFile,
                legacyFile = null,
                idFactory = { ids.next() },
            )
            ui.create(projectId = "project-a")
            ui.create(projectId = "project-b")
            ui.select(CONVERSATION_A)
            assertEquals(CONVERSATION_A, ui.state().selectedConversationId)

            blockMetadataParent()
            val failure = runCatching {
                ui.select(CONVERSATION_B)
            }.exceptionOrNull()

            assertNotNull("metadata persistence must fail for a non-directory parent", failure)
            assertEquals(CONVERSATION_A, ui.state().selectedConversationId)

            restoreMetadataParent()
            assertEquals(CONVERSATION_B, ui.select(CONVERSATION_B).selectedConversationId)
        }

        PortableConversationStore(context).use { reopenedHistory ->
            val reopenedUi = CanonicalConversationStore(
                history = reopenedHistory,
                metadataFile = metadataFile,
                legacyFile = null,
            )
            assertEquals(CONVERSATION_B, reopenedUi.state().selectedConversationId)
        }
    }

    private fun blockMetadataParent() {
        assertTrue(metadataFile.delete())
        assertTrue(metadataRoot.delete())
        metadataRoot.writeText("not-a-directory")
        assertTrue(metadataRoot.isFile)
    }

    private fun restoreMetadataParent() {
        assertTrue(metadataRoot.delete())
        assertTrue(metadataRoot.mkdirs())
    }

    private companion object {
        const val CONVERSATION_A = "metadata-atomicity-a"
        const val CONVERSATION_B = "metadata-atomicity-b"
    }
}
