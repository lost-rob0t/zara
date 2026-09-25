package ai.zara.app.conversations

import ai.zara.app.history.ConversationHistoryContract
import ai.zara.app.history.PortableConversationStore
import android.content.Context
import androidx.test.platform.app.InstrumentationRegistry
import java.io.DataOutputStream
import java.io.File
import java.io.FileOutputStream
import java.nio.charset.StandardCharsets
import org.junit.After
import org.junit.Assert.assertEquals
import org.junit.Assert.assertThrows
import org.junit.Before
import org.junit.Test

/**
 * The Android sidecar is UI metadata over the canonical history owner, not an alternate source of
 * conversation identity. Duplicate conversation rows therefore represent corrupt/non-canonical
 * state and must fail closed instead of silently selecting whichever duplicate was decoded last.
 */
class CanonicalConversationDuplicateMetadataFenceInstrumentedTest {
    private lateinit var context: Context
    private lateinit var metadataFile: File

    @Before
    fun setUp() {
        context = InstrumentationRegistry.getInstrumentation().targetContext
        context.deleteDatabase(ConversationHistoryContract.databaseName)
        metadataFile = File(context.cacheDir, "duplicate-conversation-ui-metadata.bin")
        metadataFile.delete()
    }

    @After
    fun tearDown() {
        context.deleteDatabase(ConversationHistoryContract.databaseName)
        metadataFile.delete()
    }

    @Test
    fun duplicateConversationRowsFailClosedBeforeUiMetadataCanBeUsedOrRewritten() {
        PortableConversationStore(context).use { history ->
            CanonicalConversationStore(
                history = history,
                metadataFile = metadataFile,
                legacyFile = null,
                idFactory = { CONVERSATION_ID },
            ).create(projectId = "project:original")
        }

        writeDuplicateMetadata()

        PortableConversationStore(context).use { history ->
            val reopened = CanonicalConversationStore(
                history = history,
                metadataFile = metadataFile,
                legacyFile = null,
                idFactory = { "unused" },
            )

            assertEquals(
                "Conversation UI metadata is corrupt or unsupported",
                reopened.state().loadFailure,
            )
            assertThrows(IllegalStateException::class.java) {
                reopened.select(CONVERSATION_ID)
            }
        }
    }

    private fun writeDuplicateMetadata() {
        DataOutputStream(FileOutputStream(metadataFile).buffered()).use { output ->
            output.writeUTF("ZARA-CONVERSATION-UI/1")
            output.writeBoundedString(CONVERSATION_ID)
            output.writeInt(2)

            output.writeBoundedString(CONVERSATION_ID)
            output.writeBoolean(false)
            output.writeBoundedString("project:first")
            output.writeBoundedString("remote:first")

            output.writeBoundedString(CONVERSATION_ID)
            output.writeBoolean(true)
            output.writeBoundedString("project:second")
            output.writeBoundedString("remote:second")
        }
    }

    private fun DataOutputStream.writeBoundedString(value: String) {
        val bytes = value.toByteArray(StandardCharsets.UTF_8)
        writeInt(bytes.size)
        write(bytes)
    }

    private companion object {
        const val CONVERSATION_ID = "android-duplicate-metadata-fence"
    }
}
