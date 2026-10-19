package ai.zara.app.conversations

import ai.zara.app.diagnostics.ChatErrorDetails
import ai.zara.app.diagnostics.LocalRuntimeDiagnostics
import java.io.File
import java.nio.file.Files
import org.junit.Assert.*
import org.junit.Test

class ConversationErrorDetailsTest {
    @Test fun exactFailureSurvivesLaterSuccessNavigationResetAndReload() {
        val file = File(Files.createTempDirectory("zara-error-details").toFile(), "conversations.bin")
        val ids = ArrayDeque(listOf("first", "second"))
        val store = ConversationStore(file, { ids.removeFirst() })
        val first = store.create()
        store.beginTurn(first.id, "hello")
        val details = ChatErrorDetails.capture(version="test", versionCode=6, sourceSha="sha", runtimeMode="local",
            conversationId=first.localConversationId, turnId="failed-turn", code="local.symbolic.failed",
            error=IllegalStateException("symbolic failure"))
        store.completeTurn(first.id, "Failed", false, errorDetails=details)
        val global = LocalRuntimeDiagnostics(File(file.parentFile, "diagnostics.log"))
        global.record("local_symbolic.failed", error=IllegalStateException("symbolic failure"))
        global.clear()
        store.beginTurn(first.id, "later")
        store.completeTurn(first.id, "Success", true)
        store.create()
        store.select(first.id)
        val restored = ConversationStore(file).state().selectedConversation!!
        assertEquals(details, restored.turns.first().errorDetails)
        assertNull(restored.turns.last().errorDetails)
        assertFalse(details.contains("hello"))
        assertFalse(details.contains("later"))
    }

    @Test fun invalidDetailsNeverPartiallyCompleteTheTurn() {
        val store = ConversationStore(File(Files.createTempDirectory("zara-error-invalid").toFile(), "chats"))
        val chat = store.create()
        store.beginTurn(chat.id, "hello")
        assertThrows(IllegalArgumentException::class.java) {
            store.completeTurn(chat.id, "ok", true, errorDetails="should not be attached")
        }
        assertThrows(IllegalArgumentException::class.java) {
            store.completeTurn(chat.id, "fail", false, errorDetails="é".repeat(5000))
        }
        assertNull(store.state().selectedConversation!!.turns.single().success)
    }
    @Test fun legacyVersionOneChatsLoadWithoutInventedDetailsAndUpgradeOnWrite() {
        val file = File(Files.createTempDirectory("zara-legacy-errors").toFile(), "chats")
        java.io.DataOutputStream(file.outputStream()).use { out ->
            fun text(value: String) { val bytes=value.toByteArray(Charsets.UTF_8); out.writeInt(bytes.size); out.write(bytes) }
            out.writeUTF("ZARA-CONVERSATIONS/1")
            text("legacy"); out.writeInt(1)
            text("legacy"); text("Old chat"); out.writeBoolean(false); text(""); text("")
            text("Failed"); out.writeLong(1); out.writeLong(1); out.writeInt(1)
            text("hello"); out.writeBoolean(true); text("Old failure"); out.writeByte(0)
        }
        val store = ConversationStore(file)
        assertNull(store.state().loadFailure)
        assertNull(store.state().selectedConversation!!.turns.single().errorDetails)
        store.beginTurn("legacy", "retry")
        store.completeTurn("legacy", "failed", false, errorDetails="new retained error")
        val turns = ConversationStore(file).state().selectedConversation!!.turns
        assertNull(turns.first().errorDetails)
        assertEquals("new retained error", turns.last().errorDetails)
    }

}
