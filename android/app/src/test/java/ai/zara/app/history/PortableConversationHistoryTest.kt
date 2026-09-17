package ai.zara.app.history

import java.io.File
import org.junit.Assert.assertEquals
import org.junit.Assert.assertTrue
import org.junit.Test

class PortableConversationHistoryTest {
    @Test
    fun `title derivation matches desktop sixty code point policy`() {
        assertEquals("hello world", ConversationHistoryContract.deriveTitle("  hello   world  "))
        assertEquals("x".repeat(60), ConversationHistoryContract.deriveTitle("x".repeat(60)))
        assertEquals("x".repeat(57) + "…", ConversationHistoryContract.deriveTitle("x".repeat(61)))
        assertEquals("🙂".repeat(57) + "…", ConversationHistoryContract.deriveTitle("🙂".repeat(61)))
    }

    @Test
    fun `android packages the canonical desktop sqlite schema`() {
        val schema = File("../../zara/conversation_schema.sql").readText()
        val gradle = File("build.gradle.kts").readText()
        assertTrue(schema.contains("CREATE TABLE IF NOT EXISTS desktop_conversations"))
        assertTrue(schema.contains("CREATE TABLE IF NOT EXISTS desktop_messages"))
        assertTrue(schema.contains("principal_id TEXT NOT NULL DEFAULT 'local:owner'"))
        assertTrue(gradle.contains("../../zara/conversation_schema.sql"))
        assertTrue(gradle.contains("database/conversation_schema.sql"))
        assertEquals("local:owner", ConversationHistoryContract.localPrincipalId)
        assertEquals(2, ConversationHistoryContract.schemaVersion)
    }

    @Test
    fun `local session writes history and ui exposes recents`() {
        val session = File("src/main/java/ai/zara/app/AndroidAppSession.kt").readText()
        val activity = File("src/main/java/ai/zara/app/MainActivity.kt").readText()
        val ui = File("src/main/java/ai/zara/app/ui/ZaraApp.kt").readText()

        assertTrue(session.contains("private val localHistory = PortableConversationStore(context)"))
        assertTrue(session.contains("localHistory.saveMessage("))
        assertTrue(session.contains("conversationId = conversation.id"))
        assertTrue(session.contains("fun selectLocalConversation("))
        assertTrue(session.contains("fun newLocalConversation()"))
        assertTrue(activity.contains("refreshLocalHistory()"))
        assertTrue(activity.contains("onSelectLocalConversation ="))
        assertTrue(ui.contains("localConversations.take(8)"))
        assertTrue(ui.contains("local history is empty"))
        assertTrue(ui.contains("localConversation?.messages.orEmpty()"))
        assertTrue(!ui.contains("local history is not enabled yet"))
    }

    @Test
    fun `android legacy owner migration is numeric uid only`() {
        val store = File("src/main/java/ai/zara/app/history/PortableConversationStore.kt").readText()

        assertTrue(store.contains("substr(principal_id, 1, 4) = 'uid:'"))
        assertTrue(store.contains("length(substr(principal_id, 5)) > 0"))
        assertTrue(store.contains("substr(principal_id, 5) NOT GLOB '*[^0-9]*'"))
        assertTrue(!store.contains("principal_id LIKE 'uid:%'"))
    }
}
