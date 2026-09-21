package ai.zara.app.history

import java.io.File
import org.junit.Assert.assertEquals
import org.junit.Assert.assertFalse
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

        assertTrue(schema.contains("schema v4"))
        assertTrue(schema.contains("CREATE TABLE IF NOT EXISTS desktop_conversations"))
        assertTrue(schema.contains("CREATE TABLE IF NOT EXISTS desktop_messages"))
        assertTrue(schema.contains("CREATE TABLE IF NOT EXISTS desktop_symbolic_projections"))
        assertTrue(schema.contains("principal_id TEXT NOT NULL DEFAULT 'local:owner'"))
        assertTrue(schema.contains("providers_enabled INTEGER NOT NULL DEFAULT 1"))
        assertTrue(schema.contains("max_model_calls INTEGER NOT NULL DEFAULT 1"))
        assertTrue(gradle.contains("../../zara/conversation_schema.sql"))
        assertTrue(gradle.contains("into(output.resolve(\"database\"))"))
        assertTrue(gradle.contains("rename { \"conversation_schema.sql\" }"))
        assertEquals("local:owner", ConversationHistoryContract.localPrincipalId)
        assertEquals(4, ConversationHistoryContract.schemaVersion)
    }

    @Test
    fun `v4 upgrade path is additive and non destructive`() {
        val store = File("src/main/java/ai/zara/app/history/PortableConversationStore.kt").readText()

        assertTrue(store.contains("override fun onUpgrade"))
        assertTrue(store.contains("installSchema(db)"))
        assertFalse(store.contains("DROP TABLE"))
        assertFalse(store.contains("DELETE FROM desktop_conversations"))
        assertFalse(store.contains("DELETE FROM desktop_messages"))
    }

    @Test
    fun `terminal local history state rejects stale completion`() {
        assertTrue(
            ConversationHistoryContract.canPersistTransition(
                HistoryMessageStatus.Pending,
                HistoryMessageStatus.Complete,
            )
        )
        assertTrue(
            ConversationHistoryContract.canPersistTransition(
                HistoryMessageStatus.Streaming,
                HistoryMessageStatus.Cancelled,
            )
        )
        assertTrue(
            ConversationHistoryContract.canPersistTransition(
                HistoryMessageStatus.Streaming,
                HistoryMessageStatus.Streaming,
            )
        )
        assertFalse(
            ConversationHistoryContract.canPersistTransition(
                HistoryMessageStatus.Cancelled,
                HistoryMessageStatus.Cancelled,
            )
        )
        assertFalse(
            ConversationHistoryContract.canPersistTransition(
                HistoryMessageStatus.Complete,
                HistoryMessageStatus.Complete,
            )
        )
        assertFalse(
            ConversationHistoryContract.canPersistTransition(
                HistoryMessageStatus.Error,
                HistoryMessageStatus.Error,
            )
        )
        assertFalse(
            ConversationHistoryContract.canPersistTransition(
                HistoryMessageStatus.Cancelled,
                HistoryMessageStatus.Complete,
            )
        )
        assertFalse(
            ConversationHistoryContract.canPersistTransition(
                HistoryMessageStatus.Complete,
                HistoryMessageStatus.Streaming,
            )
        )
        assertFalse(
            ConversationHistoryContract.canPersistTransition(
                HistoryMessageStatus.Error,
                HistoryMessageStatus.Complete,
            )
        )
    }

    @Test
    fun `android legacy owner migration is numeric uid only`() {
        val store = File("src/main/java/ai/zara/app/history/PortableConversationStore.kt").readText()

        assertTrue(store.contains("substr(principal_id, 1, 4) = 'uid:'"))
        assertTrue(store.contains("length(substr(principal_id, 5)) > 0"))
        assertTrue(store.contains("substr(principal_id, 5) NOT GLOB '*[^0-9]*'"))
        assertTrue(store.contains("Stale terminal message update rejected"))
        assertFalse(store.contains("principal_id LIKE 'uid:%'"))
    }
}
