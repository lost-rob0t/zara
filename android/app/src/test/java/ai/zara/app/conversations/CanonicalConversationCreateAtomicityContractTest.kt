package ai.zara.app.conversations

import java.io.File
import org.junit.Assert.assertFalse
import org.junit.Assert.assertTrue
import org.junit.Test

class CanonicalConversationCreateAtomicityContractTest {
    @Test
    fun `metadata failure compensates through canonical history owner`() {
        val source = File(
            "src/main/java/ai/zara/app/conversations/CanonicalConversationStore.kt"
        ).readText()
        val create = source
            .substringAfter("fun create(projectId: String? = null): ConversationRecord {")
            .substringBefore("@Synchronized\n    fun select(")

        assertTrue(create.contains("val previousMetadata = metadata"))
        assertTrue(create.contains("metadata = previousMetadata"))
        assertTrue(create.contains("history.rollbackEmptyConversationCreation(id)"))
        assertTrue(create.contains("error.addSuppressed(rollbackError)"))
        assertTrue(create.indexOf("persistMetadata()") < create.indexOf("rollbackEmptyConversationCreation"))

        assertFalse(
            "UI facade must compensate through the canonical history owner, not mutate history SQL directly",
            source.contains("desktop_conversations"),
        )
    }

    @Test
    fun `rollback primitive refuses observable state and verifies the delete postcondition`() {
        val rollback = File(
            "src/main/java/ai/zara/app/history/ConversationCreateRollback.kt"
        ).readText()

        assertTrue(rollback.contains("loadMessages(conversationId).isEmpty()"))
        assertTrue(rollback.contains("loadSymbolicProjection(conversationId) == null"))
        assertTrue(rollback.contains("ConversationHistoryContract.localPrincipalId"))
        assertTrue(rollback.contains("writableDatabase.delete("))
        assertTrue(rollback.contains("\"desktop_conversations\""))
        assertTrue(rollback.contains("getConversation(conversationId) == null"))
        assertTrue(
            rollback.indexOf("writableDatabase.delete(") <
                rollback.indexOf("getConversation(conversationId) == null")
        )
    }
}
