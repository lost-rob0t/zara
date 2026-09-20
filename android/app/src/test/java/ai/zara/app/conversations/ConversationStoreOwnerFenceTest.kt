package ai.zara.app.conversations

import java.io.File
import java.nio.file.Files
import org.junit.Assert.assertEquals
import org.junit.Assert.assertThrows
import org.junit.Test

class ConversationStoreOwnerFenceTest {
    @Test
    fun `activity recreation fences a late completion from the stale store owner`() {
        val root = Files.createTempDirectory("zara-conversation-owner-fence").toFile()
        val file = File(root, "conversations.bin")
        val original = ConversationStore(file, idFactory = { "chat-a" }, clock = { 100L })
        val conversation = original.create()
        original.beginTurn(conversation.id, "remember this")

        val recreated = ConversationStore(file, idFactory = { "chat-b" }, clock = { 200L })
        val recovered = checkNotNull(recreated.state().conversation(conversation.id))
        assertEquals(ConversationStatus.Interrupted, recovered.status)
        assertEquals(false, recovered.turns.single().success)
        assertEquals("Interrupted before completion.", recovered.turns.single().assistantText)

        assertThrows(IllegalStateException::class.java) {
            original.completeTurn(
                conversationId = conversation.id,
                assistantText = "late stale success",
                success = true,
            )
        }

        val reopened = ConversationStore(file, idFactory = { "chat-c" }, clock = { 300L })
        val durable = checkNotNull(reopened.state().conversation(conversation.id))
        assertEquals(ConversationStatus.Interrupted, durable.status)
        assertEquals(false, durable.turns.single().success)
        assertEquals("Interrupted before completion.", durable.turns.single().assistantText)
    }

    @Test
    fun `non recovery reopen does not revoke the active owner`() {
        val root = Files.createTempDirectory("zara-conversation-owner-observer").toFile()
        val file = File(root, "conversations.bin")
        val active = ConversationStore(file, idFactory = { "chat-a" }, clock = { 100L })
        val conversation = active.create()
        active.rename(conversation.id, "before observer")

        val observer = ConversationStore(file)
        assertEquals("before observer", observer.state().selectedConversation!!.title)

        val state = active.rename(conversation.id, "after observer")
        assertEquals("after observer", state.selectedConversation!!.title)
    }

    @Test
    fun `current store owner can still complete its own running turn`() {
        val root = Files.createTempDirectory("zara-conversation-owner-current").toFile()
        val store = ConversationStore(
            File(root, "conversations.bin"),
            idFactory = { "chat-a" },
            clock = { 100L },
        )
        val conversation = store.create()
        store.beginTurn(conversation.id, "hello")

        val state = store.completeTurn(
            conversationId = conversation.id,
            assistantText = "hi",
            success = true,
        )

        val completed = checkNotNull(state.conversation(conversation.id))
        assertEquals(ConversationStatus.Success, completed.status)
        assertEquals(true, completed.turns.single().success)
        assertEquals("hi", completed.turns.single().assistantText)
    }
}
