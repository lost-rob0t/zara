package ai.zara.app.conversations

import java.io.File
import java.nio.file.Files
import org.junit.Assert.assertEquals
import org.junit.Assert.assertFalse
import org.junit.Assert.assertNotEquals
import org.junit.Assert.assertNotNull
import org.junit.Assert.assertNull
import org.junit.Assert.assertThrows
import org.junit.Assert.assertTrue
import org.junit.Test

class ConversationStoreTest {
    @Test fun `create select and restart preserve multiple chats`() {
        val root = Files.createTempDirectory("zara-conversations").toFile()
        val file = File(root, "conversations.bin")
        val ids = ArrayDeque(listOf("chat-a", "chat-b"))
        var now = 100L
        val store = ConversationStore(file, { ids.removeFirst() }, { now++ })

        val first = store.create()
        val second = store.create()
        store.select(first.id)

        val restored = ConversationStore(file).state()
        assertEquals(listOf("chat-a", "chat-b"), restored.conversations.map { it.id })
        assertEquals(first.id, restored.selectedConversationId)
        assertEquals(first, restored.selectedConversation)
        assertNull(restored.loadFailure)
        assertNotEquals(first.localConversationId, second.localConversationId)
    }

    @Test fun `first turn derives title and completed turn survives restart`() {
        val root = Files.createTempDirectory("zara-conversation-turn").toFile()
        val file = File(root, "conversations.bin")
        val store = ConversationStore(file, { "chat-a" }, { 100L })
        val chat = store.create()

        store.beginTurn(chat.id, "  Explain the actor model with a practical example  ")
        var active = store.state().selectedConversation!!
        assertEquals(ConversationStatus.Running, active.status)
        assertTrue(active.title.startsWith("Explain the actor model"))
        assertNull(active.turns.single().assistantText)

        store.completeTurn(
            chat.id,
            assistantText = "Actors own state and communicate through messages.",
            success = true,
            remoteConversationId = "remote-42",
        )

        active = ConversationStore(file).state().selectedConversation!!
        assertEquals(ConversationStatus.Success, active.status)
        assertEquals("remote-42", active.remoteConversationId)
        assertEquals(1, active.turns.size)
        assertEquals("Actors own state and communicate through messages.", active.turns.single().assistantText)
        assertEquals(true, active.turns.single().success)
    }

    @Test fun `pin rename and project move persist without changing local identity`() {
        val root = Files.createTempDirectory("zara-conversation-metadata").toFile()
        val file = File(root, "conversations.bin")
        val store = ConversationStore(file, { "chat-a" }, { 100L })
        val chat = store.create()
        val localId = chat.localConversationId

        store.rename(chat.id, "Runtime debugging")
        store.setPinned(chat.id, true)
        store.moveToProject(chat.id, "project-a")

        val restored = ConversationStore(file).state().selectedConversation!!
        assertEquals("Runtime debugging", restored.title)
        assertTrue(restored.pinned)
        assertEquals("project-a", restored.projectId)
        assertEquals(localId, restored.localConversationId)

        store.moveToProject(chat.id, null)
        assertNull(store.state().selectedConversation!!.projectId)
        assertEquals(localId, store.state().selectedConversation!!.localConversationId)
    }

    @Test fun `pinned chats are excluded from recents and ordered by activity`() {
        val root = Files.createTempDirectory("zara-conversation-order").toFile()
        val ids = ArrayDeque(listOf("a", "b", "c"))
        var now = 10L
        val store = ConversationStore(File(root, "conversations.bin"), { ids.removeFirst() }, { now++ })
        val a = store.create()
        val b = store.create()
        val c = store.create()

        store.setPinned(b.id, true)
        store.beginTurn(a.id, "touch a")
        store.completeTurn(a.id, "ok", true)

        val state = store.state()
        assertEquals(listOf("b"), state.pinnedConversations.map { it.id })
        assertEquals(listOf("a", "c"), state.recentConversations.map { it.id })
        assertFalse(state.recentConversations.any { it.pinned })
    }

    @Test fun `failed turn keeps transcript and failed status`() {
        val root = Files.createTempDirectory("zara-conversation-failed").toFile()
        val store = ConversationStore(File(root, "conversations.bin"), { "chat-a" }, { 100L })
        val chat = store.create()
        store.beginTurn(chat.id, "test")
        store.failTurn(chat.id, "Remote request failed")

        val restored = store.state().selectedConversation!!
        assertEquals(ConversationStatus.Failed, restored.status)
        assertEquals(false, restored.turns.single().success)
        assertEquals("Remote request failed", restored.turns.single().assistantText)
    }

    @Test fun `running turn recovery is persisted after restart`() {
        val root = Files.createTempDirectory("zara-conversation-interrupted").toFile()
        val file = File(root, "conversations.bin")
        val store = ConversationStore(file, { "chat-a" }, { 100L })
        val chat = store.create()
        store.beginTurn(chat.id, "work that never completed")
        val runningBytes = file.readBytes()

        val restored = ConversationStore(file).state().selectedConversation!!
        assertEquals(ConversationStatus.Interrupted, restored.status)
        assertEquals(false, restored.turns.single().success)
        assertNotNull(restored.turns.single().assistantText)

        val recoveredBytes = file.readBytes()
        assertFalse(runningBytes.contentEquals(recoveredBytes))

        val restoredAgain = ConversationStore(file).state().selectedConversation!!
        assertEquals(ConversationStatus.Interrupted, restoredAgain.status)
        assertEquals(false, restoredAgain.turns.single().success)
        assertTrue(recoveredBytes.contentEquals(file.readBytes()))
    }

    @Test fun `remote identity is optional bounded and preserved across local completions`() {
        val root = Files.createTempDirectory("zara-conversation-remote").toFile()
        val store = ConversationStore(File(root, "conversations.bin"), { "chat-a" }, { 100L })
        val chat = store.create()

        store.beginTurn(chat.id, "first")
        store.completeTurn(chat.id, "remote", true, "remote-a")
        store.beginTurn(chat.id, "second")
        store.completeTurn(chat.id, "local", true, null)

        assertEquals("remote-a", store.state().selectedConversation!!.remoteConversationId)
        assertThrows(IllegalArgumentException::class.java) {
            store.beginTurn(chat.id, "third")
            store.completeTurn(chat.id, "bad", true, "r".repeat(257))
        }
    }

    @Test fun `invalid metadata and overlapping turns fail without rewriting chat`() {
        val root = Files.createTempDirectory("zara-conversation-invalid").toFile()
        val store = ConversationStore(File(root, "conversations.bin"), { "chat-a" }, { 100L })
        val chat = store.create()

        assertThrows(IllegalArgumentException::class.java) { store.rename(chat.id, "   ") }
        assertThrows(IllegalArgumentException::class.java) { store.rename(chat.id, "x".repeat(121)) }
        assertThrows(IllegalArgumentException::class.java) { store.moveToProject(chat.id, "p".repeat(129)) }
        assertThrows(IllegalArgumentException::class.java) { store.select("missing") }

        store.beginTurn(chat.id, "one")
        assertThrows(IllegalStateException::class.java) { store.beginTurn(chat.id, "two") }
        assertEquals(1, store.state().selectedConversation!!.turns.size)
    }

    @Test fun `corrupt store degrades explicitly and preserves bytes`() {
        val root = Files.createTempDirectory("zara-conversation-corrupt").toFile()
        val file = File(root, "conversations.bin")
        val original = "not a Zara conversation store"
        file.writeText(original)

        val store = ConversationStore(file, { "chat-a" }, { 100L })
        val state = store.state()
        assertTrue(state.conversations.isEmpty())
        assertNull(state.selectedConversationId)
        assertNotNull(state.loadFailure)
        assertThrows(IllegalStateException::class.java) { store.create() }
        assertEquals(original, file.readText())
    }
}
