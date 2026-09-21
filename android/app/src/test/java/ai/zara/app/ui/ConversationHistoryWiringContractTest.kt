package ai.zara.app.ui

import java.io.File
import org.junit.Assert.assertFalse
import org.junit.Assert.assertTrue
import org.junit.Test

class ConversationHistoryWiringContractTest {
    @Test fun `drawer exposes real new chat pinned and recent history`() {
        val source = File("src/main/java/ai/zara/app/ui/ZaraApp.kt").readText()

        assertTrue(source.contains("\"＋  New chat\""))
        assertTrue(source.contains("conversationState.pinnedConversations"))
        assertTrue(source.contains("conversationState.recentConversations"))
        assertFalse(source.contains("\"local history is not enabled yet\""))
        assertFalse(source.contains("\"nothing is synced implicitly\""))
    }

    @Test fun `chat rows expose pin rename and project move actions`() {
        val source = File("src/main/java/ai/zara/app/ui/ZaraApp.kt").readText()

        assertTrue(source.contains("onTogglePinned(conversation.id, !conversation.pinned)"))
        assertTrue(source.contains("onRenameConversation(conversation.id, renameDraft.trim())"))
        assertTrue(source.contains("onMoveConversationToProject(conversation.id, project.id)"))
        assertTrue(source.contains("onMoveConversationToProject(conversation.id, null)"))
    }

    @Test fun `history rows surface persisted lifecycle status`() {
        val source = File("src/main/java/ai/zara/app/ui/ZaraApp.kt").readText()

        assertTrue(source.contains("ConversationStatus.Empty -> tokens.border"))
        assertTrue(source.contains("ConversationStatus.Running -> tokens.accentCyan"))
        assertTrue(source.contains("ConversationStatus.Success -> tokens.success"))
        assertTrue(source.contains("ConversationStatus.Failed -> tokens.error"))
        assertTrue(source.contains("ConversationStatus.Interrupted -> tokens.warning"))
    }

    @Test fun `active chat renders every persisted turn and running state`() {
        val source = File("src/main/java/ai/zara/app/ui/ZaraApp.kt").readText()

        assertTrue(source.contains("conversation.turns.forEachIndexed"))
        assertTrue(source.contains("UserMessage(turn.userText)"))
        assertTrue(source.contains("AssistantMessage(turn.assistantText, turn.success == true)"))
        assertTrue(source.contains("AssistantPendingMessage()"))
        assertTrue(source.contains("onSendText(message, conversation, project)"))
    }

    @Test fun `host gives each chat its own local and remote runtime identity`() {
        val session = File("src/main/java/ai/zara/app/AndroidAppSession.kt").readText()
        val host = File("src/main/java/ai/zara/app/MainActivity.kt").readText()

        assertTrue(session.contains("localConversationId: String = \"local-device\""))
        assertTrue(session.contains("submitRemoteText(text, remoteConversationId)"))
        assertTrue(host.contains("localConversationId = conversation.localConversationId"))
        assertTrue(host.contains("remoteConversationId = conversation.remoteConversationId"))
    }

    @Test fun `host persists lifecycle mutations through canonical portable history facade`() {
        val source = File("src/main/java/ai/zara/app/MainActivity.kt").readText()
        val facade = File(
            "src/main/java/ai/zara/app/conversations/CanonicalConversationStore.kt"
        ).readText()

        assertTrue(source.contains("PortableConversationStore(this)"))
        assertTrue(source.contains("CanonicalConversationStore("))
        assertFalse(source.contains("ConversationStore(File(filesDir, \"conversations.bin\"))"))
        assertTrue(source.contains("conversationStore.create()"))
        assertTrue(source.contains("conversationStore.select(conversationId)"))
        assertTrue(source.contains("conversationStore.setPinned(conversationId, pinned)"))
        assertTrue(source.contains("conversationStore.rename(conversationId, title)"))
        assertTrue(source.contains("conversationStore.moveToProject(conversationId, projectId)"))
        assertTrue(source.contains("conversationStore.beginTurn(conversationId, text)"))
        assertTrue(source.contains("conversationStore.completeTurn("))
        assertTrue(source.contains("conversationStore.failTurn("))
        assertTrue(source.contains("expectedTurnId = expectedTurnId"))
        assertTrue(facade.contains("history.saveMessage("))
        assertTrue(facade.contains("history.loadState(conversation.id)"))
        assertFalse(facade.contains("assistantText" + " = DataOutput"))
    }
}
