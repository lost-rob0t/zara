package ai.zara.app.runtime

import java.util.concurrent.CompletableFuture
import org.junit.Assert.assertEquals
import org.junit.Assert.assertNull
import org.junit.Test

class AndroidProjectConversationRoutingTest {
    @Test fun `explicit null project conversation starts fresh instead of reusing global selection`() {
        val client = RecordingProjectTextClient()
        val controller = AndroidTextSessionController(connectedState("global-conversation"), client, NoopProjectReconnectScheduler())
        client.nextResult = TextTurnResult("project-conversation", "turn-1", "ok", true)

        val result = controller.submitText("project hello", null).get()

        assertNull(client.lastConversationId)
        assertEquals("project-conversation", result.conversationId)
        assertEquals("project-conversation", controller.state().selectedConversationId)
        controller.close()
    }

    @Test fun `explicit project conversation is routed without depending on selected global conversation`() {
        val client = RecordingProjectTextClient()
        val controller = AndroidTextSessionController(connectedState("other-conversation"), client, NoopProjectReconnectScheduler())
        client.nextResult = TextTurnResult("project-a", "turn-2", "ok", true)

        controller.submitText("continue", "project-a").get()

        assertEquals("project-a", client.lastConversationId)
        controller.close()
    }

    @Test fun `legacy unscoped submission still uses selected conversation`() {
        val client = RecordingProjectTextClient()
        val controller = AndroidTextSessionController(connectedState("selected-conversation"), client, NoopProjectReconnectScheduler())
        client.nextResult = TextTurnResult("selected-conversation", "turn-3", "ok", true)

        controller.submitText("continue globally").get()

        assertEquals("selected-conversation", client.lastConversationId)
        controller.close()
    }

    private fun connectedState(selectedConversationId: String): RuntimeState = RuntimeState(
        server = ServerConnection.Connected(3),
        assistantRole = AssistantRole.NotYetAssessed,
        enrollment = EnrollmentReadiness.Ready,
        generation = 3,
        sessionId = "session-3",
        selectedConversationId = selectedConversationId,
    )
}

private class RecordingProjectTextClient : TextSessionClient {
    var lastConversationId: String? = null
    var nextResult = TextTurnResult(null, "turn", "ok", true)

    override fun connect(
        profile: ServerProfile,
        generation: Long,
    ): CompletableFuture<ConnectedTextSession> = CompletableFuture.failedFuture(
        UnsupportedOperationException("not used"),
    )

    override fun submitText(
        generation: Long,
        sessionId: String,
        conversationId: String?,
        text: String,
    ): CompletableFuture<TextTurnResult> {
        lastConversationId = conversationId
        return CompletableFuture.completedFuture(nextResult)
    }

    override fun disconnect(): CompletableFuture<Unit> = CompletableFuture.completedFuture(Unit)

    override fun close() = Unit
}

private class NoopProjectReconnectScheduler : ReconnectScheduler {
    override fun schedule(delayMillis: Long, task: () -> Unit) = Unit
    override fun close() = Unit
}
