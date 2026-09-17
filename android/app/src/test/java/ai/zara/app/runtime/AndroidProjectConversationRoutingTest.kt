package ai.zara.app.runtime

import java.util.concurrent.CompletableFuture
import org.junit.Assert.assertEquals
import org.junit.Assert.assertNull
import org.junit.Test

class AndroidProjectConversationRoutingTest {
    @Test fun `explicit null project conversation starts fresh without replacing global selection`() {
        val client = RecordingProjectTextClient()
        val controller = AndroidTextSessionController(connectedState("global-conversation"), client, NoopProjectReconnectScheduler())
        client.nextResult = TextTurnResult("project-conversation", "turn-1", "ok", true)

        val result = controller.submitText("project hello", null).get()

        assertNull(client.conversationIds.single())
        assertEquals("project-conversation", result.conversationId)
        assertEquals("global-conversation", controller.state().selectedConversationId)
        controller.close()
    }

    @Test fun `project turn cannot contaminate later unscoped conversation routing`() {
        val client = RecordingProjectTextClient()
        val controller = AndroidTextSessionController(connectedState("global-conversation"), client, NoopProjectReconnectScheduler())
        client.nextResult = TextTurnResult("project-a", "turn-2", "project ok", true)
        controller.submitText("continue project", "project-a").get()

        client.nextResult = TextTurnResult("global-conversation", "turn-3", "global ok", true)
        controller.submitText("continue globally").get()

        assertEquals(listOf("project-a", "global-conversation"), client.conversationIds)
        assertEquals("global-conversation", controller.state().selectedConversationId)
        controller.close()
    }

    @Test fun `legacy unscoped submission adopts returned conversation`() {
        val client = RecordingProjectTextClient()
        val controller = AndroidTextSessionController(connectedState("selected-conversation"), client, NoopProjectReconnectScheduler())
        client.nextResult = TextTurnResult("next-conversation", "turn-4", "ok", true)

        controller.submitText("continue globally").get()

        assertEquals(listOf("selected-conversation"), client.conversationIds)
        assertEquals("next-conversation", controller.state().selectedConversationId)
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
    val conversationIds = mutableListOf<String?>()
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
        conversationIds += conversationId
        return CompletableFuture.completedFuture(nextResult)
    }

    override fun disconnect(): CompletableFuture<Unit> = CompletableFuture.completedFuture(Unit)

    override fun close() = Unit
}

private class NoopProjectReconnectScheduler : ReconnectScheduler {
    override fun schedule(delayMillis: Long, task: () -> Unit) = Unit
    override fun close() = Unit
}
