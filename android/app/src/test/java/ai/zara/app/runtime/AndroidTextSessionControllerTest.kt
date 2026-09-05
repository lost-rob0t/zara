package ai.zara.app.runtime

import java.util.concurrent.CompletableFuture
import org.junit.Assert.assertEquals
import org.junit.Assert.assertTrue
import org.junit.Test

class AndroidTextSessionControllerTest {

    @Test
    fun connect_commits_hello_through_runtime_reducer() {
        val client = FakeTextSessionClient()
        val controller = AndroidTextSessionController(
            initialState = RuntimeState.initial().copy(enrollment = EnrollmentReadiness.Ready),
            client = client,
        )
        val profile = ServerProfile.create("tcp://127.0.0.1:5555")

        val future = controller.connect(profile)

        assertEquals(1L, client.connectGeneration)
        assertTrue(controller.state().server is ServerConnection.Connecting)
        client.connectFuture.complete(ConnectedTextSession(1, "session-1"))
        future.get()

        val state = controller.state()
        assertEquals(ServerConnection.Connected(1), state.server)
        assertEquals("session-1", state.sessionId)
    }

    @Test
    fun successful_text_turn_updates_only_durable_conversation_state() {
        val client = FakeTextSessionClient()
        val controller = connectedController(client)

        val future = controller.submitText("hello")

        assertEquals("hello", client.lastText)
        assertEquals(1L, client.lastGeneration)
        assertEquals("session-1", client.lastSessionId)
        client.turnFuture.complete(TextTurnResult("conversation-2", "turn-1", "hi", true))
        assertEquals("hi", future.get().text)

        assertEquals("conversation-2", controller.state().selectedConversationId)
        assertEquals("session-1", controller.state().sessionId)
    }

    @Test
    fun stale_turn_completion_cannot_mutate_new_generation() {
        val client = FakeTextSessionClient()
        val controller = connectedController(client)
        val future = controller.submitText("hello")

        controller.connectionLost("network")
        assertTrue(controller.state().server is ServerConnection.Reconnecting)
        assertEquals(2L, controller.state().generation)

        client.turnFuture.complete(TextTurnResult("stale-conversation", "turn-1", "late", true))
        future.get()

        assertEquals(null, controller.state().selectedConversationId)
        assertEquals(null, controller.state().sessionId)
        assertEquals(2L, controller.state().generation)
    }

    @Test
    fun connect_failure_enters_bounded_reconnect_state() {
        val client = FakeTextSessionClient()
        val controller = AndroidTextSessionController(
            initialState = RuntimeState.initial().copy(enrollment = EnrollmentReadiness.Ready),
            client = client,
        )
        val profile = ServerProfile.create("tcp://127.0.0.1:5555")

        val future = controller.connect(profile)
        client.connectFuture.completeExceptionally(ZaraWireException("offline"))

        try {
            future.get()
        } catch (_: Exception) {
        }

        val state = controller.state()
        assertEquals(ServerConnection.Reconnecting(2, 1), state.server)
        assertEquals(null, state.sessionId)
    }

    private fun connectedController(client: FakeTextSessionClient): AndroidTextSessionController {
        val controller = AndroidTextSessionController(
            initialState = RuntimeState.initial().copy(enrollment = EnrollmentReadiness.Ready),
            client = client,
        )
        val profile = ServerProfile.create("tcp://127.0.0.1:5555")
        val connect = controller.connect(profile)
        client.connectFuture.complete(ConnectedTextSession(1, "session-1"))
        connect.get()
        return controller
    }
}

private class FakeTextSessionClient : TextSessionClient {
    var connectGeneration: Long? = null
    val connectFuture = CompletableFuture<ConnectedTextSession>()
    val turnFuture = CompletableFuture<TextTurnResult>()
    var lastGeneration: Long? = null
    var lastSessionId: String? = null
    var lastText: String? = null

    override fun connect(profile: ServerProfile, generation: Long): CompletableFuture<ConnectedTextSession> {
        connectGeneration = generation
        return connectFuture
    }

    override fun submitText(
        generation: Long,
        sessionId: String,
        conversationId: String?,
        text: String,
    ): CompletableFuture<TextTurnResult> {
        lastGeneration = generation
        lastSessionId = sessionId
        lastText = text
        return turnFuture
    }

    override fun disconnect(): CompletableFuture<Unit> = CompletableFuture.completedFuture(Unit)

    override fun close() = Unit
}
