package ai.zara.app.runtime

import java.util.concurrent.CompletableFuture
import java.util.concurrent.ExecutionException
import org.junit.Assert.assertEquals
import org.junit.Assert.assertTrue
import org.junit.Test

class AndroidTextSessionControllerTest {

    @Test
    fun connect_commits_hello_through_runtime_reducer() {
        val client = FakeTextSessionClient()
        val scheduler = FakeReconnectScheduler()
        val controller = AndroidTextSessionController(
            initialState = RuntimeState.initial().copy(enrollment = EnrollmentReadiness.Ready),
            client = client,
            reconnectScheduler = scheduler,
        )
        val profile = ServerProfile.create("tcp://127.0.0.1:5555")

        val future = controller.connect(profile)

        assertEquals(profile, controller.state().configuredProfile)
        assertEquals(1L, client.connectGenerations.single())
        assertTrue(controller.state().server is ServerConnection.Connecting)
        client.completeConnect(0, ConnectedTextSession(1, "session-1"))
        future.get()

        val state = controller.state()
        assertEquals(ServerConnection.Connected(1), state.server)
        assertEquals("session-1", state.sessionId)
        assertEquals(emptyList<Long>(), scheduler.delays)
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
    fun text_timeout_invalidates_session_and_schedules_bounded_reconnect() {
        val client = FakeTextSessionClient()
        val scheduler = FakeReconnectScheduler()
        val controller = connectedController(client, scheduler)

        val future = controller.submitText("hello")
        client.turnFuture.completeExceptionally(TextRequestTimeoutException("timed out"))
        try {
            future.get()
        } catch (_: Exception) {
        }

        assertEquals(ServerConnection.Reconnecting(2, 1), controller.state().server)
        assertEquals(2L, controller.state().generation)
        assertEquals(null, controller.state().sessionId)
        assertEquals(1, client.disconnectCalls)
        assertEquals(listOf(250L), scheduler.delays)
    }

    @Test
    fun handshake_required_turn_failure_invalidates_session_and_schedules_bounded_reconnect() {
        val client = FakeTextSessionClient()
        val scheduler = FakeReconnectScheduler()
        val controller = connectedController(client, scheduler)

        val future = controller.submitText("hello")
        client.turnFuture.completeExceptionally(
            ZaraWireException(
                "turn submit failed: handshake_required",
                code = "handshake_required",
            ),
        )
        try {
            future.get()
        } catch (_: Exception) {
        }

        assertEquals(ServerConnection.Reconnecting(2, 1), controller.state().server)
        assertEquals(2L, controller.state().generation)
        assertEquals(null, controller.state().sessionId)
        assertEquals(1, client.disconnectCalls)
        assertEquals(listOf(250L), scheduler.delays)

        scheduler.runNext()
        client.completeConnect(1, ConnectedTextSession(2, "session-2"))
        assertEquals(ServerConnection.Connected(2), controller.state().server)
        assertEquals("session-2", controller.state().sessionId)
    }

    @Test
    fun stale_session_turn_failure_invalidates_session_and_schedules_bounded_reconnect() {
        val client = FakeTextSessionClient()
        val scheduler = FakeReconnectScheduler()
        val controller = connectedController(client, scheduler)

        val future = controller.submitText("hello")
        client.turnFuture.completeExceptionally(
            ZaraWireException("turn failed: stale_session", code = "stale_session"),
        )
        try {
            future.get()
        } catch (_: Exception) {
        }

        assertEquals(ServerConnection.Reconnecting(2, 1), controller.state().server)
        assertEquals(2L, controller.state().generation)
        assertEquals(1, client.disconnectCalls)
    }

    @Test
    fun per_turn_protocol_error_does_not_invalidate_a_current_session() {
        val client = FakeTextSessionClient()
        val scheduler = FakeReconnectScheduler()
        val controller = connectedController(client, scheduler)

        val future = controller.submitText("hello")
        client.turnFuture.completeExceptionally(
            ZaraWireException("turn failed: invalid_message", code = "invalid_message"),
        )
        try {
            future.get()
        } catch (_: Exception) {
        }

        assertEquals(ServerConnection.Connected(1), controller.state().server)
        assertEquals("session-1", controller.state().sessionId)
        assertEquals(0, client.disconnectCalls)
        assertEquals(emptyList<Long>(), scheduler.delays)
    }

    @Test
    fun stale_generation_handshake_failure_cannot_invalidate_a_newer_session() {
        val client = FakeTextSessionClient()
        val scheduler = FakeReconnectScheduler()
        val controller = connectedController(client, scheduler)

        val staleFuture = controller.submitText("stale turn")
        controller.connectionLost("network drop")
        scheduler.runNext()
        client.completeConnect(1, ConnectedTextSession(2, "session-2"))
        assertEquals(ServerConnection.Connected(2), controller.state().server)

        client.turnFuture.completeExceptionally(
            ZaraWireException(
                "turn submit failed: handshake_required",
                code = "handshake_required",
            ),
        )
        try {
            staleFuture.get()
        } catch (_: Exception) {
        }

        assertEquals(ServerConnection.Connected(2), controller.state().server)
        assertEquals("session-2", controller.state().sessionId)
        assertEquals(1, client.disconnectCalls)
    }

    @Test
    fun stale_turn_completion_cannot_mutate_new_generation() {
        val client = FakeTextSessionClient()
        val scheduler = FakeReconnectScheduler()
        val controller = connectedController(client, scheduler)
        val future = controller.submitText("hello")

        controller.connectionLost("network")
        assertTrue(controller.state().server is ServerConnection.Reconnecting)
        assertEquals(2L, controller.state().generation)

        client.turnFuture.complete(TextTurnResult("stale-conversation", "turn-1", "late", true))
        val error = try {
            future.get()
            throw AssertionError("stale text turn must fail closed")
        } catch (error: ExecutionException) {
            error.cause
        }

        assertTrue(error is StaleTextSessionException)
        assertEquals(null, controller.state().selectedConversationId)
        assertEquals(null, controller.state().sessionId)
        assertEquals(2L, controller.state().generation)
    }

    @Test
    fun connection_loss_schedules_bounded_reconnect_and_commits_fresh_hello() {
        val client = FakeTextSessionClient()
        val scheduler = FakeReconnectScheduler()
        val controller = connectedController(client, scheduler)

        controller.connectionLost("network")

        assertEquals(ServerConnection.Reconnecting(2, 1), controller.state().server)
        assertEquals(listOf(250L), scheduler.delays)
        assertEquals(1, client.disconnectCalls)

        scheduler.runNext()
        assertEquals(listOf(1L, 2L), client.connectGenerations)
        client.completeConnect(1, ConnectedTextSession(2, "session-2"))

        assertEquals(ServerConnection.Connected(2), controller.state().server)
        assertEquals("session-2", controller.state().sessionId)
    }

    @Test
    fun reconnect_failure_advances_attempt_and_schedules_next_delay() {
        val client = FakeTextSessionClient()
        val scheduler = FakeReconnectScheduler()
        val controller = connectedController(client, scheduler)

        controller.connectionLost("network")
        scheduler.runNext()
        client.failConnect(1, ZaraWireException("still offline"))

        assertEquals(ServerConnection.Reconnecting(3, 2), controller.state().server)
        assertEquals(listOf(250L, 500L), scheduler.delays)

        scheduler.runNext()
        assertEquals(listOf(1L, 2L, 3L), client.connectGenerations)
    }

    @Test
    fun duplicate_connection_loss_during_reconnect_does_not_consume_retry_budget() {
        val client = FakeTextSessionClient()
        val scheduler = FakeReconnectScheduler()
        val controller = connectedController(client, scheduler)

        controller.connectionLost("network")
        repeat(8) {
            controller.connectionLost("duplicate-network-callback")
        }

        assertEquals(ServerConnection.Reconnecting(2, 1), controller.state().server)
        assertEquals(2L, controller.state().generation)
        assertEquals(listOf(250L), scheduler.delays)
        assertEquals(1, client.disconnectCalls)

        scheduler.runNext()
        assertEquals(listOf(1L, 2L), client.connectGenerations)
        client.failConnect(1, ZaraWireException("actual reconnect failed"))

        assertEquals(ServerConnection.Reconnecting(3, 2), controller.state().server)
        assertEquals(listOf(250L, 500L), scheduler.delays)
        assertEquals(2, client.disconnectCalls)
    }

    @Test
    fun enrollment_loss_disconnects_current_session_without_reconnect() {
        val client = FakeTextSessionClient()
        val scheduler = FakeReconnectScheduler()
        val controller = connectedController(client, scheduler)

        controller.observeEnrollment(EnrollmentReadiness.Unenrolled)

        assertEquals(EnrollmentReadiness.Unenrolled, controller.state().enrollment)
        assertEquals(ServerConnection.Disconnected, controller.state().server)
        assertEquals(2L, controller.state().generation)
        assertEquals(null, controller.state().sessionId)
        assertEquals(1, client.disconnectCalls)
        assertEquals(emptyList<Long>(), scheduler.delays)
    }

    @Test
    fun changing_server_trust_invalidates_the_session_and_allows_a_fresh_connect() {
        val client = FakeTextSessionClient()
        val scheduler = FakeReconnectScheduler()
        val controller = connectedController(client, scheduler)
        val profile = controller.state().configuredProfile!!

        controller.serverTrustChanged()

        assertEquals(ServerConnection.Disconnected, controller.state().server)
        assertEquals(EnrollmentReadiness.Ready, controller.state().enrollment)
        assertEquals(null, controller.state().sessionId)
        assertEquals(2L, controller.state().generation)
        assertEquals(1, client.disconnectCalls)
        val fresh = controller.connect(profile)
        assertEquals(3L, client.connectGenerations.last())
        client.completeConnect(1, ConnectedTextSession(3, "new-session"))
        fresh.get()
        assertEquals("new-session", controller.state().sessionId)
    }

    @Test
    fun changing_server_trust_cancels_a_queued_reconnect() {
        val client = FakeTextSessionClient()
        val scheduler = FakeReconnectScheduler()
        val controller = connectedController(client, scheduler)
        controller.connectionLost("network")
        assertEquals(ServerConnection.Reconnecting(2, 1), controller.state().server)

        controller.serverTrustChanged()
        scheduler.runNext()

        assertEquals(ServerConnection.Disconnected, controller.state().server)
        assertEquals(3L, controller.state().generation)
        assertEquals(listOf(1L), client.connectGenerations)
    }

    @Test
    fun assistant_role_observation_uses_runtime_reducer_without_touching_connection() {
        val client = FakeTextSessionClient()
        val controller = connectedController(client)

        controller.observeAssistantRole(RoleOutcome.HELD)

        assertEquals(AssistantRole.Held, controller.state().assistantRole)
        assertEquals(ServerConnection.Connected(1), controller.state().server)
        assertEquals("session-1", controller.state().sessionId)
    }

    @Test
    fun connect_failure_enters_bounded_reconnect_state_and_schedules_retry() {
        val client = FakeTextSessionClient()
        val scheduler = FakeReconnectScheduler()
        val controller = AndroidTextSessionController(
            initialState = RuntimeState.initial().copy(enrollment = EnrollmentReadiness.Ready),
            client = client,
            reconnectScheduler = scheduler,
        )
        val profile = ServerProfile.create("tcp://127.0.0.1:5555")

        val future = controller.connect(profile)
        client.failConnect(0, ZaraWireException("offline"))

        try {
            future.get()
        } catch (_: Exception) {
        }

        val state = controller.state()
        assertEquals(ServerConnection.Reconnecting(2, 1), state.server)
        assertEquals(null, state.sessionId)
        assertEquals(listOf(250L), scheduler.delays)
    }

    private fun connectedController(
        client: FakeTextSessionClient,
        scheduler: FakeReconnectScheduler = FakeReconnectScheduler(),
    ): AndroidTextSessionController {
        val controller = AndroidTextSessionController(
            initialState = RuntimeState.initial().copy(enrollment = EnrollmentReadiness.Ready),
            client = client,
            reconnectScheduler = scheduler,
        )
        val profile = ServerProfile.create("tcp://127.0.0.1:5555")
        val connect = controller.connect(profile)
        client.completeConnect(0, ConnectedTextSession(1, "session-1"))
        connect.get()
        return controller
    }
}

private class FakeTextSessionClient : TextSessionClient {
    val connectGenerations = mutableListOf<Long>()
    private val connectFutures = mutableListOf<CompletableFuture<ConnectedTextSession>>()
    val turnFuture = CompletableFuture<TextTurnResult>()
    var disconnectCalls = 0
    var lastGeneration: Long? = null
    var lastSessionId: String? = null
    var lastText: String? = null

    override fun connect(profile: ServerProfile, generation: Long): CompletableFuture<ConnectedTextSession> {
        connectGenerations += generation
        return CompletableFuture<ConnectedTextSession>().also(connectFutures::add)
    }

    fun completeConnect(index: Int, session: ConnectedTextSession) {
        connectFutures[index].complete(session)
    }

    fun failConnect(index: Int, error: Throwable) {
        connectFutures[index].completeExceptionally(error)
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

    override fun disconnect(): CompletableFuture<Unit> {
        disconnectCalls += 1
        return CompletableFuture.completedFuture(Unit)
    }

    override fun close() = Unit
}

private class FakeReconnectScheduler : ReconnectScheduler {
    val delays = mutableListOf<Long>()
    private val tasks = ArrayDeque<() -> Unit>()

    override fun schedule(delayMillis: Long, task: () -> Unit) {
        delays += delayMillis
        tasks.addLast(task)
    }

    fun runNext() {
        tasks.removeFirst().invoke()
    }

    override fun close() = Unit
}
