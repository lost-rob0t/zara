package ai.zara.app.runtime

import java.util.concurrent.CompletableFuture
import java.util.concurrent.ExecutionException
import org.junit.Assert.assertEquals
import org.junit.Assert.assertTrue
import org.junit.Test

class StrictLocalRemoteSuspensionTest {
    @Test
    fun `strict local suspension disconnects authenticated remote without losing profile`() {
        val client = SuspensionFakeTextSessionClient()
        val scheduler = SuspensionFakeReconnectScheduler()
        val controller = connectedController(client, scheduler)
        val profile = controller.state().configuredProfile

        controller.suspendRemoteForLocalMode()

        assertEquals(ServerConnection.Disconnected, controller.state().server)
        assertEquals(2L, controller.state().generation)
        assertEquals(null, controller.state().sessionId)
        assertEquals(profile, controller.state().configuredProfile)
        assertEquals(1, client.disconnectCalls)
        assertEquals(emptyList<Long>(), scheduler.delays)
    }

    @Test
    fun `strict local suspension fences a queued remote reconnect`() {
        val client = SuspensionFakeTextSessionClient()
        val scheduler = SuspensionFakeReconnectScheduler()
        val controller = connectedController(client, scheduler)

        controller.connectionLost("network")
        assertEquals(ServerConnection.Reconnecting(2, 1), controller.state().server)
        assertEquals(listOf(250L), scheduler.delays)

        controller.suspendRemoteForLocalMode()
        scheduler.runNext()

        assertEquals(ServerConnection.Disconnected, controller.state().server)
        assertEquals(3L, controller.state().generation)
        assertEquals(listOf(1L), client.connectGenerations)
        assertEquals(2, client.disconnectCalls)
    }

    @Test
    fun `strict local suspension rejects a stale remote turn completion`() {
        val client = SuspensionFakeTextSessionClient()
        val controller = connectedController(client, SuspensionFakeReconnectScheduler())
        val turn = controller.submitText("hello")

        controller.suspendRemoteForLocalMode()
        client.turnFuture.complete(TextTurnResult("remote-chat", "turn-1", "late", true))

        val failure = try {
            turn.get()
            throw AssertionError("remote completion must be rejected after strict Local suspension")
        } catch (error: ExecutionException) {
            error.cause
        }
        assertTrue(failure is StaleTextSessionException)
        assertEquals(null, controller.state().selectedConversationId)
    }

    @Test
    fun `strict local suspension rejects a stale remote connect completion`() {
        val client = SuspensionFakeTextSessionClient()
        val controller = AndroidTextSessionController(
            initialState = RuntimeState.initial().copy(enrollment = EnrollmentReadiness.Ready),
            client = client,
            reconnectScheduler = SuspensionFakeReconnectScheduler(),
        )
        val connect = controller.connect(ServerProfile.create("tcp://127.0.0.1:5555"))

        controller.suspendRemoteForLocalMode()
        client.completeConnect(0, ConnectedTextSession(1, "session-1"))

        val failure = try {
            connect.get()
            throw AssertionError("remote connect completion must be rejected after strict Local suspension")
        } catch (error: ExecutionException) {
            error.cause
        }
        assertTrue(failure is StaleTextSessionException)
        assertEquals(ServerConnection.Disconnected, controller.state().server)
        assertEquals(2L, controller.state().generation)
        assertEquals(null, controller.state().sessionId)
        assertEquals(1, client.disconnectCalls)
    }

    private fun connectedController(
        client: SuspensionFakeTextSessionClient,
        scheduler: SuspensionFakeReconnectScheduler,
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

private class SuspensionFakeTextSessionClient : TextSessionClient {
    val connectGenerations = mutableListOf<Long>()
    private val connectFutures = mutableListOf<CompletableFuture<ConnectedTextSession>>()
    val turnFuture = CompletableFuture<TextTurnResult>()
    var disconnectCalls = 0

    override fun connect(
        profile: ServerProfile,
        generation: Long,
    ): CompletableFuture<ConnectedTextSession> {
        connectGenerations += generation
        return CompletableFuture<ConnectedTextSession>().also(connectFutures::add)
    }

    fun completeConnect(index: Int, session: ConnectedTextSession) {
        connectFutures[index].complete(session)
    }

    override fun submitText(
        generation: Long,
        sessionId: String,
        conversationId: String?,
        text: String,
    ): CompletableFuture<TextTurnResult> = turnFuture

    override fun disconnect(): CompletableFuture<Unit> {
        disconnectCalls += 1
        return CompletableFuture.completedFuture(Unit)
    }

    override fun close() = Unit
}

private class SuspensionFakeReconnectScheduler : ReconnectScheduler {
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
