package ai.zara.app.runtime

import java.util.concurrent.CompletableFuture
import java.util.concurrent.ExecutionException
import org.junit.Assert.assertEquals
import org.junit.Assert.assertTrue
import org.junit.Test

class AndroidTextSessionRestartTest {
    @Test
    fun `late turn from dead daemon session fails after replacement hello`() {
        val client = RestartTextSessionClient()
        val scheduler = RestartScheduler()
        val controller = AndroidTextSessionController(
            initialState = RuntimeState.initial().copy(enrollment = EnrollmentReadiness.Ready),
            client = client,
            reconnectScheduler = scheduler,
        )
        val profile = ServerProfile.create("tcp://127.0.0.1:5555")
        val firstConnect = controller.connect(profile)
        client.completeConnect(0, ConnectedTextSession(1, "session-1"))
        firstConnect.get()

        val staleTurn = controller.submitText("old daemon turn")
        controller.connectionLost("daemon restarted")
        scheduler.runNext()
        client.completeConnect(1, ConnectedTextSession(2, "session-2"))

        assertEquals(ServerConnection.Connected(2), controller.state().server)
        assertEquals("session-2", controller.state().sessionId)

        client.turnFuture.complete(TextTurnResult("stale-conversation", "turn-old", "late success", true))

        val error = try {
            staleTurn.get()
            throw AssertionError("late response from stale authenticated session must fail closed")
        } catch (error: ExecutionException) {
            error.cause
        }
        assertTrue(error is StaleTextSessionException)
        assertEquals(null, controller.state().selectedConversationId)
        assertEquals("session-2", controller.state().sessionId)
    }
}

private class RestartTextSessionClient : TextSessionClient {
    private val connectFutures = mutableListOf<CompletableFuture<ConnectedTextSession>>()
    val turnFuture = CompletableFuture<TextTurnResult>()

    override fun connect(profile: ServerProfile, generation: Long): CompletableFuture<ConnectedTextSession> =
        CompletableFuture<ConnectedTextSession>().also(connectFutures::add)

    fun completeConnect(index: Int, session: ConnectedTextSession) {
        connectFutures[index].complete(session)
    }

    override fun submitText(
        generation: Long,
        sessionId: String,
        conversationId: String?,
        text: String,
    ): CompletableFuture<TextTurnResult> = turnFuture

    override fun disconnect(): CompletableFuture<Unit> = CompletableFuture.completedFuture(Unit)

    override fun close() = Unit
}

private class RestartScheduler : ReconnectScheduler {
    private val tasks = ArrayDeque<() -> Unit>()

    override fun schedule(delayMillis: Long, task: () -> Unit) {
        tasks.addLast(task)
    }

    fun runNext() {
        tasks.removeFirst().invoke()
    }

    override fun close() = Unit
}
