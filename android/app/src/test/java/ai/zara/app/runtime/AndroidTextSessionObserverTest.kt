package ai.zara.app.runtime

import java.util.concurrent.CompletableFuture
import org.junit.Assert.assertEquals
import org.junit.Assert.assertTrue
import org.junit.Test

class AndroidTextSessionObserverTest {
    @Test
    fun observerReceivesConnectReconnectAndFreshSessionReducerStates() {
        val client = ObserverTextSessionClient()
        val scheduler = ObserverReconnectScheduler()
        val controller = AndroidTextSessionController(
            initialState = RuntimeState.initial().copy(enrollment = EnrollmentReadiness.Ready),
            client = client,
            reconnectScheduler = scheduler,
        )
        val observed = mutableListOf<RuntimeState>()
        controller.setStateObserver(observed::add)

        val profile = ServerProfile.create("tcp://127.0.0.1:5555")
        controller.connect(profile)
        assertTrue(observed.last().server is ServerConnection.Connecting)

        client.completeConnect(0, ConnectedTextSession(1, "session-1"))
        assertEquals(ServerConnection.Connected(1), observed.last().server)
        assertEquals("session-1", observed.last().sessionId)

        controller.connectionLost("network")
        assertEquals(ServerConnection.Reconnecting(2, 1), observed.last().server)
        assertEquals(null, observed.last().sessionId)

        scheduler.runNext()
        client.completeConnect(1, ConnectedTextSession(2, "session-2"))
        assertEquals(ServerConnection.Connected(2), observed.last().server)
        assertEquals("session-2", observed.last().sessionId)
    }

    @Test
    fun observerReceivesConversationContinuationAfterRealTurnResult() {
        val client = ObserverTextSessionClient()
        val controller = AndroidTextSessionController(
            initialState = RuntimeState.initial().copy(enrollment = EnrollmentReadiness.Ready),
            client = client,
            reconnectScheduler = ObserverReconnectScheduler(),
        )
        val observed = mutableListOf<RuntimeState>()
        controller.setStateObserver(observed::add)
        val connect = controller.connect(ServerProfile.create("tcp://127.0.0.1:5555"))
        client.completeConnect(0, ConnectedTextSession(1, "session-1"))
        connect.get()

        controller.submitText("hello")
        client.turnFuture.complete(TextTurnResult("conversation-7", "turn-1", "hi", true))

        assertEquals("conversation-7", observed.last().selectedConversationId)
    }
}

private class ObserverTextSessionClient : TextSessionClient {
    private val connectFutures = mutableListOf<CompletableFuture<ConnectedTextSession>>()
    val turnFuture = CompletableFuture<TextTurnResult>()

    override fun connect(
        profile: ServerProfile,
        generation: Long,
    ): CompletableFuture<ConnectedTextSession> =
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

private class ObserverReconnectScheduler : ReconnectScheduler {
    private val tasks = ArrayDeque<() -> Unit>()

    override fun schedule(delayMillis: Long, task: () -> Unit) {
        tasks.addLast(task)
    }

    fun runNext() {
        tasks.removeFirst().invoke()
    }

    override fun close() = Unit
}
