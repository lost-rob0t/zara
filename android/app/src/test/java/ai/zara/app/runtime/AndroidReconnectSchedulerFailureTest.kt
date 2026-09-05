package ai.zara.app.runtime

import java.util.concurrent.CompletableFuture
import org.junit.Assert.assertEquals
import org.junit.Test

class AndroidReconnectSchedulerFailureTest {
    @Test
    fun `scheduler rejection fails closed instead of leaving zombie reconnecting state`() {
        val client = SchedulerFailureClient()
        val controller = AndroidTextSessionController(
            initialState = RuntimeState.initial().copy(enrollment = EnrollmentReadiness.Ready),
            client = client,
            reconnectScheduler = RejectingReconnectScheduler(),
        )
        val profile = ServerProfile.create("tcp://127.0.0.1:5555")
        val connect = controller.connect(profile)
        client.connectFuture.complete(ConnectedTextSession(1, "session-1"))
        connect.get()

        controller.connectionLost("network")

        assertEquals(
            ServerConnection.OfflineDegraded(1, "reconnect scheduler unavailable"),
            controller.state().server,
        )
        assertEquals(null, controller.state().sessionId)
        assertEquals(1, client.disconnectCalls)
    }
}

private class SchedulerFailureClient : TextSessionClient {
    val connectFuture = CompletableFuture<ConnectedTextSession>()
    var disconnectCalls = 0

    override fun connect(
        profile: ServerProfile,
        generation: Long,
    ): CompletableFuture<ConnectedTextSession> = connectFuture

    override fun submitText(
        generation: Long,
        sessionId: String,
        conversationId: String?,
        text: String,
    ): CompletableFuture<TextTurnResult> =
        CompletableFuture.failedFuture(UnsupportedOperationException("unused"))

    override fun disconnect(): CompletableFuture<Unit> {
        disconnectCalls += 1
        return CompletableFuture.completedFuture(Unit)
    }

    override fun close() = Unit
}

private class RejectingReconnectScheduler : ReconnectScheduler {
    override fun schedule(delayMillis: Long, task: () -> Unit) {
        throw IllegalStateException("scheduler rejected work")
    }

    override fun close() = Unit
}
