package ai.zara.app.runtime

import java.util.concurrent.CompletableFuture
import java.util.concurrent.Executors
import java.util.concurrent.ScheduledExecutorService
import java.util.concurrent.TimeUnit

interface TextSessionClient : AutoCloseable {
    fun connect(profile: ServerProfile, generation: Long): CompletableFuture<ConnectedTextSession>

    fun submitText(
        generation: Long,
        sessionId: String,
        conversationId: String?,
        text: String,
    ): CompletableFuture<TextTurnResult>

    fun disconnect(): CompletableFuture<Unit>

    override fun close()
}

interface ReconnectScheduler : AutoCloseable {
    fun schedule(delayMillis: Long, task: () -> Unit)
    override fun close()
}

class ScheduledReconnectScheduler(
    private val executor: ScheduledExecutorService = Executors.newSingleThreadScheduledExecutor { runnable ->
        Thread(runnable, "zara-android-reconnect").apply { isDaemon = true }
    },
) : ReconnectScheduler {
    override fun schedule(delayMillis: Long, task: () -> Unit) {
        require(delayMillis >= 0) { "reconnect delay must be non-negative" }
        executor.schedule(task, delayMillis, TimeUnit.MILLISECONDS)
    }

    override fun close() {
        executor.shutdownNow()
    }
}

class AndroidTextSessionController(
    initialState: RuntimeState,
    private val client: TextSessionClient,
    private val reconnectScheduler: ReconnectScheduler = ScheduledReconnectScheduler(),
) : AutoCloseable {
    private val lock = Any()
    private var runtimeState = initialState
    private var closed = false

    fun state(): RuntimeState = synchronized(lock) { runtimeState }

    fun connect(profile: ServerProfile): CompletableFuture<ConnectedTextSession> {
        val generation = synchronized(lock) {
            check(!closed) { "Android text session controller is closed" }
            check(runtimeState.enrollment == EnrollmentReadiness.Ready) {
                "Android enrollment must be ready before connecting"
            }
            runtimeState = reduce(runtimeState, RuntimeEvent.ServerConfigured(profile))
            runtimeState = reduce(runtimeState, RuntimeEvent.ConnectRequested)
            val connecting = runtimeState.server as? ServerConnection.Connecting
                ?: throw IllegalStateException("connection is already active")
            connecting.generation
        }
        return connectGeneration(profile, generation)
    }

    fun submitText(text: String): CompletableFuture<TextTurnResult> {
        require(text.isNotBlank()) { "text turn must not be blank" }
        val request = synchronized(lock) {
            check(!closed) { "Android text session controller is closed" }
            val connected = runtimeState.server as? ServerConnection.Connected
                ?: throw IllegalStateException("Android text client is not connected")
            val sessionId = runtimeState.sessionId
                ?: throw IllegalStateException("connected state requires a session id")
            TurnRequest(
                generation = connected.generation,
                sessionId = sessionId,
                conversationId = runtimeState.selectedConversationId,
            )
        }

        val future = client.submitText(
            generation = request.generation,
            sessionId = request.sessionId,
            conversationId = request.conversationId,
            text = text,
        )
        future.whenComplete { result, error ->
            if (error != null || result == null) return@whenComplete
            synchronized(lock) {
                val connected = runtimeState.server as? ServerConnection.Connected
                if (
                    !closed &&
                    connected?.generation == request.generation &&
                    runtimeState.sessionId == request.sessionId
                ) {
                    runtimeState = runtimeState.copy(
                        selectedConversationId = result.conversationId
                            ?: runtimeState.selectedConversationId,
                    )
                }
            }
        }
        return future
    }

    fun connectionLost(reason: String) {
        require(reason.isNotBlank()) { "connection loss reason is required" }
        val shouldReconnect = synchronized(lock) {
            if (closed) return
            val previousGeneration = runtimeState.generation
            runtimeState = reduce(
                runtimeState,
                RuntimeEvent.ConnectionLost(previousGeneration, reason),
            )
            runtimeState.server is ServerConnection.Reconnecting &&
                runtimeState.generation != previousGeneration
        }
        if (shouldReconnect) {
            client.disconnect()
            scheduleReconnect()
        }
    }

    fun observeEnrollment(readiness: EnrollmentReadiness) {
        val shouldDisconnect = synchronized(lock) {
            if (closed) return
            val wasActive = runtimeState.server !is ServerConnection.Disconnected
            runtimeState = reduce(runtimeState, RuntimeEvent.EnrollmentObserved(readiness))
            wasActive && readiness != EnrollmentReadiness.Ready &&
                runtimeState.server is ServerConnection.Disconnected
        }
        if (shouldDisconnect) {
            client.disconnect()
        }
    }

    override fun close() {
        synchronized(lock) {
            if (closed) return
            closed = true
        }
        reconnectScheduler.close()
        client.close()
    }

    private fun connectGeneration(
        profile: ServerProfile,
        generation: Long,
    ): CompletableFuture<ConnectedTextSession> {
        val future = client.connect(profile, generation)
        future.whenComplete { session, error ->
            var scheduleNext = false
            synchronized(lock) {
                if (closed || runtimeState.generation != generation) return@whenComplete
                if (error == null && session != null) {
                    runtimeState = reduce(
                        runtimeState,
                        RuntimeEvent.HelloAccepted(session.generation, session.sessionId),
                    )
                } else {
                    runtimeState = reduce(
                        runtimeState,
                        RuntimeEvent.ConnectionFailed(
                            generation,
                            error?.message ?: "connection failed",
                        ),
                    )
                    scheduleNext = runtimeState.server is ServerConnection.Reconnecting
                }
            }
            if (scheduleNext) {
                client.disconnect()
                scheduleReconnect()
            }
        }
        return future
    }

    private fun scheduleReconnect() {
        val reconnect = synchronized(lock) {
            if (closed) return
            val connection = runtimeState.server as? ServerConnection.Reconnecting ?: return
            val profile = runtimeState.configuredProfile ?: return
            ReconnectRequest(
                profile = profile,
                generation = connection.generation,
                delayMillis = reconnectDelayMillis(connection.attempt),
            )
        }
        reconnectScheduler.schedule(reconnect.delayMillis) {
            val stillCurrent = synchronized(lock) {
                !closed &&
                    runtimeState.generation == reconnect.generation &&
                    runtimeState.server is ServerConnection.Reconnecting
            }
            if (stillCurrent) {
                connectGeneration(reconnect.profile, reconnect.generation)
            }
        }
    }

    private data class TurnRequest(
        val generation: Long,
        val sessionId: String,
        val conversationId: String?,
    )

    private data class ReconnectRequest(
        val profile: ServerProfile,
        val generation: Long,
        val delayMillis: Long,
    )
}