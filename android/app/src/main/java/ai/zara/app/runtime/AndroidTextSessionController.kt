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
    private var stateObserver: ((RuntimeState) -> Unit)? = null

    fun state(): RuntimeState = synchronized(lock) { runtimeState }

    fun setStateObserver(observer: ((RuntimeState) -> Unit)?) {
        val snapshot = synchronized(lock) {
            check(!closed) { "Android text session controller is closed" }
            stateObserver = observer
            runtimeState
        }
        observer?.invoke(snapshot)
    }

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
        publishState()
        return connectGeneration(profile, generation)
    }

    fun submitText(text: String): CompletableFuture<TextTurnResult> {
        val conversationId = synchronized(lock) { runtimeState.selectedConversationId }
        return submitTextInternal(text, conversationId, adoptConversation = true)
    }

    /**
     * Submit a turn in an explicit scoped conversation without changing the controller's
     * default selected conversation. Project contexts use this path so a project reply
     * cannot contaminate ordinary chat after the user switches scopes.
     */
    fun submitText(text: String, conversationId: String?): CompletableFuture<TextTurnResult> =
        submitTextInternal(text, conversationId, adoptConversation = false)

    private fun submitTextInternal(
        text: String,
        conversationId: String?,
        adoptConversation: Boolean,
    ): CompletableFuture<TextTurnResult> {
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
                conversationId = conversationId,
                adoptConversation = adoptConversation,
            )
        }

        val clientFuture = client.submitText(
            generation = request.generation,
            sessionId = request.sessionId,
            conversationId = request.conversationId,
            text = text,
        )
        val resultFuture = CompletableFuture<TextTurnResult>()
        clientFuture.whenComplete { result, error ->
            if (error != null) {
                if (rootCause(error) is TextRequestTimeoutException && requestIsCurrent(request)) {
                    connectionLost("text request timed out")
                }
                resultFuture.completeExceptionally(error)
                return@whenComplete
            }
            if (result == null) {
                resultFuture.completeExceptionally(
                    IllegalStateException("text client completed without a result"),
                )
                return@whenComplete
            }

            var changed = false
            val accepted = synchronized(lock) {
                val connected = runtimeState.server as? ServerConnection.Connected
                if (
                    !closed &&
                    connected?.generation == request.generation &&
                    runtimeState.sessionId == request.sessionId
                ) {
                    if (request.adoptConversation) {
                        val previous = runtimeState
                        runtimeState = runtimeState.copy(
                            selectedConversationId = result.conversationId
                                ?: runtimeState.selectedConversationId,
                        )
                        changed = runtimeState != previous
                    }
                    true
                } else {
                    false
                }
            }
            if (!accepted) {
                resultFuture.completeExceptionally(
                    StaleTextSessionException(
                        "text turn completed for superseded session ${request.sessionId}",
                    ),
                )
                return@whenComplete
            }
            if (changed) publishState()
            resultFuture.complete(result)
        }
        return resultFuture
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
        publishState()
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
        publishState()
        if (shouldDisconnect) {
            client.disconnect()
        }
    }

    fun serverTrustChanged() {
        synchronized(lock) {
            check(!closed) { "Android text session controller is closed" }
            runtimeState = reduce(runtimeState, RuntimeEvent.ServerTrustChanged)
        }
        publishState()
        client.disconnect()
    }

    fun suspendRemoteForLocalMode() {
        val shouldDisconnect = synchronized(lock) {
            check(!closed) { "Android text session controller is closed" }
            val active = when (runtimeState.server) {
                is ServerConnection.Connecting,
                is ServerConnection.Connected,
                is ServerConnection.Reconnecting -> true
                is ServerConnection.Disconnected,
                is ServerConnection.OfflineDegraded -> false
            }
            if (!active) {
                false
            } else {
                runtimeState = runtimeState.copy(
                    server = ServerConnection.Disconnected,
                    generation = runtimeState.generation + 1,
                    sessionId = null,
                )
                true
            }
        }
        if (shouldDisconnect) {
            publishState()
            client.disconnect()
        }
    }

    fun observeAssistantRole(outcome: RoleOutcome) {
        synchronized(lock) {
            if (closed) return
            runtimeState = reduce(runtimeState, RuntimeEvent.RoleAssessed(outcome))
        }
        publishState()
    }

    override fun close() {
        synchronized(lock) {
            if (closed) return
            closed = true
            stateObserver = null
        }
        reconnectScheduler.close()
        client.close()
    }

    private fun connectGeneration(
        profile: ServerProfile,
        generation: Long,
    ): CompletableFuture<ConnectedTextSession> {
        val clientFuture = client.connect(profile, generation)
        val resultFuture = CompletableFuture<ConnectedTextSession>()
        clientFuture.whenComplete { session, error ->
            var scheduleNext = false
            var changed = false
            var stale = false
            var accepted = false
            synchronized(lock) {
                if (closed || runtimeState.generation != generation) {
                    stale = true
                } else {
                    val previous = runtimeState
                    if (error == null && session != null) {
                        runtimeState = reduce(
                            runtimeState,
                            RuntimeEvent.HelloAccepted(session.generation, session.sessionId),
                        )
                        accepted = runtimeState.server is ServerConnection.Connected &&
                            runtimeState.generation == generation &&
                            runtimeState.sessionId == session.sessionId
                    } else {
                        runtimeState = reduce(
                            runtimeState,
                            RuntimeEvent.ConnectionFailed(
                                generation,
                                ConnectionFailureReason.summarize(error),
                            ),
                        )
                        scheduleNext = runtimeState.server is ServerConnection.Reconnecting
                    }
                    changed = runtimeState != previous
                }
            }

            if (stale) {
                resultFuture.completeExceptionally(
                    StaleTextSessionException(
                        "connection completed for superseded generation $generation",
                    ),
                )
                return@whenComplete
            }

            if (changed) publishState()
            if (scheduleNext) {
                client.disconnect()
                scheduleReconnect()
            }

            when {
                error != null -> resultFuture.completeExceptionally(error)
                session == null -> resultFuture.completeExceptionally(
                    IllegalStateException("text client completed without a session"),
                )
                !accepted -> resultFuture.completeExceptionally(
                    StaleTextSessionException(
                        "connection result did not become current for generation $generation",
                    ),
                )
                else -> resultFuture.complete(session)
            }
        }
        return resultFuture
    }

    private fun publishState() {
        val publication = synchronized(lock) {
            if (closed) return
            val observer = stateObserver ?: return
            observer to runtimeState
        }
        publication.first.invoke(publication.second)
    }

    private fun requestIsCurrent(request: TurnRequest): Boolean = synchronized(lock) {
        val connected = runtimeState.server as? ServerConnection.Connected
        return@synchronized !closed &&
            connected?.generation == request.generation &&
            runtimeState.sessionId == request.sessionId
    }

    private fun rootCause(error: Throwable): Throwable {
        var current = error
        while (current.cause != null && current.cause !== current) {
            current = current.cause!!
        }
        return current
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
        try {
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
        } catch (_: RuntimeException) {
            val changed = synchronized(lock) {
                if (
                    closed ||
                    runtimeState.generation != reconnect.generation ||
                    runtimeState.server !is ServerConnection.Reconnecting
                ) {
                    false
                } else {
                    val previous = runtimeState
                    runtimeState = reduce(
                        runtimeState,
                        RuntimeEvent.ReconnectSchedulingFailed(reconnect.generation),
                    )
                    runtimeState != previous
                }
            }
            if (changed) publishState()
        }
    }

    private data class TurnRequest(
        val generation: Long,
        val sessionId: String,
        val conversationId: String?,
        val adoptConversation: Boolean,
    )

    private data class ReconnectRequest(
        val profile: ServerProfile,
        val generation: Long,
        val delayMillis: Long,
    )
}
