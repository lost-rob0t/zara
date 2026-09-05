package ai.zara.app.runtime

import java.util.concurrent.CompletableFuture

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

class AndroidTextSessionController(
    initialState: RuntimeState,
    private val client: TextSessionClient,
) : AutoCloseable {
    private val lock = Any()
    private var runtimeState = initialState

    fun state(): RuntimeState = synchronized(lock) { runtimeState }

    fun connect(profile: ServerProfile): CompletableFuture<ConnectedTextSession> {
        val generation = synchronized(lock) {
            check(runtimeState.enrollment == EnrollmentReadiness.Ready) {
                "Android enrollment must be ready before connecting"
            }
            runtimeState = reduce(runtimeState, RuntimeEvent.ConnectRequested)
            val connecting = runtimeState.server as? ServerConnection.Connecting
                ?: throw IllegalStateException("connection is already active")
            connecting.generation
        }

        val future = client.connect(profile, generation)
        future.whenComplete { session, error ->
            synchronized(lock) {
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
                }
            }
        }
        return future
    }

    fun submitText(text: String): CompletableFuture<TextTurnResult> {
        require(text.isNotBlank()) { "text turn must not be blank" }
        val request = synchronized(lock) {
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
        synchronized(lock) {
            runtimeState = reduce(
                runtimeState,
                RuntimeEvent.ConnectionLost(runtimeState.generation, reason),
            )
        }
    }

    fun observeEnrollment(readiness: EnrollmentReadiness) {
        synchronized(lock) {
            runtimeState = reduce(runtimeState, RuntimeEvent.EnrollmentObserved(readiness))
        }
    }

    override fun close() {
        client.close()
    }

    private data class TurnRequest(
        val generation: Long,
        val sessionId: String,
        val conversationId: String?,
    )
}
