package ai.zara.app.localai

import java.util.concurrent.CompletableFuture

enum class LocalTtsPhase {
    STARTING,
    READY,
    SPEAKING,
    UNAVAILABLE,
    FAILED,
    STOPPED,
}

data class LocalTtsState(
    val phase: LocalTtsPhase,
    val voiceId: String? = null,
    val locale: String? = null,
    val failure: String? = null,
)

interface LocalTtsBackend : AutoCloseable {
    fun state(): LocalTtsState
    fun initialize(): CompletableFuture<LocalTtsState>
    fun speak(text: String): CompletableFuture<Unit>
    fun stop()
}
