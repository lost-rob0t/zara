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

data class LocalTtsProviderCapabilities(
    val id: String,
    val displayName: String,
    val offlineOnly: Boolean,
    val modelBacked: Boolean,
    val modelContainerExtensions: Set<String> = emptySet(),
)

interface LocalTtsBackend : AutoCloseable {
    fun state(): LocalTtsState
    fun initialize(): CompletableFuture<LocalTtsState>
    fun speak(text: String): CompletableFuture<Unit>
    fun stop()
}

interface LocalTtsProvider : LocalTtsBackend {
    val capabilities: LocalTtsProviderCapabilities
}

class LocalTtsProviderRegistry(
    providers: List<LocalTtsProvider>,
) : AutoCloseable {
    private val ordered = providers.toList()
    private val byId = ordered.associateBy { it.capabilities.id }

    init {
        require(ordered.isNotEmpty()) { "At least one local TTS provider is required" }
        require(byId.size == ordered.size) { "Local TTS provider ids must be unique" }
    }

    fun capabilities(): List<LocalTtsProviderCapabilities> = ordered.map { it.capabilities }

    fun provider(id: String): LocalTtsProvider =
        byId[id] ?: throw IllegalArgumentException("Unknown local TTS provider: $id")

    fun default(): LocalTtsProvider = ordered.first()

    override fun close() {
        ordered.asReversed().forEach { provider -> runCatching { provider.close() } }
    }
}
