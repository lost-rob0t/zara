package ai.zara.app.localai

import java.io.InputStream
import java.util.concurrent.CompletableFuture

/**
 * First-class on-device model provider boundary.
 *
 * The provider API is intentionally model-agnostic: callers select a verified model from the
 * provider catalog and do not depend on the concrete inference runtime. The built-in provider is
 * backed by the app-private LiteRT-LM service today; additional local engines can implement this
 * same contract without changing Zara's Prolog-first routing layer.
 */
data class LocalAiProviderCapabilities(
    val id: String,
    val displayName: String,
    val offlineOnly: Boolean,
    val streaming: Boolean,
    val speech: Boolean,
    val modelContainerExtensions: Set<String>,
    val accelerators: Set<LocalModelBackend>,
)

interface LocalAiProvider : AutoCloseable {
    val capabilities: LocalAiProviderCapabilities

    fun state(): CompletableFuture<LocalAiState>

    fun models(): CompletableFuture<List<LocalModelSpec>>

    fun activeModel(): CompletableFuture<LocalModelSpec?>

    fun installModel(
        source: InputStream,
        metadata: LocalModelMetadata,
    ): CompletableFuture<LocalModelSpec>

    fun selectModel(
        id: String,
        version: String,
    ): CompletableFuture<LocalAiState>

    fun generate(
        request: LocalGenerationRequest,
        onChunk: (String) -> Unit = {},
    ): CompletableFuture<LocalGenerationResult>

    fun cancelGeneration(): CompletableFuture<LocalAiState>

    fun unloadModel(): CompletableFuture<LocalAiState>

    fun ttsState(): CompletableFuture<LocalTtsState>

    fun speak(text: String): CompletableFuture<Unit>

    fun stopSpeech()
}

class EmbeddedLocalAiProvider(
    private val client: LocalAiServiceClient,
) : LocalAiProvider {
    override val capabilities = LocalAiProviderCapabilities(
        id = ID,
        displayName = "Embedded Local",
        offlineOnly = true,
        streaming = true,
        speech = true,
        modelContainerExtensions = setOf(".litertlm"),
        accelerators = LocalModelBackend.entries.toSet(),
    )

    override fun state(): CompletableFuture<LocalAiState> = client.state()

    override fun models(): CompletableFuture<List<LocalModelSpec>> = client.models()

    override fun activeModel(): CompletableFuture<LocalModelSpec?> = client.activeModel()

    override fun installModel(
        source: InputStream,
        metadata: LocalModelMetadata,
    ): CompletableFuture<LocalModelSpec> = client.installModel(source, metadata)

    override fun selectModel(
        id: String,
        version: String,
    ): CompletableFuture<LocalAiState> = client.selectModel(id, version)

    override fun generate(
        request: LocalGenerationRequest,
        onChunk: (String) -> Unit,
    ): CompletableFuture<LocalGenerationResult> = client.generate(request, onChunk)

    override fun cancelGeneration(): CompletableFuture<LocalAiState> = client.cancelGeneration()

    override fun unloadModel(): CompletableFuture<LocalAiState> = client.unloadModel()

    override fun ttsState(): CompletableFuture<LocalTtsState> = client.ttsState()

    override fun speak(text: String): CompletableFuture<Unit> = client.speak(text)

    override fun stopSpeech() = client.stopSpeech()

    override fun close() = client.close()

    companion object {
        const val ID = "embedded"
    }
}

/** Registry used by the Android control plane instead of hard-coding a concrete local engine. */
class LocalAiProviderRegistry(
    providers: List<LocalAiProvider>,
) : AutoCloseable {
    private val ordered = providers.toList()
    private val byId = ordered.associateBy { it.capabilities.id }

    init {
        require(ordered.isNotEmpty()) { "At least one local AI provider is required" }
        require(byId.size == ordered.size) { "Local AI provider ids must be unique" }
    }

    fun capabilities(): List<LocalAiProviderCapabilities> = ordered.map { it.capabilities }

    fun provider(id: String): LocalAiProvider =
        byId[id] ?: throw IllegalArgumentException("Unknown local AI provider: $id")

    fun default(): LocalAiProvider = ordered.first()

    override fun close() {
        ordered.asReversed().forEach { provider -> runCatching { provider.close() } }
    }
}
