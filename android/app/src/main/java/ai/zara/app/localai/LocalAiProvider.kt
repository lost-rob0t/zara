package ai.zara.app.localai

import java.io.InputStream
import java.util.concurrent.CompletableFuture

class EmbeddedLocalAiProvider(
    private val client: LocalAiServiceClient,
) : LocalAiProvider {
    override val capabilities = LocalAiProviderCapabilities(
        id = ID,
        displayName = "Embedded Local",
        offlineOnly = true,
        streaming = true,
        modelFormats = setOf(LocalModelFormat.LITERT_LM),
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
