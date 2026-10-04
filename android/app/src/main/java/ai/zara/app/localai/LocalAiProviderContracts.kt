package ai.zara.app.localai

import java.io.InputStream
import java.util.concurrent.CompletableFuture

/**
 * First-class on-device language-model provider boundary.
 *
 * The provider API is intentionally model-agnostic: callers select a verified model from the
 * provider catalog and do not depend on the concrete inference runtime. Speech is a separate
 * provider family so an LLM backend never has to pretend it owns TTS, and vice versa.
 */
data class LocalAiProviderCapabilities(
    val id: String,
    val displayName: String,
    val offlineOnly: Boolean,
    val streaming: Boolean,
    val modelFormats: Set<LocalModelFormat>,
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
}

