package ai.zara.app.localai

import android.content.Context
import android.net.Uri
import java.io.InputStream
import java.util.concurrent.CompletableFuture

class LocalAiServiceClient(context: Context) : LocalAiProvider {
    override val capabilities = LocalAiProviderCapabilities(
        id = EmbeddedLocalAiProvider.ID,
        displayName = "Embedded Local",
        offlineOnly = true,
        streaming = true,
        modelFormats = setOf(LocalModelFormat.LITERT_LM),
        accelerators = LocalModelBackend.entries.toSet(),
    )
    private val remote = LocalAiRemoteClient(context)

    override fun state() = remote.state()
    override fun models() = remote.models()
    override fun activeModel() = remote.activeModel()
    fun loadActiveModel() = remote.loadActiveModel()
    override fun installModel(source: InputStream, metadata: LocalModelMetadata) = remote.installModel(source, metadata)
    fun installModel(uri: Uri, metadata: LocalModelMetadata) = remote.installModel(uri, metadata)
    override fun selectModel(id: String, version: String) = remote.selectModel(id, version)
    fun ttsState() = remote.ttsState()
    fun refreshSpeech() = remote.refreshSpeech()
    fun ttsProviders() = remote.ttsProviders()
    fun selectTtsProvider(id: String) = remote.selectTtsProvider(id)
    override fun generate(request: LocalGenerationRequest, onChunk: (String) -> Unit) = remote.generate(request, onChunk)
    override fun cancelGeneration() = remote.cancelGeneration()
    override fun unloadModel() = remote.unloadModel()
    fun speak(text: String): CompletableFuture<Unit> = remote.speak(text)
    fun stopSpeech() { remote.stopSpeech() }
    override fun close() = remote.close()
}
