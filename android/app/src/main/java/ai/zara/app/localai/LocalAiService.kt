package ai.zara.app.localai

import android.app.Service
import android.content.Intent
import android.os.Binder
import android.os.IBinder
import java.io.File
import java.io.InputStream
import java.util.concurrent.CompletableFuture
import java.util.concurrent.ExecutorService
import java.util.concurrent.Executors

class LocalAiService : Service() {
    private val binder = LocalBinder()
    private val modelIo: ExecutorService = Executors.newSingleThreadExecutor { runnable ->
        Thread(runnable, "zara-local-model-store").apply { isDaemon = true }
    }

    private lateinit var modelStore: LocalModelStore
    private lateinit var runtime: LocalAiRuntime
    private lateinit var ttsRegistry: LocalTtsProviderRegistry

    @Volatile
    private lateinit var tts: LocalTtsProvider

    @Volatile
    private var loading: CompletableFuture<LocalAiState>? = null

    override fun onCreate() {
        super.onCreate()
        modelStore = LocalModelStore(File(filesDir, "zara/models"))
        runtime = LocalAiRuntime(LiteRtLocalLlmBackend(this))
        ttsRegistry = LocalTtsProviderRegistry(
            listOf(
                AndroidOfflineTtsBackend(this),
            )
        )
        tts = ttsRegistry.default()
        tts.initialize()
        loadActiveModel()
    }

    override fun onBind(intent: Intent?): IBinder = binder

    override fun onDestroy() {
        loading?.cancel(true)
        runtime.close()
        ttsRegistry.close()
        modelIo.shutdownNow()
        super.onDestroy()
    }

    inner class LocalBinder : Binder() {
        fun state(): LocalAiState = runtime.state()

        fun models(): CompletableFuture<List<LocalModelSpec>> = this@LocalAiService.models()

        fun activeModel(): CompletableFuture<LocalModelSpec?> = this@LocalAiService.activeModel()

        fun ttsState(): LocalTtsState = tts.state()

        fun ttsProviders(): List<LocalTtsProviderCapabilities> = ttsRegistry.capabilities()

        fun selectTtsProvider(id: String): CompletableFuture<LocalTtsState> =
            this@LocalAiService.selectTtsProvider(id)

        fun loadActiveModel(): CompletableFuture<LocalAiState> = this@LocalAiService.loadActiveModel()

        fun installModel(
            source: InputStream,
            metadata: LocalModelMetadata,
        ): CompletableFuture<LocalModelSpec> = this@LocalAiService.installModel(source, metadata)

        fun selectModel(
            id: String,
            version: String,
        ): CompletableFuture<LocalAiState> = this@LocalAiService.selectModel(id, version)

        fun generate(
            request: LocalGenerationRequest,
            onChunk: (String) -> Unit = {},
        ): CompletableFuture<LocalGenerationResult> =
            this@LocalAiService.generate(request, onChunk)

        fun cancelGeneration(): CompletableFuture<LocalAiState> = runtime.cancel()

        fun unloadModel(): CompletableFuture<LocalAiState> = runtime.unload()

        fun speak(text: String): CompletableFuture<Unit> = tts.speak(text)

        fun stopSpeech() = tts.stop()
    }

    private fun models(): CompletableFuture<List<LocalModelSpec>> =
        CompletableFuture.supplyAsync(modelStore::installedModels, modelIo)

    private fun activeModel(): CompletableFuture<LocalModelSpec?> =
        CompletableFuture.supplyAsync(modelStore::activeModel, modelIo)

    @Synchronized
    private fun selectTtsProvider(id: String): CompletableFuture<LocalTtsState> {
        val selected = ttsRegistry.provider(id)
        if (selected === tts) return selected.initialize()
        tts.stop()
        tts = selected
        return selected.initialize()
    }

    @Synchronized
    private fun loadActiveModel(): CompletableFuture<LocalAiState> {
        if (runtime.state().phase == LocalAiPhase.READY) {
            return CompletableFuture.completedFuture(runtime.state())
        }
        loading?.let { return it }
        val future = CompletableFuture.supplyAsync(
            {
                modelStore.activeModel()
                    ?.also(::requireEmbeddedFormat)
                    ?: throw LocalAiUnavailableException("No verified local model is installed")
            },
            modelIo,
        ).thenCompose(runtime::load)
        loading = future
        future.whenComplete { _, _ ->
            synchronized(this) {
                if (loading === future) loading = null
            }
        }
        return future
    }

    private fun installModel(
        source: InputStream,
        metadata: LocalModelMetadata,
    ): CompletableFuture<LocalModelSpec> {
        require(metadata.format == LocalModelFormat.LITERT_LM) {
            "Embedded provider accepts only ${LocalModelFormat.LITERT_LM.wireName} models"
        }
        return CompletableFuture.supplyAsync(
            { source.use { modelStore.install(it, metadata) } },
            modelIo,
        ).thenCompose { spec ->
            runtime.load(spec).thenApply { spec }
        }
    }

    private fun selectModel(
        id: String,
        version: String,
    ): CompletableFuture<LocalAiState> =
        CompletableFuture.supplyAsync(
            {
                modelStore.model(id, version)
                    ?.also(::requireEmbeddedFormat)
                    ?: throw LocalAiUnavailableException("Local model is not installed: $id@$version")
            },
            modelIo,
        ).thenCompose { spec ->
            runtime.load(spec).thenCompose { state ->
                CompletableFuture.supplyAsync(
                    {
                        modelStore.activate(spec)
                        state
                    },
                    modelIo,
                )
            }
        }

    private fun generate(
        request: LocalGenerationRequest,
        onChunk: (String) -> Unit,
    ): CompletableFuture<LocalGenerationResult> =
        loadActiveModel().thenCompose {
            runtime.generate(request, onChunk)
        }

    private fun requireEmbeddedFormat(spec: LocalModelSpec) {
        require(spec.format == LocalModelFormat.LITERT_LM) {
            "Embedded provider cannot execute ${spec.format.wireName} models"
        }
    }
}

class LocalAiUnavailableException(message: String) : IllegalStateException(message)
