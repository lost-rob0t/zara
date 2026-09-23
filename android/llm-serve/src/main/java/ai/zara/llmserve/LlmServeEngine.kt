package ai.zara.llmserve

import ai.zara.app.localai.LiteRtLocalLlmBackend
import ai.zara.app.localai.LocalAiPhase
import ai.zara.app.localai.LocalAiRuntime
import ai.zara.app.localai.LocalAiState
import ai.zara.app.localai.LocalGenerationRequest
import ai.zara.app.localai.LocalGenerationResult
import ai.zara.app.localai.LocalModelFormat
import ai.zara.app.localai.LocalModelMetadata
import ai.zara.app.localai.LocalModelSpec
import ai.zara.app.localai.LocalModelStore
import android.content.Context
import java.io.File
import java.io.InputStream
import java.util.concurrent.TimeUnit

class LlmServeEngine(context: Context) : AutoCloseable {
    private val modelStore = LocalModelStore(File(context.filesDir, "zara/models"))
    private val runtime = LocalAiRuntime(LiteRtLocalLlmBackend(context))

    fun loadActiveModel(): LocalAiState {
        val active = modelStore.activeModel() ?: return runtime.state()
        return runtime.load(active).get(LOAD_TIMEOUT_SECONDS, TimeUnit.SECONDS)
    }

    fun models(): List<LocalModelSpec> = modelStore.installedModels()

    fun activeModel(): LocalModelSpec? = modelStore.activeModel()

    fun state(): LocalAiState = runtime.state()

    fun install(source: InputStream, metadata: LocalModelMetadata): LocalModelSpec {
        require(metadata.format == LocalModelFormat.LITERT_LM) {
            "LLM Serve currently accepts only litert-lm models"
        }
        check(runtime.state().phase != LocalAiPhase.GENERATING) {
            "Cannot replace the active model during generation"
        }
        val previous = modelStore.activeModel()
        val spec = modelStore.install(source, metadata)
        try {
            runtime.load(spec).get(LOAD_TIMEOUT_SECONDS, TimeUnit.SECONDS)
            return spec
        } catch (error: Throwable) {
            if (previous == null) {
                modelStore.clear()
            } else {
                modelStore.activate(previous)
                runCatching {
                    runtime.load(previous).get(LOAD_TIMEOUT_SECONDS, TimeUnit.SECONDS)
                }
            }
            throw error
        }
    }

    fun selectModel(name: String?): LocalModelSpec {
        val requested = name?.trim().orEmpty()
        val active = modelStore.activeModel()
        if (requested.isEmpty() && active != null) {
            ensureLoaded(active)
            return active
        }

        val candidate = models().firstOrNull { spec ->
            requested == spec.id ||
                requested == spec.id + ":" + spec.version ||
                requested == spec.id + "@" + spec.version
        } ?: throw IllegalArgumentException(
            "Unknown local model: " + if (requested.isEmpty()) "<none>" else requested
        )

        ensureLoaded(candidate)
        modelStore.activate(candidate)
        return candidate
    }

    fun generate(
        model: String?,
        prompt: String,
        maxOutputTokens: Int,
        onChunk: (String) -> Unit = {},
    ): LocalGenerationResult {
        val spec = selectModel(model)
        check(runtime.state().phase == LocalAiPhase.READY) { "Local model is not ready" }
        val result = runtime.generate(
            LocalGenerationRequest(prompt = prompt, maxOutputTokens = maxOutputTokens),
            onChunk,
        ).get(GENERATION_TIMEOUT_SECONDS, TimeUnit.SECONDS)
        check(result.modelId == spec.id && result.modelVersion == spec.version) {
            "Local model generation identity changed"
        }
        return result
    }

    fun cancel() {
        runtime.cancel().get(CANCEL_TIMEOUT_SECONDS, TimeUnit.SECONDS)
    }

    private fun ensureLoaded(spec: LocalModelSpec) {
        val current = runtime.state()
        if (current.phase == LocalAiPhase.READY && current.model == spec) return
        runtime.load(spec).get(LOAD_TIMEOUT_SECONDS, TimeUnit.SECONDS)
    }

    override fun close() {
        runtime.close()
    }

    companion object {
        private const val LOAD_TIMEOUT_SECONDS = 180L
        private const val GENERATION_TIMEOUT_SECONDS = 300L
        private const val CANCEL_TIMEOUT_SECONDS = 5L
    }
}
