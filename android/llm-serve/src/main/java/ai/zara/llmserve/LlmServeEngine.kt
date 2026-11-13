package ai.zara.llmserve

import ai.zara.app.localai.LocalAiPhase
import ai.zara.app.localai.LocalAiRemoteClient
import ai.zara.app.localai.LocalAiState
import ai.zara.app.localai.LocalGenerationRequest
import ai.zara.app.localai.LocalGenerationResult
import ai.zara.app.localai.LocalModelFormat
import ai.zara.app.localai.LocalModelMetadata
import ai.zara.app.localai.LocalModelSpec
import android.content.Context
import android.net.Uri
import java.util.concurrent.TimeUnit

class LlmServeEngine(context: Context) : AutoCloseable {
    private val remote = LocalAiRemoteClient(context)

    fun loadActiveModel(): LocalAiState {
        val active = activeModel() ?: return state()
        return remote.selectModel(active.id, active.version)
            .get(LOAD_TIMEOUT_SECONDS, TimeUnit.SECONDS)
    }

    fun models(): List<LocalModelSpec> =
        remote.models().get(QUERY_TIMEOUT_SECONDS, TimeUnit.SECONDS)

    fun activeModel(): LocalModelSpec? =
        remote.activeModel().get(QUERY_TIMEOUT_SECONDS, TimeUnit.SECONDS)

    fun state(): LocalAiState =
        remote.state().get(QUERY_TIMEOUT_SECONDS, TimeUnit.SECONDS)

    fun install(uri: Uri, metadata: LocalModelMetadata): LocalModelSpec {
        require(metadata.format == LocalModelFormat.LITERT_LM) {
            "LLM Serve currently accepts only litert-lm models"
        }
        return remote.installModel(uri, metadata)
            .get(LOAD_TIMEOUT_SECONDS, TimeUnit.SECONDS)
    }

    fun selectModel(name: String?): LocalModelSpec {
        val requested = name?.trim().orEmpty()
        val active = activeModel()
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
        return candidate
    }

    fun generate(
        model: String?,
        prompt: String,
        maxOutputTokens: Int,
        onChunk: (String) -> Unit = {},
    ): LocalGenerationResult {
        val spec = selectModel(model)
        val current = state()
        check(current.phase == LocalAiPhase.READY && current.model?.id == spec.id && current.model?.version == spec.version) {
            "Local model is not ready"
        }
        val result = remote.generate(
            LocalGenerationRequest(prompt = prompt, maxOutputTokens = maxOutputTokens),
            onChunk,
        ).get(GENERATION_TIMEOUT_SECONDS, TimeUnit.SECONDS)
        check(result.modelId == spec.id && result.modelVersion == spec.version) {
            "Local model generation identity changed"
        }
        return result
    }

    fun cancel() {
        remote.cancelGeneration().get(CANCEL_TIMEOUT_SECONDS, TimeUnit.SECONDS)
    }

    private fun ensureLoaded(spec: LocalModelSpec) {
        val current = state()
        if (
            current.phase == LocalAiPhase.READY &&
            current.model?.id == spec.id &&
            current.model?.version == spec.version
        ) {
            return
        }
        val selected = remote.selectModel(spec.id, spec.version)
            .get(LOAD_TIMEOUT_SECONDS, TimeUnit.SECONDS)
        check(
            selected.phase == LocalAiPhase.READY &&
                selected.model?.id == spec.id &&
                selected.model?.version == spec.version
        ) {
            "Canonical local runtime did not activate the selected model"
        }
    }

    override fun close() {
        remote.close()
    }

    companion object {
        private const val QUERY_TIMEOUT_SECONDS = 10L
        private const val LOAD_TIMEOUT_SECONDS = 180L
        private const val GENERATION_TIMEOUT_SECONDS = 300L
        private const val CANCEL_TIMEOUT_SECONDS = 5L
    }
}
