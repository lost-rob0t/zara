package ai.zara.app.localai

import android.os.Bundle

object LocalAiRemoteProtocol {
    const val ACTION_BIND = "ai.zara.app.localai.BIND_REMOTE"
    const val HOST_PACKAGE = "ai.zara.app"
    const val HOST_SERVICE = "ai.zara.app.localai.LocalAiService"
    const val PERMISSION = "ai.zara.app.permission.LOCAL_AI"

    const val MSG_STATE = 1
    const val MSG_MODELS = 2
    const val MSG_ACTIVE_MODEL = 3
    const val MSG_INSTALL_MODEL = 4
    const val MSG_SELECT_MODEL = 5
    const val MSG_GENERATE = 6
    const val MSG_CANCEL = 7
    const val MSG_UNLOAD = 8

    const val EVENT_CHUNK = 100
    const val RESULT_OK = 200
    const val RESULT_ERROR = 201

    const val KEY_REQUEST_ID = "request_id"
    const val KEY_MODEL = "model"
    const val KEY_MODELS = "models"
    const val KEY_MODEL_FD = "model_fd"
    const val KEY_METADATA = "metadata"
    const val KEY_STATE = "state"
    const val KEY_RESULT = "result"
    const val KEY_CHUNK = "chunk"
    const val KEY_ID = "id"
    const val KEY_VERSION = "version"
    const val KEY_PROMPT = "prompt"
    const val KEY_MAX_OUTPUT_TOKENS = "max_output_tokens"
    const val KEY_ERROR_TYPE = "error_type"
    const val KEY_ERROR_MESSAGE = "error_message"

    private const val KEY_QUANTIZATION = "quantization"
    private const val KEY_SHA256 = "sha256"
    private const val KEY_PATH = "path"
    private const val KEY_MAX_CONTEXT_TOKENS = "max_context_tokens"
    private const val KEY_BACKEND = "backend"
    private const val KEY_FORMAT = "format"
    private const val KEY_PHASE = "phase"
    private const val KEY_GENERATION = "generation"
    private const val KEY_FAILURE = "failure"
    private const val KEY_TEXT = "text"
    private const val KEY_MODEL_ID = "model_id"
    private const val KEY_MODEL_VERSION = "model_version"

    fun metadataToBundle(metadata: LocalModelMetadata): Bundle = Bundle().apply {
        putString(KEY_ID, metadata.id)
        putString(KEY_VERSION, metadata.version)
        putString(KEY_QUANTIZATION, metadata.quantization.wireName)
        putString(KEY_SHA256, metadata.sha256)
        putInt(KEY_MAX_CONTEXT_TOKENS, metadata.maxContextTokens)
        putString(KEY_BACKEND, metadata.backend.name)
        putString(KEY_FORMAT, metadata.format.wireName)
    }

    fun metadataFromBundle(bundle: Bundle): LocalModelMetadata = LocalModelMetadata(
        id = requireString(bundle, KEY_ID),
        version = requireString(bundle, KEY_VERSION),
        quantization = LocalModelQuantization.requireKnown(requireString(bundle, KEY_QUANTIZATION)),
        sha256 = requireString(bundle, KEY_SHA256),
        maxContextTokens = bundle.getInt(KEY_MAX_CONTEXT_TOKENS),
        backend = runCatching {
            LocalModelBackend.valueOf(requireString(bundle, KEY_BACKEND))
        }.getOrElse { throw IllegalArgumentException("Local model backend is invalid") },
        format = LocalModelFormat.requireKnown(requireString(bundle, KEY_FORMAT)),
    )

    fun modelToBundle(spec: LocalModelSpec): Bundle = metadataToBundle(spec.metadata()).apply {
        putString(KEY_PATH, spec.path)
    }

    fun modelFromBundle(bundle: Bundle): LocalModelSpec {
        val metadata = metadataFromBundle(bundle)
        return LocalModelSpec(
            id = metadata.id,
            version = metadata.version,
            quantization = metadata.quantization,
            sha256 = metadata.sha256,
            path = requireString(bundle, KEY_PATH),
            maxContextTokens = metadata.maxContextTokens,
            backend = metadata.backend,
            format = metadata.format,
        )
    }

    fun modelsToBundle(models: List<LocalModelSpec>): Bundle = Bundle().apply {
        putParcelableArrayList(KEY_MODELS, ArrayList(models.map(::modelToBundle)))
    }

    @Suppress("DEPRECATION")
    fun modelsFromBundle(bundle: Bundle): List<LocalModelSpec> =
        bundle.getParcelableArrayList<Bundle>(KEY_MODELS)
            ?.map(::modelFromBundle)
            ?: emptyList()

    fun stateToBundle(state: LocalAiState): Bundle = Bundle().apply {
        putString(KEY_PHASE, state.phase.name)
        putLong(KEY_GENERATION, state.generation)
        putBundle(KEY_MODEL, state.model?.let(::modelToBundle))
        putString(KEY_FAILURE, state.failure)
    }

    fun stateFromBundle(bundle: Bundle): LocalAiState = LocalAiState(
        phase = runCatching { LocalAiPhase.valueOf(requireString(bundle, KEY_PHASE)) }
            .getOrElse { throw IllegalArgumentException("Local AI phase is invalid") },
        generation = bundle.getLong(KEY_GENERATION),
        model = bundle.getBundle(KEY_MODEL)?.let(::modelFromBundle),
        failure = bundle.getString(KEY_FAILURE),
    )

    fun generationResultToBundle(result: LocalGenerationResult): Bundle = Bundle().apply {
        putString(KEY_TEXT, result.text)
        putString(KEY_MODEL_ID, result.modelId)
        putString(KEY_MODEL_VERSION, result.modelVersion)
        putString(KEY_QUANTIZATION, result.quantization.wireName)
        putLong(KEY_GENERATION, result.generation)
    }

    fun generationResultFromBundle(bundle: Bundle): LocalGenerationResult = LocalGenerationResult(
        text = requireString(bundle, KEY_TEXT),
        modelId = requireString(bundle, KEY_MODEL_ID),
        modelVersion = requireString(bundle, KEY_MODEL_VERSION),
        quantization = LocalModelQuantization.requireKnown(requireString(bundle, KEY_QUANTIZATION)),
        generation = bundle.getLong(KEY_GENERATION),
    )

    fun errorToBundle(requestId: Long, error: Throwable): Bundle = Bundle().apply {
        putLong(KEY_REQUEST_ID, requestId)
        putString(KEY_ERROR_TYPE, error::class.java.simpleName.take(96))
        putString(KEY_ERROR_MESSAGE, (error.message ?: error::class.java.simpleName).take(512))
    }

    private fun requireString(bundle: Bundle, key: String): String =
        bundle.getString(key)?.takeIf(String::isNotBlank)
            ?: throw IllegalArgumentException("Missing local AI IPC field: $key")
}
