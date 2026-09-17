package ai.zara.app.localai

import java.util.Locale

enum class LocalModelBackend {
    CPU,
    GPU,
    NPU,
}

enum class LocalModelQuantization(val wireName: String) {
    DYNAMIC_INT4("dynamic-int4"),
    INT4("int4"),
    INT8("int8"),
    FP8("fp8"),
    BF16("bf16"),
    FP16("fp16");

    companion object {
        fun requireKnown(value: String): LocalModelQuantization {
            val normalized = value.trim().lowercase(Locale.ROOT)
            return entries.firstOrNull { it.wireName == normalized }
                ?: throw IllegalArgumentException("Unsupported or unknown model quantization: $value")
        }
    }
}

data class LocalModelMetadata(
    val id: String,
    val version: String,
    val quantization: LocalModelQuantization,
    val sha256: String,
    val maxContextTokens: Int,
    val backend: LocalModelBackend,
) {
    init {
        require(id.matches(Regex("[A-Za-z0-9._-]{1,96}"))) { "Model id is invalid" }
        require(version.matches(Regex("[A-Za-z0-9._+-]{1,64}"))) { "Model version is invalid" }
        require(sha256.matches(Regex("[0-9a-f]{64}"))) { "Model SHA-256 must be lowercase hexadecimal" }
        require(maxContextTokens in 128..131_072) { "Model context limit is outside the supported range" }
    }
}

data class LocalModelSpec(
    val id: String,
    val version: String,
    val quantization: LocalModelQuantization,
    val sha256: String,
    val path: String,
    val maxContextTokens: Int,
    val backend: LocalModelBackend,
) {
    init {
        LocalModelMetadata(id, version, quantization, sha256, maxContextTokens, backend)
        require(path.endsWith(".litertlm")) { "LiteRT-LM model must use the .litertlm container" }
    }

    fun metadata(): LocalModelMetadata = LocalModelMetadata(
        id = id,
        version = version,
        quantization = quantization,
        sha256 = sha256,
        maxContextTokens = maxContextTokens,
        backend = backend,
    )
}

data class LocalGenerationRequest(
    val prompt: String,
    val maxOutputTokens: Int = 256,
) {
    init {
        require(prompt.isNotBlank()) { "Local model prompt is required" }
        require(prompt.length <= 32_768) { "Local model prompt is too large" }
        require(maxOutputTokens in 1..4_096) { "Local model output limit is outside the supported range" }
    }
}

data class LocalGenerationResult(
    val text: String,
    val modelId: String,
    val modelVersion: String,
    val quantization: LocalModelQuantization,
    val generation: Long,
)

enum class LocalAiPhase {
    STOPPED,
    LOADING,
    READY,
    GENERATING,
    FAILED,
}

data class LocalAiState(
    val phase: LocalAiPhase = LocalAiPhase.STOPPED,
    val generation: Long = 0,
    val model: LocalModelSpec? = null,
    val failure: String? = null,
)

interface LocalGenerationListener {
    fun onChunk(text: String)
    fun onDone()
    fun onError(error: Throwable)
}

interface LocalGenerationSession : AutoCloseable {
    fun cancel()
}

interface LocalLlmBackend : AutoCloseable {
    fun load(spec: LocalModelSpec)

    fun generate(
        request: LocalGenerationRequest,
        listener: LocalGenerationListener,
    ): LocalGenerationSession

    fun unload()
}
