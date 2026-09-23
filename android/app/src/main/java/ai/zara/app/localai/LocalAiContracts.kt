package ai.zara.app.localai

import java.util.Locale

enum class LocalModelBackend {
    CPU,
    GPU,
    NPU,
}

enum class LocalModelFormat(
    val wireName: String,
    val extension: String,
) {
    LITERT_LM("litert-lm", ".litertlm"),
    GGUF("gguf", ".gguf"),
    ONNX("onnx", ".onnx"),
    TFLITE("tflite", ".tflite");

    companion object {
        fun requireKnown(value: String): LocalModelFormat {
            val normalized = value.trim().lowercase(Locale.ROOT)
            return entries.firstOrNull { it.wireName == normalized }
                ?: throw IllegalArgumentException("Unsupported or unknown model format: $value")
        }
    }
}

enum class LocalModelQuantization(val wireName: String) {
    DYNAMIC_INT4("dynamic-int4"),
    INT4("int4"),
    INT8("int8"),
    FP8("fp8"),
    BF16("bf16"),
    FP16("fp16"),
    FP32("fp32"),
    Q8_0("q8_0"),
    Q6_K("q6_k"),
    Q5_K_M("q5_k_m"),
    Q5_K_S("q5_k_s"),
    Q4_K_M("q4_k_m"),
    Q4_K_S("q4_k_s"),
    Q4_0("q4_0"),
    Q3_K_M("q3_k_m"),
    Q2_K("q2_k");

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
    val format: LocalModelFormat = LocalModelFormat.LITERT_LM,
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
    val format: LocalModelFormat = LocalModelFormat.LITERT_LM,
) {
    init {
        LocalModelMetadata(id, version, quantization, sha256, maxContextTokens, backend, format)
        require(path.lowercase(Locale.ROOT).endsWith(format.extension)) {
            "${format.wireName} model must use the ${format.extension} container"
        }
    }

    fun metadata(): LocalModelMetadata = LocalModelMetadata(
        id = id,
        version = version,
        quantization = quantization,
        sha256 = sha256,
        maxContextTokens = maxContextTokens,
        backend = backend,
        format = format,
    )
}

data class LocalGenerationRequest(
    val prompt: String,
    val maxOutputTokens: Int = 256,
    val imagePng: ByteArray? = null,
) {
    init {
        require(prompt.isNotBlank()) { "Local model prompt is required" }
        require(prompt.length <= 32_768) { "Local model prompt is too large" }
        require(maxOutputTokens in 1..4_096) { "Local model output limit is outside the supported range" }
        imagePng?.let { image ->
            require(image.size in 8..MAX_LOCAL_IMAGE_BYTES) { "Local model image is outside the supported size range" }
            require(image.copyOfRange(0, PNG_MAGIC.size).contentEquals(PNG_MAGIC)) {
                "Local model image must be a PNG"
            }
        }
    }

    companion object {
        const val MAX_LOCAL_IMAGE_BYTES = 16 * 1024 * 1024
        private val PNG_MAGIC = byteArrayOf(
            0x89.toByte(), 0x50, 0x4E, 0x47, 0x0D, 0x0A, 0x1A, 0x0A,
        )
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
