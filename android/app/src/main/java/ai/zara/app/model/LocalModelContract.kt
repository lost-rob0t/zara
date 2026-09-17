package ai.zara.app.model

import java.net.URI

enum class LocalModelFailureReason {
    DISABLED,
    UNAVAILABLE,
    UNSUPPORTED,
    OUT_OF_MEMORY,
    CANCELLED,
    TIMEOUT,
    INVALID_RESPONSE,
}

class LocalModelException(
    val reason: LocalModelFailureReason,
    message: String,
    cause: Throwable? = null,
) : IllegalStateException(message, cause)

data class LocalModelIdentity(
    val backend: String,
    val model: String,
    val quantization: String?,
)

data class LocalModelConfig(
    val enabled: Boolean = false,
    val endpoint: String = DEFAULT_ENDPOINT,
    val model: String = DEFAULT_MODEL,
    val quantization: String? = null,
    val maxOutputTokens: Int = DEFAULT_MAX_OUTPUT_TOKENS,
    val deadlineMs: Long = DEFAULT_DEADLINE_MS,
) {
    fun validated(): LocalModelConfig {
        val uri = runCatching { URI(endpoint.trim()) }
            .getOrElse { throw IllegalArgumentException("Local model endpoint is invalid", it) }
        require(uri.scheme in setOf("http", "https")) {
            "Local model endpoint must use HTTP or HTTPS"
        }
        require(uri.rawUserInfo == null) { "Local model endpoint must not contain credentials" }
        require(uri.rawQuery == null && uri.rawFragment == null) {
            "Local model endpoint must not contain query or fragment data"
        }
        require(uri.path.isNullOrEmpty() || uri.path == "/") {
            "Local model endpoint must be an origin, not an API path"
        }
        require(uri.host?.lowercase() in LOOPBACK_HOSTS) {
            "Local model endpoint must stay on this device"
        }
        require(uri.port == -1 || uri.port in 1..65535) {
            "Local model endpoint port is invalid"
        }
        require(model.isNotBlank() && model.length <= 128) { "Local model name is invalid" }
        require(quantization == null || quantization.length <= 64) {
            "Local model quantization label is invalid"
        }
        require(maxOutputTokens in 1..MAX_OUTPUT_TOKENS) {
            "Local model output token limit is invalid"
        }
        require(deadlineMs in MIN_DEADLINE_MS..MAX_DEADLINE_MS) {
            "Local model deadline is invalid"
        }
        return copy(
            endpoint = uri.toString().removeSuffix("/"),
            model = model.trim(),
            quantization = quantization?.trim()?.takeIf(String::isNotEmpty),
        )
    }

    fun identity(): LocalModelIdentity = LocalModelIdentity(
        backend = "openai-loopback",
        model = model,
        quantization = quantization,
    )

    companion object {
        const val DEFAULT_ENDPOINT = "http://127.0.0.1:8080"
        const val DEFAULT_MODEL = "local-model"
        const val DEFAULT_MAX_OUTPUT_TOKENS = 256
        const val DEFAULT_DEADLINE_MS = 30_000L
        const val MAX_OUTPUT_TOKENS = 2_048
        const val MIN_DEADLINE_MS = 1_000L
        const val MAX_DEADLINE_MS = 120_000L
        private val LOOPBACK_HOSTS = setOf("localhost", "127.0.0.1", "::1")
    }
}

data class LocalModelRequest(
    val prompt: String,
    val maxOutputTokens: Int,
    val deadlineMs: Long,
    val systemPrompt: String = DEFAULT_SYSTEM_PROMPT,
) {
    init {
        require(prompt.isNotBlank()) { "Local model prompt is required" }
        require(prompt.length <= MAX_PROMPT_CHARS) { "Local model prompt is too large" }
        require(maxOutputTokens in 1..LocalModelConfig.MAX_OUTPUT_TOKENS) {
            "Local model output token limit is invalid"
        }
        require(deadlineMs in LocalModelConfig.MIN_DEADLINE_MS..LocalModelConfig.MAX_DEADLINE_MS) {
            "Local model deadline is invalid"
        }
    }

    companion object {
        const val MAX_PROMPT_CHARS = 16_384
        const val DEFAULT_SYSTEM_PROMPT =
            "You are Zara's local conversational model. Answer the user directly. " +
                "Do not claim to execute tools, device actions, timers, apps, URLs, or other side effects. " +
                "Those actions are owned by Zara's symbolic runtime."
    }
}

data class LocalModelResult(
    val identity: LocalModelIdentity,
    val text: String,
    val elapsedMs: Long,
)

interface LocalModelBackend : AutoCloseable {
    fun generate(
        config: LocalModelConfig,
        request: LocalModelRequest,
        cancelled: () -> Boolean,
        onText: (String) -> Unit,
    ): LocalModelResult

    override fun close() = Unit
}
