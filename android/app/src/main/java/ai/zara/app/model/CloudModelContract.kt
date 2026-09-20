package ai.zara.app.model

import java.net.URI

enum class CloudModelProvider(
    val wireName: String,
    val codingOnly: Boolean,
    val requiresApiKey: Boolean,
    val localModelServer: Boolean,
) {
    OPENAI_COMPATIBLE("openai-compatible", false, true, false),
    OPENROUTER("openrouter", false, true, false),
    OLLAMA("ollama", false, false, true),
    LLAMA_CPP("llama.cpp", false, false, true),
    ZAI_CODING_PLAN("z-ai-coding-plan", true, true, false),
    ;

    companion object {
        fun fromWireName(value: String): CloudModelProvider =
            entries.firstOrNull { it.wireName == value }
                ?: throw IllegalArgumentException("Unknown cloud model provider: $value")
    }
}

enum class OpenRouterProviderSort(val wireName: String) {
    PRICE("price"),
    THROUGHPUT("throughput"),
    LATENCY("latency"),
    ;

    companion object {
        fun fromWireName(value: String): OpenRouterProviderSort =
            entries.firstOrNull { it.wireName == value }
                ?: throw IllegalArgumentException("Unknown OpenRouter provider sort: $value")
    }
}

enum class OpenRouterDataCollection(val wireName: String) {
    ALLOW("allow"),
    DENY("deny"),
    ;

    companion object {
        fun fromWireName(value: String): OpenRouterDataCollection =
            entries.firstOrNull { it.wireName == value }
                ?: throw IllegalArgumentException("Unknown OpenRouter data-collection policy: $value")
    }
}

data class OpenRouterProviderPolicy(
    val sort: OpenRouterProviderSort = OpenRouterProviderSort.PRICE,
    val allowFallbacks: Boolean = true,
    val quantizations: List<String> = DEFAULT_ALLOWED_QUANTIZATIONS,
    val dataCollection: OpenRouterDataCollection = OpenRouterDataCollection.DENY,
    val zeroDataRetention: Boolean = false,
    val requireParameters: Boolean = true,
    val order: List<String> = emptyList(),
    val only: List<String> = emptyList(),
    val ignore: List<String> = emptyList(),
    val maxPromptUsdPerMillion: Double? = null,
    val maxCompletionUsdPerMillion: Double? = null,
) {
    fun validated(): OpenRouterProviderPolicy {
        val safeQuantizations = normalizeTokens(quantizations, "quantization").map { it.lowercase() }
        require(safeQuantizations.size == safeQuantizations.toSet().size) {
            "OpenRouter quantization list contains duplicates"
        }
        require("unknown" !in safeQuantizations) {
            "OpenRouter quantization must be explicit; unknown is not routable"
        }
        val safeOrder = normalizeProviderSlugs(order)
        val safeOnly = normalizeProviderSlugs(only)
        val safeIgnore = normalizeProviderSlugs(ignore)
        require(safeOnly.intersect(safeIgnore.toSet()).isEmpty()) {
            "OpenRouter provider allowlist and blocklist must not overlap"
        }
        validatePrice(maxPromptUsdPerMillion, "prompt")
        validatePrice(maxCompletionUsdPerMillion, "completion")
        return copy(
            quantizations = safeQuantizations,
            order = safeOrder,
            only = safeOnly,
            ignore = safeIgnore,
        )
    }

    internal fun toWireMap(): Map<String, Any> {
        val safe = validated()
        return buildMap {
            put("sort", safe.sort.wireName)
            put("allow_fallbacks", safe.allowFallbacks)
            if (safe.quantizations.isNotEmpty()) put("quantizations", safe.quantizations)
            put("data_collection", safe.dataCollection.wireName)
            put("zdr", safe.zeroDataRetention)
            put("require_parameters", safe.requireParameters)
            if (safe.order.isNotEmpty()) put("order", safe.order)
            if (safe.only.isNotEmpty()) put("only", safe.only)
            if (safe.ignore.isNotEmpty()) put("ignore", safe.ignore)
            if (safe.maxPromptUsdPerMillion != null || safe.maxCompletionUsdPerMillion != null) {
                put(
                    "max_price",
                    buildMap {
                        safe.maxPromptUsdPerMillion?.let { put("prompt", it) }
                        safe.maxCompletionUsdPerMillion?.let { put("completion", it) }
                    },
                )
            }
        }
    }

    private fun normalizeProviderSlugs(values: List<String>): List<String> {
        val safe = normalizeTokens(values, "provider").map { it.lowercase() }
        require(safe.size == safe.toSet().size) {
            "OpenRouter provider list contains duplicates"
        }
        safe.forEach { value ->
            require(PROVIDER_SLUG.matches(value)) { "OpenRouter provider slug is invalid: $value" }
        }
        return safe
    }

    private fun normalizeTokens(values: List<String>, label: String): List<String> {
        require(values.size <= MAX_LIST_ITEMS) { "OpenRouter $label list is too large" }
        val safe = values.map(String::trim).filter(String::isNotEmpty)
        require(safe.size == safe.toSet().size) { "OpenRouter $label list contains duplicates" }
        safe.forEach { value ->
            require(value.length <= MAX_TOKEN_CHARS) { "OpenRouter $label token is too long" }
            require(TOKEN.matches(value)) { "OpenRouter $label token is invalid: $value" }
        }
        return safe
    }

    private fun validatePrice(value: Double?, label: String) {
        if (value == null) return
        require(value.isFinite() && value >= 0.0 && value <= MAX_USD_PER_MILLION) {
            "OpenRouter $label price ceiling is invalid"
        }
    }

    companion object {
        val DEFAULT_ALLOWED_QUANTIZATIONS: List<String> = listOf("fp16", "bf16", "fp8")
        private const val MAX_LIST_ITEMS = 32
        private const val MAX_TOKEN_CHARS = 128
        private const val MAX_USD_PER_MILLION = 1_000_000.0
        private val TOKEN = Regex("[A-Za-z0-9][A-Za-z0-9._/-]*")
        private val PROVIDER_SLUG = Regex("[a-z0-9][a-z0-9._/-]*")
    }
}

enum class CloudModelPurpose { GENERAL, CODING }

enum class CloudModelFailureReason {
    DISABLED,
    AUTHENTICATION,
    RATE_LIMIT,
    UNAVAILABLE,
    UNSUPPORTED,
    CANCELLED,
    TIMEOUT,
    INVALID_RESPONSE,
}

class CloudModelException(
    val reason: CloudModelFailureReason,
    message: String,
    cause: Throwable? = null,
) : IllegalStateException(message, cause)

data class CloudModelConfig(
    val schemaVersion: Int = SCHEMA_VERSION,
    val enabled: Boolean = false,
    val provider: CloudModelProvider = CloudModelProvider.OPENAI_COMPATIBLE,
    val endpoint: String = DEFAULT_STARINTEL_ENDPOINT,
    val model: String = "",
    val appName: String = DEFAULT_APP_NAME,
    val maxOutputTokens: Int = DEFAULT_MAX_OUTPUT_TOKENS,
    val deadlineMs: Long = DEFAULT_DEADLINE_MS,
    val openRouterPolicy: OpenRouterProviderPolicy = OpenRouterProviderPolicy(),
) {
    fun validated(): CloudModelConfig {
        require(schemaVersion == SCHEMA_VERSION) { "Unsupported cloud model config schema: $schemaVersion" }
        val normalizedEndpoint = endpoint.trim().removeSuffix("/")
        val uri = runCatching { URI(normalizedEndpoint) }
            .getOrElse { throw IllegalArgumentException("Cloud model endpoint is invalid", it) }
        val scheme = uri.scheme?.lowercase()
        require(scheme == "https" || (provider.localModelServer && scheme == "http")) {
            "Hosted model endpoints must use HTTPS; local model servers may use explicit private HTTP"
        }
        require(!uri.host.isNullOrBlank()) { "Cloud model endpoint must include a host" }
        require(uri.rawUserInfo == null) { "Cloud model endpoint must not contain credentials" }
        require(uri.rawQuery == null && uri.rawFragment == null) {
            "Cloud model endpoint must not contain query or fragment data"
        }
        require(uri.port == -1 || uri.port in 1..65535) { "Cloud model endpoint port is invalid" }
        if (scheme == "http") {
            require(isPrivateModelServerHost(uri.host)) {
                "Cleartext model-server endpoints must use localhost or a private IPv4 address"
            }
        }
        require(normalizedEndpoint.length <= MAX_ENDPOINT_CHARS) { "Cloud model endpoint is too long" }
        require(model.length <= MAX_MODEL_CHARS) { "Cloud model name is too long" }
        if (enabled) require(model.isNotBlank()) { "Cloud model name is required when enabled" }
        val normalizedAppName = appName.trim()
        require(normalizedAppName.isNotEmpty()) { "LLM app name is required" }
        require(normalizedAppName.length <= MAX_APP_NAME_CHARS) { "LLM app name is too long" }
        require(normalizedAppName.none { it == '\r' || it == '\n' }) { "LLM app name contains invalid characters" }
        require(maxOutputTokens in 1..MAX_OUTPUT_TOKENS) { "Cloud model output token limit is invalid" }
        require(deadlineMs in MIN_DEADLINE_MS..MAX_DEADLINE_MS) { "Cloud model deadline is invalid" }
        val safePolicy = openRouterPolicy.validated()

        when (provider) {
            CloudModelProvider.OPENAI_COMPATIBLE -> Unit
            CloudModelProvider.OLLAMA -> Unit
            CloudModelProvider.LLAMA_CPP -> Unit
            CloudModelProvider.OPENROUTER -> require(normalizedEndpoint in OPENROUTER_ENDPOINTS) {
                "OpenRouter must use an official OpenRouter API base URL"
            }
            CloudModelProvider.ZAI_CODING_PLAN -> require(normalizedEndpoint == ZAI_CODING_ENDPOINT) {
                "Z.AI Coding Plan must use its coding-only endpoint"
            }
        }

        return copy(
            endpoint = normalizedEndpoint,
            model = model.trim(),
            appName = normalizedAppName,
            openRouterPolicy = safePolicy,
        )
    }

    companion object {
        const val SCHEMA_VERSION = 1
        const val DEFAULT_STARINTEL_ENDPOINT = "https://llm.starintel.actor/v1"
        const val OLLAMA_ENDPOINT = "http://127.0.0.1:11434"
        const val LLAMA_CPP_ENDPOINT = "http://127.0.0.1:8080/v1"
        const val STATINTEL_ENDPOINT = "https://llm.statintel.actor/v1"
        const val OPENROUTER_ENDPOINT = "https://openrouter.ai/api/v1"
        const val ZAI_CODING_ENDPOINT = "https://api.z.ai/api/coding/paas/v4"
        const val DEFAULT_APP_NAME = "Zara Android"
        const val DEFAULT_MAX_OUTPUT_TOKENS = 1_024
        const val DEFAULT_DEADLINE_MS = 90_000L
        const val MAX_OUTPUT_TOKENS = 8_192
        const val MIN_DEADLINE_MS = 1_000L
        const val MAX_DEADLINE_MS = 300_000L
        const val MAX_ENDPOINT_CHARS = 2_048
        const val MAX_MODEL_CHARS = 256
        const val MAX_APP_NAME_CHARS = 80

        private fun isPrivateModelServerHost(host: String): Boolean {
            val normalized = host.lowercase()
            if (normalized == "localhost" || normalized == "::1") return true
            val parts = normalized.split('.')
            if (parts.size != 4) return false
            val octets = parts.map { it.toIntOrNull() ?: return false }
            if (octets.any { it !in 0..255 }) return false
            return octets[0] == 127 ||
                octets[0] == 10 ||
                (octets[0] == 172 && octets[1] in 16..31) ||
                (octets[0] == 192 && octets[1] == 168)
        }

        private val OPENROUTER_ENDPOINTS = setOf(
            OPENROUTER_ENDPOINT,
            "https://us.openrouter.ai/api/v1",
            "https://eu.openrouter.ai/api/v1",
        )
    }
}

data class CloudModelRequest(
    val prompt: String,
    val purpose: CloudModelPurpose,
    val maxOutputTokens: Int,
    val deadlineMs: Long,
    val appName: String,
) {
    init {
        require(prompt.isNotBlank()) { "Cloud model prompt is required" }
        require(prompt.length <= MAX_PROMPT_CHARS) { "Cloud model prompt is too large" }
    }

    fun systemPrompt(): String = when (purpose) {
        CloudModelPurpose.GENERAL ->
            "You are $appName. Answer the user directly. " +
                "Do not claim to execute Android actions, tools, timers, apps, URLs, or other side effects; " +
                "those remain owned by the symbolic runtime."
        CloudModelPurpose.CODING ->
            "You are $appName in an explicit coding session. Focus on programming and software-engineering work. " +
                "Do not claim to execute Android side effects unless the symbolic runtime reports that they happened."
    }

    companion object {
        const val MAX_PROMPT_CHARS = 65_536
    }
}

data class CloudModelIdentity(
    val provider: CloudModelProvider,
    val endpoint: String,
    val model: String,
    val appName: String,
)

data class CloudModelResult(
    val identity: CloudModelIdentity,
    val text: String,
    val elapsedMs: Long,
)

interface CloudModelBackend : AutoCloseable {
    fun generate(
        config: CloudModelConfig,
        apiKey: String?,
        request: CloudModelRequest,
        cancelled: () -> Boolean,
        onText: (String) -> Unit,
    ): CloudModelResult

    fun models(
        config: CloudModelConfig,
        apiKey: String?,
        cancelled: () -> Boolean = { false },
    ): List<String> = emptyList()

    fun cancel() = Unit

    override fun close() = Unit
}
