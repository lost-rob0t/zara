package ai.zara.app.localai

import ai.zara.app.prolog.PrologSource
import ai.zara.app.prolog.PrologWorkspace

data class LocalLlmConfiguration(
    val apiEnabled: Boolean = false,
    val background: Boolean = true,
    val apiPort: Int = 11_435,
    val maxOutputTokens: Int = 256,
) {
    init {
        require(apiPort in 1_024..65_535) { "Local LLM API port must be between 1024 and 65535" }
        require(maxOutputTokens in 1..4_096) {
            "Local LLM max output tokens must be between 1 and 4096"
        }
    }

    val loopbackEndpoint: String
        get() = "http://127.0.0.1:$apiPort/v1"
}

/**
 * Owns Zara's generated local-LLM configuration block inside the canonical Android Prolog config.
 *
 * Manual/operator Prolog outside the marked block is never rewritten. Secrets deliberately do not
 * belong here: API credentials remain app-private runtime state while Prolog owns non-secret policy.
 */
class LocalLlmPrologConfigStore(
    private val workspace: PrologWorkspace,
) {
    fun ensureDefaults(): LocalLlmConfiguration {
        val source = runCatching { workspace.readSource(CONFIG_SOURCE) }.getOrNull()
            ?: return save(LocalLlmConfiguration()).let { LocalLlmConfiguration() }
        if (managedBlock(source.text) == null) {
            save(LocalLlmConfiguration())
        }
        return readManaged()
    }

    fun readManaged(): LocalLlmConfiguration {
        val source = runCatching { workspace.readSource(CONFIG_SOURCE) }.getOrNull()
            ?: return LocalLlmConfiguration()
        val block = managedBlock(source.text) ?: return LocalLlmConfiguration()
        return LocalLlmConfiguration(
            apiEnabled = booleanValue(block, KEY_API_ENABLED),
            background = booleanValue(block, KEY_BACKGROUND),
            apiPort = integerValue(block, KEY_API_PORT),
            maxOutputTokens = integerValue(block, KEY_MAX_OUTPUT_TOKENS),
        )
    }

    fun save(configuration: LocalLlmConfiguration): PrologSource {
        val current = runCatching { workspace.readSource(CONFIG_SOURCE) }
            .getOrElse { PrologSource(CONFIG_SOURCE, "") }
        val unmanaged = removeManagedBlock(current.text)
        requireNoDuplicateManagedFacts(unmanaged)
        val prefix = unmanaged.trimEnd()
        val next = buildString {
            if (prefix.isNotEmpty()) {
                append(prefix)
                append("\n\n")
            }
            append(BEGIN_MARKER)
            append('\n')
            append("% Non-secret local model/API policy. The API listener is loopback-only.\n")
            append("config($KEY_API_ENABLED, ${configuration.apiEnabled}).\n")
            append("config($KEY_BACKGROUND, ${configuration.background}).\n")
            append("config($KEY_API_PORT, ${configuration.apiPort}).\n")
            append("config($KEY_MAX_OUTPUT_TOKENS, ${configuration.maxOutputTokens}).\n")
            append(END_MARKER)
            append('\n')
        }
        return workspace.saveSource(CONFIG_SOURCE, next)
    }

    private fun managedBlock(text: String): String? {
        val begin = text.indexOf(BEGIN_MARKER)
        val end = text.indexOf(END_MARKER)
        require((begin >= 0) == (end >= 0)) { "Local LLM config block markers are incomplete" }
        if (begin < 0) return null
        require(end > begin) { "Local LLM config block markers are out of order" }
        require(text.indexOf(BEGIN_MARKER, begin + BEGIN_MARKER.length) < 0) {
            "Duplicate local LLM config block"
        }
        require(text.indexOf(END_MARKER, end + END_MARKER.length) < 0) {
            "Duplicate local LLM config block"
        }
        return text.substring(begin + BEGIN_MARKER.length, end)
    }

    private fun removeManagedBlock(text: String): String {
        val begin = text.indexOf(BEGIN_MARKER)
        val end = text.indexOf(END_MARKER)
        require((begin >= 0) == (end >= 0)) { "Local LLM config block markers are incomplete" }
        if (begin < 0) return text
        require(end > begin) { "Local LLM config block markers are out of order" }
        val endOfMarker = end + END_MARKER.length
        val endOfLine = text.indexOf('\n', endOfMarker).let { if (it < 0) endOfMarker else it + 1 }
        return text.removeRange(begin, endOfLine)
    }

    private fun requireNoDuplicateManagedFacts(unmanaged: String) {
        MANAGED_KEYS.forEach { key ->
            val matcher = Regex(
                "(?m)^\\s*config\\(\\s*${Regex.escape(key)}\\s*,",
            )
            require(!matcher.containsMatchIn(unmanaged)) {
                "$key is already defined outside Zara's managed local LLM config block"
            }
        }
    }

    private fun booleanValue(block: String, key: String): Boolean =
        when (singleValue(block, key)) {
            "true" -> true
            "false" -> false
            else -> throw IllegalArgumentException("$key must be true or false")
        }

    private fun integerValue(block: String, key: String): Int =
        singleValue(block, key).toIntOrNull()
            ?: throw IllegalArgumentException("$key must be an integer")

    private fun singleValue(block: String, key: String): String {
        val matcher = Regex(
            "(?m)^\\s*config\\(\\s*${Regex.escape(key)}\\s*,\\s*([a-zA-Z0-9_+-]+)\\s*\\)\\.\\s*$",
        )
        val values = matcher.findAll(block).map { it.groupValues[1] }.toList()
        require(values.size == 1) { "$key must have exactly one managed value" }
        return values.single()
    }

    companion object {
        const val CONFIG_SOURCE = "config.pl"
        const val BEGIN_MARKER = "% BEGIN ZARA LOCAL LLM CONFIG"
        const val END_MARKER = "% END ZARA LOCAL LLM CONFIG"

        private const val KEY_API_ENABLED = "local_llm_api_enabled"
        private const val KEY_BACKGROUND = "local_llm_background"
        private const val KEY_API_PORT = "local_llm_api_port"
        private const val KEY_MAX_OUTPUT_TOKENS = "local_llm_max_output_tokens"
        private val MANAGED_KEYS = listOf(
            KEY_API_ENABLED,
            KEY_BACKGROUND,
            KEY_API_PORT,
            KEY_MAX_OUTPUT_TOKENS,
        )
    }
}
