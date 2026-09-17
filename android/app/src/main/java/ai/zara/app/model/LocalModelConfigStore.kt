package ai.zara.app.model

import java.io.File
import java.util.Properties

class LocalModelConfigStore(
    private val file: File,
) {
    fun load(): LocalModelConfig {
        if (!file.isFile) return LocalModelConfig()
        return runCatching {
            val properties = Properties()
            file.inputStream().use(properties::load)
            LocalModelConfig(
                enabled = properties.getProperty("enabled")?.toBooleanStrictOrNull() ?: false,
                endpoint = properties.getProperty("endpoint") ?: LocalModelConfig.DEFAULT_ENDPOINT,
                model = properties.getProperty("model") ?: LocalModelConfig.DEFAULT_MODEL,
                quantization = properties.getProperty("quantization")?.takeIf(String::isNotBlank),
                maxOutputTokens = properties.getProperty("max_output_tokens")
                    ?.toIntOrNull()
                    ?: LocalModelConfig.DEFAULT_MAX_OUTPUT_TOKENS,
                deadlineMs = properties.getProperty("deadline_ms")
                    ?.toLongOrNull()
                    ?: LocalModelConfig.DEFAULT_DEADLINE_MS,
            ).validated()
        }.getOrElse {
            LocalModelConfig()
        }
    }

    fun save(config: LocalModelConfig): LocalModelConfig {
        val safe = config.validated()
        check(file.parentFile?.mkdirs() != false) { "Local model config directory is unavailable" }
        val properties = Properties().apply {
            setProperty("enabled", safe.enabled.toString())
            setProperty("endpoint", safe.endpoint)
            setProperty("model", safe.model)
            safe.quantization?.let { setProperty("quantization", it) }
            setProperty("max_output_tokens", safe.maxOutputTokens.toString())
            setProperty("deadline_ms", safe.deadlineMs.toString())
        }
        val temporary = File(file.parentFile, "${file.name}.tmp")
        temporary.outputStream().use { properties.store(it, "Zara local model") }
        if (file.exists()) check(file.delete()) { "Old local model config could not be replaced" }
        check(temporary.renameTo(file)) { "Local model config could not be committed" }
        return safe
    }
}
