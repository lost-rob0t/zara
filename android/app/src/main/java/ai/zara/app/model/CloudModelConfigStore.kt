package ai.zara.app.model

import java.io.File
import java.nio.file.AtomicMoveNotSupportedException
import java.nio.file.Files
import java.nio.file.StandardCopyOption
import java.util.Properties

class CloudModelConfigStore(
    private val file: File,
) {
    fun load(): CloudModelConfig {
        if (!file.isFile) return CloudModelConfig()
        return runCatching {
            val properties = Properties()
            file.inputStream().use(properties::load)
            CloudModelConfig(
                enabled = properties.getProperty("enabled")?.toBooleanStrictOrNull() ?: false,
                provider = properties.getProperty("provider")
                    ?.let(CloudModelProvider::fromWireName)
                    ?: CloudModelProvider.OPENAI_COMPATIBLE,
                endpoint = properties.getProperty("endpoint") ?: CloudModelConfig.DEFAULT_STARINTEL_ENDPOINT,
                model = properties.getProperty("model") ?: "",
                appName = properties.getProperty("app_name") ?: CloudModelConfig.DEFAULT_APP_NAME,
                maxOutputTokens = properties.getProperty("max_output_tokens")
                    ?.toIntOrNull()
                    ?: CloudModelConfig.DEFAULT_MAX_OUTPUT_TOKENS,
                deadlineMs = properties.getProperty("deadline_ms")
                    ?.toLongOrNull()
                    ?: CloudModelConfig.DEFAULT_DEADLINE_MS,
            ).validated()
        }.getOrElse { CloudModelConfig() }
    }

    fun save(config: CloudModelConfig): CloudModelConfig {
        val safe = config.validated()
        val parent = file.parentFile
        check(parent == null || parent.mkdirs() || parent.isDirectory) {
            "Cloud model config directory is unavailable"
        }
        val properties = Properties().apply {
            setProperty("enabled", safe.enabled.toString())
            setProperty("provider", safe.provider.wireName)
            setProperty("endpoint", safe.endpoint)
            setProperty("model", safe.model)
            setProperty("app_name", safe.appName)
            setProperty("max_output_tokens", safe.maxOutputTokens.toString())
            setProperty("deadline_ms", safe.deadlineMs.toString())
        }
        val temporary = File(parent, ".${file.name}.tmp")
        try {
            temporary.outputStream().use { properties.store(it, "Zara cloud model") }
            try {
                Files.move(
                    temporary.toPath(),
                    file.toPath(),
                    StandardCopyOption.ATOMIC_MOVE,
                    StandardCopyOption.REPLACE_EXISTING,
                )
            } catch (_: AtomicMoveNotSupportedException) {
                Files.move(
                    temporary.toPath(),
                    file.toPath(),
                    StandardCopyOption.REPLACE_EXISTING,
                )
            }
        } finally {
            temporary.delete()
        }
        return safe
    }
}
