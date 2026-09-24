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
                schemaVersion = properties.getProperty("schema_version")
                    ?.toInt()
                    ?: CloudModelConfig.SCHEMA_VERSION,
                enabled = properties.getProperty("enabled")?.toBooleanStrictOrNull() ?: false,
                provider = properties.getProperty("provider")
                    ?.let(CloudModelProvider::fromWireName)
                    ?: CloudModelProvider.OPENAI_COMPATIBLE,
                endpoint = properties.getProperty("endpoint") ?: CloudModelConfig.DEFAULT_STARINTEL_ENDPOINT,
                model = properties.getProperty("model") ?: "",
                appName = properties.getProperty("app_name") ?: CloudModelConfig.DEFAULT_APP_NAME,
                maxOutputTokens = properties.getProperty("max_output_tokens")
                    ?.toInt()
                    ?: CloudModelConfig.DEFAULT_MAX_OUTPUT_TOKENS,
                deadlineMs = properties.getProperty("deadline_ms")
                    ?.toLong()
                    ?: CloudModelConfig.DEFAULT_DEADLINE_MS,
                openRouterPolicy = OpenRouterProviderPolicy(
                    sort = properties.getProperty("openrouter.sort")
                        ?.let(OpenRouterProviderSort::fromWireName)
                        ?: OpenRouterProviderSort.PRICE,
                    allowFallbacks = properties.getProperty("openrouter.allow_fallbacks")
                        ?.toBooleanStrictOrNull()
                        ?: true,
                    quantizations = properties.optionalCsv("openrouter.quantizations")
                        ?: OpenRouterProviderPolicy.DEFAULT_ALLOWED_QUANTIZATIONS,
                    dataCollection = properties.getProperty("openrouter.data_collection")
                        ?.let(OpenRouterDataCollection::fromWireName)
                        ?: OpenRouterDataCollection.DENY,
                    zeroDataRetention = properties.getProperty("openrouter.zdr")
                        ?.toBooleanStrictOrNull()
                        ?: false,
                    requireParameters = properties.getProperty("openrouter.require_parameters")
                        ?.toBooleanStrictOrNull()
                        ?: true,
                    order = properties.optionalCsv("openrouter.order") ?: emptyList(),
                    only = properties.optionalCsv("openrouter.only") ?: emptyList(),
                    ignore = properties.optionalCsv("openrouter.ignore") ?: emptyList(),
                    maxPromptUsdPerMillion = properties.optionalDouble("openrouter.max_prompt_usd_per_m"),
                    maxCompletionUsdPerMillion = properties.optionalDouble("openrouter.max_completion_usd_per_m"),
                ),
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
            setProperty("schema_version", safe.schemaVersion.toString())
            setProperty("enabled", safe.enabled.toString())
            setProperty("provider", safe.provider.wireName)
            setProperty("endpoint", safe.endpoint)
            setProperty("model", safe.model)
            setProperty("app_name", safe.appName)
            setProperty("max_output_tokens", safe.maxOutputTokens.toString())
            setProperty("deadline_ms", safe.deadlineMs.toString())
            setProperty("openrouter.sort", safe.openRouterPolicy.sort.wireName)
            setProperty("openrouter.allow_fallbacks", safe.openRouterPolicy.allowFallbacks.toString())
            setProperty("openrouter.quantizations", safe.openRouterPolicy.quantizations.joinToString(","))
            setProperty("openrouter.data_collection", safe.openRouterPolicy.dataCollection.wireName)
            setProperty("openrouter.zdr", safe.openRouterPolicy.zeroDataRetention.toString())
            setProperty("openrouter.require_parameters", safe.openRouterPolicy.requireParameters.toString())
            setProperty("openrouter.order", safe.openRouterPolicy.order.joinToString(","))
            setProperty("openrouter.only", safe.openRouterPolicy.only.joinToString(","))
            setProperty("openrouter.ignore", safe.openRouterPolicy.ignore.joinToString(","))
            safe.openRouterPolicy.maxPromptUsdPerMillion?.let {
                setProperty("openrouter.max_prompt_usd_per_m", it.toString())
            }
            safe.openRouterPolicy.maxCompletionUsdPerMillion?.let {
                setProperty("openrouter.max_completion_usd_per_m", it.toString())
            }
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

    private fun Properties.optionalCsv(key: String): List<String>? =
        getProperty(key)?.split(',')?.map(String::trim)?.filter(String::isNotEmpty)

    private fun Properties.optionalDouble(key: String): Double? =
        getProperty(key)?.trim()?.takeIf(String::isNotEmpty)?.toDouble()
}
