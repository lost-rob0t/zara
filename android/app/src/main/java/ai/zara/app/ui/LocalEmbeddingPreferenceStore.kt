package ai.zara.app.ui

import ai.zara.app.prolog.LocalEmbeddingConfiguration
import java.io.File

class LocalEmbeddingPreferenceStore(private val file: File) {
    fun load(): LocalEmbeddingConfiguration = LocalEmbeddingConfiguration(
        enabled = file.takeIf(File::isFile)?.readText()?.trim() == "enabled",
    )

    fun save(configuration: LocalEmbeddingConfiguration) {
        check(file.parentFile?.mkdirs() != false) { "Embedding preference directory is unavailable" }
        val temporary = File(file.parentFile, ".${file.name}.tmp")
        temporary.writeText(if (configuration.enabled) "enabled" else "disabled")
        check(temporary.renameTo(file) || runCatching {
            file.writeText(temporary.readText())
            temporary.delete()
            true
        }.getOrDefault(false)) { "Embedding preference could not be saved" }
    }
}
