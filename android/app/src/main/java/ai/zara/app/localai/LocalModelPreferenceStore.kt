package ai.zara.app.localai

import java.io.File

enum class LocalModelProvider { EMBEDDED, OLLAMA }

data class LocalModelSelection(
    val provider: LocalModelProvider = LocalModelProvider.EMBEDDED,
    val modelName: String = "",
) {
    init {
        require(provider != LocalModelProvider.OLLAMA || validOllamaModelName(modelName)) {
            "Choose an installed Ollama model name, such as gemma3:1b"
        }
    }
}

internal fun validOllamaModelName(name: String): Boolean =
    name.length in 1..256 && name.matches(Regex("[A-Za-z0-9][A-Za-z0-9._:/-]*")) &&
        !name.contains("://")

class LocalModelPreferenceStore(private val file: File) {
    fun load(): LocalModelSelection = runCatching {
        val lines = file.readLines()
        require(lines.size == 3 && lines[0] == "local-model-v1")
        LocalModelSelection(LocalModelProvider.valueOf(lines[1]), lines[2])
    }.getOrDefault(LocalModelSelection())

    fun save(selection: LocalModelSelection) {
        check(file.parentFile?.mkdirs() != false || file.parentFile?.isDirectory == true) {
            "Local model preference directory is unavailable"
        }
        val temporary = File(file.parentFile, "${file.name}.tmp")
        try {
            temporary.writeText("local-model-v1\n${selection.provider.name}\n${selection.modelName}\n")
            check(temporary.renameTo(file)) { "Local model preference could not be saved" }
        } finally {
            temporary.delete()
        }
    }
}
