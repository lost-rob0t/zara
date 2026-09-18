package ai.zara.app.ui

import ai.zara.app.runtime.EMBEDDED_LOCAL_RUNTIME_ID
import java.io.File

class AssistantRuntimePreferenceStore(private val file: File) {
    fun load(): String = runCatching {
        val raw = file.readText().trim()
        if (raw.startsWith(FORMAT_PREFIX)) {
            normalize(raw.removePrefix(FORMAT_PREFIX))
        } else {
            EMBEDDED_LOCAL_RUNTIME_ID
        }
    }.getOrDefault(EMBEDDED_LOCAL_RUNTIME_ID)

    fun save(runtimeId: String) {
        val normalized = normalize(runtimeId)
        check(file.parentFile?.mkdirs() != false || file.parentFile?.isDirectory == true) {
            "Assistant runtime preference directory is unavailable"
        }
        val temporary = File(file.parentFile, "${file.name}.tmp")
        try {
            temporary.writeText("$FORMAT_PREFIX$normalized")
            check(
                temporary.renameTo(file) || run {
                    file.delete()
                    temporary.renameTo(file)
                },
            ) { "Assistant runtime preference could not be saved" }
        } finally {
            temporary.delete()
        }
    }

    private fun normalize(value: String): String {
        val normalized = value.trim()
        require(normalized.matches(Regex("[a-z0-9][a-z0-9._-]{0,63}"))) {
            "Assistant runtime id is invalid"
        }
        return normalized
    }

    private companion object {
        const val FORMAT_PREFIX = "v1:"
    }
}
