package ai.zara.app.ui

import ai.zara.app.runtime.RuntimeMode
import java.io.File

class RuntimeModePreferenceStore(private val file: File) {
    fun load(): RuntimeMode = runCatching {
        RuntimeMode.valueOf(file.readText().trim())
    }.getOrDefault(RuntimeMode.Auto)

    fun save(mode: RuntimeMode) {
        check(file.parentFile?.mkdirs() != false || file.parentFile?.isDirectory == true) {
            "Runtime preference directory is unavailable"
        }
        val temporary = File(file.parentFile, "${file.name}.tmp")
        try {
            temporary.writeText(mode.name)
            check(temporary.renameTo(file) || run {
                file.delete()
                temporary.renameTo(file)
            }) { "Runtime preference could not be saved" }
        } finally {
            temporary.delete()
        }
    }
}
