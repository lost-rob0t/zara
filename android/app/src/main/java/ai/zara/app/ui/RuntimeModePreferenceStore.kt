package ai.zara.app.ui

import ai.zara.app.runtime.RuntimeMode
import java.io.File

class RuntimeModePreferenceStore(private val file: File) {
    fun load(): RuntimeMode = runCatching {
        val raw = file.readText().trim()
        if (raw.startsWith(FORMAT_PREFIX)) {
            RuntimeMode.valueOf(raw.removePrefix(FORMAT_PREFIX))
        } else {
            // Pre-v2 builds could leave a device permanently in Remote mode even when
            // no authenticated server existed. Migrate that stale preference once so
            // an upgraded local-first APK is immediately usable offline.
            when (RuntimeMode.valueOf(raw)) {
                RuntimeMode.Remote -> RuntimeMode.Local
                else -> RuntimeMode.valueOf(raw)
            }
        }
    }.getOrDefault(RuntimeMode.Local)

    fun save(mode: RuntimeMode) {
        check(file.parentFile?.mkdirs() != false || file.parentFile?.isDirectory == true) {
            "Runtime preference directory is unavailable"
        }
        val temporary = File(file.parentFile, "${file.name}.tmp")
        try {
            temporary.writeText("$FORMAT_PREFIX${mode.name}")
            check(temporary.renameTo(file) || run {
                file.delete()
                temporary.renameTo(file)
            }) { "Runtime preference could not be saved" }
        } finally {
            temporary.delete()
        }
    }

    private companion object {
        const val FORMAT_PREFIX = "v2:"
    }
}
