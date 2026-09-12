package ai.zara.app.ui

import ai.zara.ui.theme.ZaraTheme
import java.io.File
import java.nio.file.AtomicMoveNotSupportedException
import java.nio.file.Files
import java.nio.file.StandardCopyOption

private const val MAX_THEME_PREFERENCE_BYTES = 64

class ThemePreferenceStore(private val file: File) {
    fun load(): ZaraTheme {
        if (!file.exists() || file.length() !in 1..MAX_THEME_PREFERENCE_BYTES.toLong()) return ZaraTheme.Outrun
        return try {
            ZaraTheme.valueOf(file.readText().trim())
        } catch (_: Exception) {
            ZaraTheme.Outrun
        }
    }

    fun save(theme: ZaraTheme) {
        val directory = file.absoluteFile.parentFile
            ?: throw IllegalStateException("theme preference path has no parent directory")
        check(directory.exists() || directory.mkdirs()) {
            "theme preference directory could not be created"
        }
        val temp = Files.createTempFile(directory.toPath(), ".${file.name}.", ".tmp").toFile()
        try {
            temp.writeText(theme.name)
            replace(temp, file)
        } finally {
            if (temp.exists()) temp.delete()
        }
    }

    private fun replace(source: File, destination: File) {
        try {
            Files.move(
                source.toPath(),
                destination.toPath(),
                StandardCopyOption.ATOMIC_MOVE,
                StandardCopyOption.REPLACE_EXISTING,
            )
        } catch (_: AtomicMoveNotSupportedException) {
            Files.move(source.toPath(), destination.toPath(), StandardCopyOption.REPLACE_EXISTING)
        }
    }
}
