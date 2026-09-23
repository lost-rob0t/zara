package ai.zara.app.ui

import ai.zara.app.prolog.LocalEmbeddingConfiguration
import android.content.Context
import java.io.File
import java.io.FileOutputStream
import java.nio.file.AtomicMoveNotSupportedException
import java.nio.file.Files
import java.nio.file.StandardCopyOption

private const val MAX_EMBEDDING_PREFERENCE_BYTES = 64
private const val ENABLED = "enabled"
private const val DISABLED = "disabled"

sealed interface LocalEmbeddingPreferenceSaveResult {
    object Saved : LocalEmbeddingPreferenceSaveResult
    data class Failed(val message: String) : LocalEmbeddingPreferenceSaveResult
}

data class LocalEmbeddingPreferenceLoadResult(
    val configuration: LocalEmbeddingConfiguration,
    val warning: String? = null,
)

/**
 * Single application-owned store for the local embedding toggle.
 *
 * Every Android process resolves the same package files directory through the
 * application context. The root-level legacy file is read only for migration;
 * all current writes go to the canonical application preference path.
 */
class LocalEmbeddingPreferenceStore private constructor(
    private val file: File,
    private val legacyFile: File?,
) {
    fun load(): LocalEmbeddingPreferenceLoadResult {
        return when (val current = read(file)) {
            is PreferenceRead.Value -> LocalEmbeddingPreferenceLoadResult(
                configuration = LocalEmbeddingConfiguration(enabled = current.enabled),
            )
            is PreferenceRead.Failed -> LocalEmbeddingPreferenceLoadResult(
                configuration = LocalEmbeddingConfiguration(enabled = false),
                warning = current.message,
            )
            PreferenceRead.Missing -> loadLegacy()
        }
    }

    fun save(configuration: LocalEmbeddingConfiguration): LocalEmbeddingPreferenceSaveResult {
        val directory = file.absoluteFile.parentFile
            ?: return LocalEmbeddingPreferenceSaveResult.Failed(
                "Embedding preference path has no parent directory",
            )

        if (!directory.isDirectory && !directory.mkdirs() && !directory.isDirectory) {
            return LocalEmbeddingPreferenceSaveResult.Failed(
                "Embedding preference directory is unavailable",
            )
        }

        var temporary: File? = null
        return try {
            temporary = Files.createTempFile(
                directory.toPath(),
                ".${file.name}.",
                ".tmp",
            ).toFile()
            FileOutputStream(temporary).use { output ->
                output.write(if (configuration.enabled) ENABLED.encodeToByteArray() else DISABLED.encodeToByteArray())
                output.flush()
                output.fd.sync()
            }
            replace(temporary, file)
            LocalEmbeddingPreferenceSaveResult.Saved
        } catch (_: Exception) {
            LocalEmbeddingPreferenceSaveResult.Failed(
                "Embedding preference could not be saved",
            )
        } finally {
            temporary?.let { temp ->
                if (temp.exists()) temp.delete()
            }
        }
    }

    private fun loadLegacy(): LocalEmbeddingPreferenceLoadResult {
        val legacy = legacyFile ?: return LocalEmbeddingPreferenceLoadResult(
            configuration = LocalEmbeddingConfiguration(enabled = false),
        )
        return when (val old = read(legacy)) {
            PreferenceRead.Missing -> LocalEmbeddingPreferenceLoadResult(
                configuration = LocalEmbeddingConfiguration(enabled = false),
            )
            is PreferenceRead.Failed -> LocalEmbeddingPreferenceLoadResult(
                configuration = LocalEmbeddingConfiguration(enabled = false),
                warning = old.message,
            )
            is PreferenceRead.Value -> {
                val configuration = LocalEmbeddingConfiguration(enabled = old.enabled)
                when (val migration = save(configuration)) {
                    LocalEmbeddingPreferenceSaveResult.Saved -> {
                        legacy.delete()
                        LocalEmbeddingPreferenceLoadResult(configuration = configuration)
                    }
                    is LocalEmbeddingPreferenceSaveResult.Failed -> LocalEmbeddingPreferenceLoadResult(
                        configuration = configuration,
                        warning = "${migration.message}; using the existing embedding preference until migration can retry",
                    )
                }
            }
        }
    }

    private fun read(candidate: File): PreferenceRead {
        if (!candidate.exists()) return PreferenceRead.Missing
        if (!candidate.isFile || candidate.length() !in 1..MAX_EMBEDDING_PREFERENCE_BYTES.toLong()) {
            return PreferenceRead.Failed("Embedding preference is unreadable")
        }
        return try {
            when (candidate.readText().trim()) {
                ENABLED -> PreferenceRead.Value(enabled = true)
                DISABLED -> PreferenceRead.Value(enabled = false)
                else -> PreferenceRead.Failed("Embedding preference is invalid")
            }
        } catch (_: Exception) {
            PreferenceRead.Failed("Embedding preference is unreadable")
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
            Files.move(
                source.toPath(),
                destination.toPath(),
                StandardCopyOption.REPLACE_EXISTING,
            )
        }
    }

    private sealed interface PreferenceRead {
        object Missing : PreferenceRead
        data class Value(val enabled: Boolean) : PreferenceRead
        data class Failed(val message: String) : PreferenceRead
    }

    companion object {
        private const val CANONICAL_RELATIVE_PATH = "zara/preferences/local-embedding.bin"
        private const val LEGACY_RELATIVE_PATH = "local-embedding.bin"

        fun create(context: Context): LocalEmbeddingPreferenceStore {
            val applicationContext = context.applicationContext ?: context
            return forApplicationFiles(applicationContext.filesDir)
        }

        internal fun forApplicationFiles(filesDirectory: File): LocalEmbeddingPreferenceStore =
            LocalEmbeddingPreferenceStore(
                file = File(filesDirectory, CANONICAL_RELATIVE_PATH),
                legacyFile = File(filesDirectory, LEGACY_RELATIVE_PATH),
            )
    }
}
