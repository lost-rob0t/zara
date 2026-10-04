package ai.zara.app.ui

import ai.zara.app.prolog.LocalEmbeddingConfiguration
import java.io.File
import java.nio.file.Files
import org.junit.Test

/** File metadata and cleanup failures must not escape into Compose/main-thread callers. */
class LocalEmbeddingPreferenceFailureBoundaryTest {
    @Test
    fun deniedAbsolutePathLookupReturnsTypedSaveFailure() {
        val file = object : File("denied-preference") {
            override fun getAbsoluteFile(): File = throw SecurityException("fixture")
        }
        check(store(file).save(LocalEmbeddingConfiguration(enabled = true)) is LocalEmbeddingPreferenceSaveResult.Failed)
    }

    @Test
    fun deniedDirectoryCreationReturnsTypedSaveFailure() {
        val parent = object : File("denied-parent") {
            override fun isDirectory(): Boolean = false
            override fun mkdirs(): Boolean = throw SecurityException("fixture")
        }
        val file = withParent(File(parent, "preference.bin"), parent)
        check(store(file).save(LocalEmbeddingConfiguration(enabled = true)) is LocalEmbeddingPreferenceSaveResult.Failed)
    }

    @Test
    fun deniedReadMetadataReturnsWarningInsteadOfThrowing() {
        for (operation in listOf("exists", "isFile", "length")) {
            val file = object : File("denied-preference") {
                override fun exists(): Boolean {
                    if (operation == "exists") throw SecurityException("fixture")
                    return true
                }
                override fun isFile(): Boolean {
                    if (operation == "isFile") throw SecurityException("fixture")
                    return true
                }
                override fun length(): Long = throw SecurityException("fixture")
            }
            check(store(file).load().warning != null) { "Missing warning for $operation" }
        }
    }

    @Test
    fun deniedDirectoryCanRecoverWithoutRecreatingStore() = withRoot { root ->
        var denied = true
        val parent = object : File(root, "preferences") {
            override fun isDirectory(): Boolean {
                if (denied) throw SecurityException("fixture")
                return super.isDirectory()
            }
        }
        val file = withParent(File(parent, "local-embedding.bin"), parent)
        val preferences = store(file)
        check(preferences.save(LocalEmbeddingConfiguration(enabled = true)) is LocalEmbeddingPreferenceSaveResult.Failed)
        check(!file.exists())
        denied = false
        check(preferences.save(LocalEmbeddingConfiguration(enabled = true)) == LocalEmbeddingPreferenceSaveResult.Saved)
        check(preferences.load().configuration.enabled)
        check(preferences.save(LocalEmbeddingConfiguration(enabled = false)) == LocalEmbeddingPreferenceSaveResult.Saved)
        check(!store(File(file.path)).load().configuration.enabled)
    }

    @Test
    fun deniedMigrationWriteRetainsEnabledLegacyPreference() = withRoot { root ->
        val legacy = File(root, "local-embedding.bin").apply { writeText("enabled") }
        val parent = object : File(root, "preferences") {
            override fun isDirectory(): Boolean = throw SecurityException("fixture")
        }
        val canonical = withParent(File(parent, "local-embedding.bin"), parent)
        val result = store(canonical, legacy).load()
        check(result.configuration.enabled)
        check(result.warning != null)
        check(legacy.readText() == "enabled")
        check(!canonical.exists())
    }

    @Test
    fun deniedLegacyCleanupRetainsCommittedCanonicalPreference() = withRoot { root ->
        val legacy = object : File(root, "local-embedding.bin") {
            override fun delete(): Boolean = throw SecurityException("fixture")
        }.apply { writeText("enabled") }
        val canonical = File(root, "preferences/local-embedding.bin")
        val result = store(canonical, legacy).load()
        check(result.configuration.enabled)
        check(result.warning != null) { "Cleanup failure must stay visible" }
        check(canonical.readText() == "enabled")
        // Later canonical writes win; a leftover migration source cannot override them.
        val preferences = store(canonical, legacy)
        check(preferences.save(LocalEmbeddingConfiguration(enabled = false)) == LocalEmbeddingPreferenceSaveResult.Saved)
        check(!preferences.load().configuration.enabled)
    }

    private fun store(file: File, legacy: File? = null): LocalEmbeddingPreferenceStore {
        val constructor = LocalEmbeddingPreferenceStore::class.java.getDeclaredConstructor(File::class.java, File::class.java)
        constructor.isAccessible = true
        return constructor.newInstance(file, legacy)
    }

    private fun withParent(file: File, directory: File): File = object : File(file.path) {
        override fun getAbsoluteFile(): File = this
        override fun getParentFile(): File = directory
    }

    private fun withRoot(block: (File) -> Unit) {
        val root = Files.createTempDirectory("embedding-failure-boundary").toFile()
        try {
            block(root)
        } finally {
            check(root.deleteRecursively())
        }
    }
}
