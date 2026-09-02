package ai.zara.app.prolog

import android.content.res.AssetManager
import java.io.File
import java.io.InputStream
import java.nio.file.AtomicMoveNotSupportedException
import java.nio.file.Files
import java.nio.file.StandardCopyOption
import java.security.MessageDigest

fun interface PortableSemanticAssetSource {
    fun open(path: String): InputStream
}

class AndroidPortableSemanticAssetSource(
    private val assetManager: AssetManager
) : PortableSemanticAssetSource {
    override fun open(path: String): InputStream = assetManager.open(path, AssetManager.ACCESS_STREAMING)
}

data class StagedPortableSemanticAssets(
    val coreFile: File,
    val resources: Map<String, File>
)

class PortableSemanticAssetStager(
    private val destinationRoot: File,
    private val coreAssetPath: String = PortableSemanticCore.coreAssetPath
) {
    fun stage(source: PortableSemanticAssetSource): File {
        validateAssetPath(coreAssetPath)

        val root = destinationRoot.canonicalFile
        val destinationDirectory = File(root, "portable-semantic/ZARA-SEMANTIC-1").canonicalFile
        requireInsideRoot(root, destinationDirectory, "Portable semantic staging directory escaped its private root")
        check(destinationDirectory.mkdirs() || destinationDirectory.isDirectory) {
            "Portable semantic staging directory is unavailable"
        }

        val destination = File(destinationDirectory, "semantic_core.pl").canonicalFile
        requireInsideRoot(root, destination, "Portable semantic core destination escaped its private root")
        writeAtomically(source, coreAssetPath, destination)

        check(destination.isFile) { "Portable semantic core staging produced no file" }
        return destination
    }

    fun stageAll(source: PortableSemanticAssetSource): StagedPortableSemanticAssets {
        val resources = PortableSemanticCore.resources
        require(resources.isNotEmpty()) { "Portable semantic resource set is empty" }
        resources.forEach(::validateAssetPath)

        val bytes = linkedMapOf<String, ByteArray>()
        try {
            resources.forEach { path ->
                bytes[path] = source.open(path).use(InputStream::readBytes)
            }
        } catch (error: Throwable) {
            throw IllegalStateException("Portable semantic resource staging failed", error)
        }

        val root = destinationRoot.canonicalFile
        val setsRoot = File(root, "portable-semantic/ZARA-SEMANTIC-1/sets").canonicalFile
        requireInsideRoot(root, setsRoot, "Portable semantic staging directory escaped its private root")
        check(setsRoot.mkdirs() || setsRoot.isDirectory) {
            "Portable semantic staging directory is unavailable"
        }

        val setDirectory = File(setsRoot, resourceDigest(bytes)).canonicalFile
        requireInsideRoot(root, setDirectory, "Portable semantic resource set escaped its private root")
        if (!setDirectory.isDirectory) {
            stageCompleteSet(root, setsRoot, setDirectory, bytes)
        }

        val staged = resources.associateWith { path ->
            File(setDirectory, path).canonicalFile.also { file ->
                requireInsideRoot(root, file, "Portable semantic resource escaped its private root")
                check(file.isFile) { "Portable semantic resource staging produced no file" }
            }
        }
        val coreFile = staged[coreAssetPath]
            ?: throw IllegalStateException("Portable semantic core is not declared in the staged resource set")
        return StagedPortableSemanticAssets(coreFile = coreFile, resources = staged)
    }

    private fun stageCompleteSet(
        root: File,
        setsRoot: File,
        destination: File,
        resources: Map<String, ByteArray>
    ) {
        val temporary = Files.createTempDirectory(setsRoot.toPath(), ".staging-").toFile().canonicalFile
        requireInsideRoot(root, temporary, "Portable semantic temporary set escaped its private root")
        try {
            resources.forEach { (path, content) ->
                val file = File(temporary, path).canonicalFile
                requireInsideRoot(temporary, file, "Portable semantic resource escaped its staging set")
                check(file.parentFile.mkdirs() || file.parentFile.isDirectory) {
                    "Portable semantic resource directory is unavailable"
                }
                file.writeBytes(content)
            }
            moveDirectory(temporary, destination)
        } catch (error: Throwable) {
            temporary.deleteRecursively()
            if (!destination.isDirectory) {
                throw IllegalStateException("Portable semantic resource staging failed", error)
            }
        }
    }

    private fun resourceDigest(resources: Map<String, ByteArray>): String {
        val digest = MessageDigest.getInstance("SHA-256")
        resources.forEach { (path, content) ->
            digest.update(path.encodeToByteArray())
            digest.update(0.toByte())
            digest.update(content)
            digest.update(0.toByte())
        }
        return digest.digest().joinToString("") { byte ->
            "%02x".format(byte.toInt() and 0xff)
        }
    }

    private fun writeAtomically(
        source: PortableSemanticAssetSource,
        assetPath: String,
        destination: File
    ) {
        val temporary = File.createTempFile("semantic_core.", ".tmp", destination.parentFile)
        try {
            source.open(assetPath).use { input ->
                temporary.outputStream().use { output -> input.copyTo(output) }
            }
            replaceAtomically(temporary, destination)
        } catch (error: Throwable) {
            temporary.delete()
            throw IllegalStateException("Portable semantic core staging failed", error)
        }
    }

    private fun validateAssetPath(path: String) {
        require(path.isNotBlank()) { "Portable semantic asset path is required" }
        require(!path.startsWith('/')) { "Portable semantic asset path must be relative" }
        require(path.split('/').none { it.isBlank() || it == "." || it == ".." }) {
            "Portable semantic asset path is invalid"
        }
    }

    private fun requireInsideRoot(root: File, file: File, message: String) {
        require(file.path.startsWith(root.path + File.separator)) { message }
    }

    private fun replaceAtomically(source: File, destination: File) {
        try {
            Files.move(
                source.toPath(),
                destination.toPath(),
                StandardCopyOption.ATOMIC_MOVE,
                StandardCopyOption.REPLACE_EXISTING
            )
        } catch (_: AtomicMoveNotSupportedException) {
            Files.move(source.toPath(), destination.toPath(), StandardCopyOption.REPLACE_EXISTING)
        }
    }

    private fun moveDirectory(source: File, destination: File) {
        try {
            Files.move(source.toPath(), destination.toPath(), StandardCopyOption.ATOMIC_MOVE)
        } catch (_: AtomicMoveNotSupportedException) {
            Files.move(source.toPath(), destination.toPath())
        }
    }
}
