package ai.zara.app.prolog

import android.content.res.AssetManager
import java.io.File
import java.io.InputStream
import java.nio.file.AtomicMoveNotSupportedException
import java.nio.file.Files
import java.nio.file.StandardCopyOption

fun interface PortableSemanticAssetSource {
    fun open(path: String): InputStream
}

class AndroidPortableSemanticAssetSource(
    private val assetManager: AssetManager
) : PortableSemanticAssetSource {
    override fun open(path: String): InputStream = assetManager.open(path, AssetManager.ACCESS_STREAMING)
}

class PortableSemanticAssetStager(
    private val destinationRoot: File,
    private val coreAssetPath: String = PortableSemanticCore.coreAssetPath
) {
    fun stage(source: PortableSemanticAssetSource): File {
        validateAssetPath(coreAssetPath)

        val root = destinationRoot.canonicalFile
        val destinationDirectory = File(root, "portable-semantic/ZARA-SEMANTIC-1").canonicalFile
        require(destinationDirectory.path.startsWith(root.path + File.separator)) {
            "Portable semantic staging directory escaped its private root"
        }
        check(destinationDirectory.mkdirs() || destinationDirectory.isDirectory) {
            "Portable semantic staging directory is unavailable"
        }

        val destination = File(destinationDirectory, "semantic_core.pl").canonicalFile
        require(destination.path.startsWith(root.path + File.separator)) {
            "Portable semantic core destination escaped its private root"
        }

        val temporary = File.createTempFile("semantic_core.", ".tmp", destinationDirectory)
        try {
            source.open(coreAssetPath).use { input ->
                temporary.outputStream().use { output ->
                    input.copyTo(output)
                }
            }
            replaceAtomically(temporary, destination)
        } catch (error: Throwable) {
            temporary.delete()
            throw IllegalStateException("Portable semantic core staging failed", error)
        }

        check(destination.isFile) { "Portable semantic core staging produced no file" }
        return destination
    }

    private fun validateAssetPath(path: String) {
        require(path.isNotBlank()) { "Portable semantic core asset path is required" }
        require(!path.startsWith('/')) { "Portable semantic core asset path must be relative" }
        require(path.split('/').none { it.isBlank() || it == "." || it == ".." }) {
            "Portable semantic core asset path is invalid"
        }
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
            Files.move(
                source.toPath(),
                destination.toPath(),
                StandardCopyOption.REPLACE_EXISTING
            )
        }
    }
}
