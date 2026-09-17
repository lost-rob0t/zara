package ai.zara.app.org

import ai.zara.org.core.OrgTangler
import java.io.File

/**
 * Applies trusted literate Org config into Zara's configured runtime directory.
 * Org remains the source; generated Python/Prolog are derived files.
 */
class LiterateOrgConfig(private val outputRoot: File) {
    init {
        outputRoot.mkdirs()
        require(outputRoot.isDirectory) { "Config output root is not a directory" }
    }

    fun tangle(source: String, sourceName: String = "config.org"): List<File> {
        val root = outputRoot.canonicalFile
        return OrgTangler.tangle(source, sourceName).outputs.map { output ->
            val destination = File(root, output.path).canonicalFile
            require(destination.toPath().startsWith(root.toPath())) { "Tangle escaped config root" }
            destination.parentFile?.mkdirs()
            destination.writeText(output.content)
            destination
        }
    }
}
