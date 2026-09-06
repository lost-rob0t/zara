package ai.zara.app.security

import java.io.File
import org.junit.Assert.assertTrue
import org.junit.Test

class AndroidProductionLoggingContractTest {
    @Test
    fun `production Android source has no raw platform or console logging sinks`() {
        val sourceRoot = projectFile("app/src/main")
        assertTrue("Android production source root is required", sourceRoot.isDirectory)

        val forbidden = listOf(
            "android.util.Log",
            "Log.",
            "Timber.",
            "println(",
            "printStackTrace(",
            "System.out",
            "System.err",
            "printf(",
            "fprintf(",
        )
        val sourceFiles = sourceRoot.walkTopDown()
            .filter(File::isFile)
            .filter { file -> file.extension in setOf("kt", "java", "c", "cc", "cpp", "h", "hpp") }
            .toList()
        assertTrue("Android production source files are required", sourceFiles.isNotEmpty())

        val violations = sourceFiles.flatMap { file ->
            val text = file.readText()
            forbidden.filter(text::contains).map { token ->
                "${file.relativeTo(sourceRoot).invariantSeparatorsPath}: $token"
            }
        }

        assertTrue(
            "raw Android logging sinks can expose private text/audio/secrets: ${violations.joinToString()}",
            violations.isEmpty(),
        )
    }

    private fun projectFile(relativePath: String): File {
        val cwd = File(System.getProperty("user.dir"))
        val candidates = listOf(
            File(cwd, relativePath),
            File(cwd, "android/$relativePath"),
            File(cwd.parentFile ?: cwd, relativePath),
            File(cwd.parentFile ?: cwd, "android/$relativePath"),
        )
        return candidates.firstOrNull(File::exists) ?: candidates.first()
    }
}
