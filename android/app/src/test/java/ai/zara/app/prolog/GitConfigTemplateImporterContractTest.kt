package ai.zara.app.prolog

import java.io.File
import org.junit.Assert.assertFalse
import org.junit.Assert.assertTrue
import org.junit.Test

class GitConfigTemplateImporterContractTest {
    private val source = File("src/main/java/ai/zara/app/prolog/GitConfigTemplateImporter.kt").readText()

    @Test
    fun `template transport is shallow https git without submodules`() {
        assertTrue(source.contains("uri.scheme == \"https\""))
        assertTrue(source.contains("uri.userInfo == null"))
        assertTrue(source.contains(".setDepth(1)"))
        assertTrue(source.contains(".setCloneSubmodules(false)"))
    }

    @Test
    fun `template import rejects path and symlink escape`() {
        assertTrue(source.contains("Files.isSymbolicLink"))
        assertTrue(source.contains("file.path.startsWith(root.path + File.separator)"))
        assertTrue(source.contains("declared == name"))
    }

    @Test
    fun `template prolog is data only`() {
        assertTrue(source.contains("ALLOWED_DIRECTIVE"))
        assertTrue(source.contains("DANGEROUS_PREDICATE"))
        assertTrue(source.contains("initialization"))
        assertTrue(source.contains("process_create"))
        assertTrue(source.contains("assertz"))
        assertTrue(source.contains("consult"))
    }

    @Test
    fun `import does not expose git credentials or shell`() {
        assertFalse(source.contains("ProcessBuilder"))
        assertFalse(source.contains("Runtime.getRuntime"))
        assertFalse(source.contains("setCredentialsProvider"))
    }
}
