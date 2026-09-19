package ai.zara.app.org

import java.io.File
import org.junit.Assert.assertEquals
import org.junit.Assert.assertFalse
import org.junit.Assert.assertTrue
import org.junit.Test

class OrgNotebookExecutionServiceContractTest {
    @Test
    fun serviceIsSignatureProtectedAndSharesCanonicalRuntimeProcess() {
        val manifest = File("src/main/AndroidManifest.xml").readText()

        assertTrue(manifest.contains("android:name=\"ai.zara.permission.EXECUTE_NOTEBOOK\""))
        assertTrue(manifest.contains("android:protectionLevel=\"signature\""))
        assertTrue(manifest.contains("android:name=\".org.OrgNotebookExecutionService\""))
        assertTrue(manifest.contains("android:permission=\"ai.zara.permission.EXECUTE_NOTEBOOK\""))

        val mainActivityProcess = componentProcess(manifest, ".MainActivity")
        val notebookServiceProcess = componentProcess(manifest, ".org.OrgNotebookExecutionService")
        assertEquals(mainActivityProcess, notebookServiceProcess)
        assertEquals(":voice", notebookServiceProcess)
    }

    @Test
    fun serviceDelegatesToSingletonAppSessionInsteadOfCreatingRuntime() {
        val source = File("src/main/java/ai/zara/app/org/OrgNotebookExecutionService.kt").readText()

        assertTrue(source.contains("(application as ZaraApplication).appSession"))
        assertTrue(source.contains("session.queryLocalProlog(body)"))
        assertFalse(source.contains("AndroidAppSession("))
        assertFalse(source.contains("LocalZaraServer("))
        assertFalse(source.contains("NativeTreallaBridge("))
        assertFalse(source.contains("TreallaBridge("))
    }

    @Test
    fun unsupportedLanguagesFailClosedInsteadOfFallingBackToShell() {
        val source = File("src/main/java/ai/zara/app/org/OrgNotebookExecutionService.kt").readText()

        assertTrue(source.contains("language != \"prolog\""))
        assertTrue(source.contains("No trusted \$language notebook provider is available"))
        assertFalse(source.contains("Runtime.getRuntime"))
        assertFalse(source.contains("ProcessBuilder("))
    }

    private fun componentProcess(manifest: String, componentName: String): String? {
        val marker = "android:name=\"$componentName\""
        val start = manifest.indexOf(marker)
        require(start >= 0) { "Missing manifest component $componentName" }
        val componentEnd = manifest.indexOf('>', start).let { end ->
            require(end >= 0) { "Unterminated manifest component $componentName" }
            end
        }
        val declaration = manifest.substring(start, componentEnd)
        return Regex("android:process=\"([^\"]+)\"")
            .find(declaration)
            ?.groupValues
            ?.get(1)
    }
}
