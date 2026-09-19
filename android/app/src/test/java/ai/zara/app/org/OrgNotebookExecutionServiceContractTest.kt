package ai.zara.app.org

import java.io.File
import org.junit.Assert.assertFalse
import org.junit.Assert.assertTrue
import org.junit.Test

class OrgNotebookExecutionServiceContractTest {
    @Test
    fun serviceIsSignatureProtectedAndStaysInCanonicalAppProcess() {
        val manifest = File("src/main/AndroidManifest.xml").readText()

        assertTrue(manifest.contains("android:name=\"ai.zara.permission.EXECUTE_NOTEBOOK\""))
        assertTrue(manifest.contains("android:protectionLevel=\"signature\""))
        assertTrue(manifest.contains("android:name=\".org.OrgNotebookExecutionService\""))
        assertTrue(manifest.contains("android:permission=\"ai.zara.permission.EXECUTE_NOTEBOOK\""))
        assertFalse(manifest.contains("android:process=\":voice\""))
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
}
