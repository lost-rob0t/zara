package ai.zara.app.ui

import java.io.File
import org.junit.Assert.assertTrue
import org.junit.Test

class CodeWorkbenchEmbeddingContractTest {
    @Test
    fun mainAppEmbedsSharedWorkbenchAndStandaloneCodeUsesSameSurface() {
        val settings = File("../settings.gradle.kts").readText()
        val app = File("src/main/java/ai/zara/app/ui/ZaraApp.kt").readText()
        val editor = File("../code-editor/src/main/java/ai/zara/code/editor/MainActivity.kt").readText()

        assertTrue(settings.contains("include(\":code-workbench\")"))
        assertTrue(app.contains("AppSurface.Code -> CodeWorkbenchSurface("))
        assertTrue(app.contains("platformVoiceEnabled = false"))
        assertTrue(editor.contains("CodeWorkbenchSurface("))
        assertTrue(editor.contains("themeTokens(ZaraTheme.Outrun"))
        assertTrue(editor.contains("platformVoiceEnabled = true"))
        val workbench = File("../code-workbench/src/main/java/ai/zara/code/workbench/CodeWorkbenchSurface.kt").readText()
        assertTrue(workbench.contains("PrologClient("))
        assertTrue(workbench.contains("PROLOG QUERY"))
    }
}
