package ai.zara.app.assistant

import java.io.File
import org.junit.Assert.assertFalse
import org.junit.Assert.assertTrue
import org.junit.Test

class AppScopedAssistantRuntimeContractTest {
    @Test
    fun `ordinary UI destruction cannot own or close the assistant runtime`() {
        val application = projectFile("app/src/main/java/ai/zara/app/ZaraApplication.kt").readText()
        val activity = projectFile("app/src/main/java/ai/zara/app/MainActivity.kt").readText()
        val assistant = projectFile(
            "app/src/main/java/ai/zara/app/assistant/ZaraVoiceInteractionSession.kt"
        ).readText()

        assertTrue(application.contains("val appSession: AndroidAppSession by lazy"))
        assertTrue(activity.contains("appSession = (application as ZaraApplication).appSession"))
        assertTrue(assistant.contains("(context.applicationContext as ZaraApplication).appSession"))

        val destroyBody = activity.substringAfter("override fun onDestroy() {")
            .substringBefore("private fun reconcileMicrophonePermission")
        assertTrue(destroyBody.contains("appSession.setStateObserver(null)"))
        assertTrue(destroyBody.contains("appSession.setVoiceStreamObserver(null)"))
        assertFalse(destroyBody.contains("appSession.close("))
        assertFalse(destroyBody.contains("appSession.disconnect("))
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
