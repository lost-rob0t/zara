package ai.zara.app.ui

import java.io.File
import org.junit.Assert.assertFalse
import org.junit.Assert.assertTrue
import org.junit.Test

class AndroidPrologStudioContractTest {
    @Test
    fun logicRouteIsRealStudioInsteadOfGatedPlaceholder() {
        val shell = File("src/main/java/ai/zara/app/ui/ZaraApp.kt").readText()
        val studio = File("src/main/java/ai/zara/app/ui/PrologStudioSurface.kt").readText()

        assertTrue(shell.contains("AppSurface.Logic -> PrologStudioSurface("))
        assertFalse(shell.contains("Logic(\"Logic\", \"#652\")"))
        listOf("IDE", "Expert", "Graph", "Learn").forEach { label ->
            assertTrue("missing studio pane $label", studio.contains("\"$label\""))
        }
        assertTrue(studio.contains("PrologVisualTransformation"))
        assertTrue(studio.contains("LogicGraphCanvas"))
        assertTrue(studio.contains("PrologTutorialCatalog.steps"))
    }

    @Test
    fun disconnectedChatUsesLocalServerAndRuntimeChoiceIsPersistent() {
        val session = File("src/main/java/ai/zara/app/AndroidAppSession.kt").readText()
        val activity = File("src/main/java/ai/zara/app/MainActivity.kt").readText()
        val submit = session.substringAfter("fun submitText(")
            .substringBefore("private fun submitLocalText")

        assertTrue(session.contains("LocalZaraServer("))
        assertTrue(submit.contains("RuntimeMode.Local -> return submitLocalText(text, localConversationId)"))
        assertTrue(submit.contains("RuntimeMode.Auto -> return submitAutoLocalFirst("))
        assertTrue(submit.contains("localConversationId = localConversationId"))
        assertTrue(submit.contains("remoteConversationId = remoteConversationId"))
        assertTrue(session.contains("private fun submitAutoLocalFirst("))
        assertTrue(submit.contains("RuntimeMode.Remote ->"))
        assertTrue(submit.contains("Remote mode requires an authenticated Zara server"))
        assertTrue(activity.contains("RuntimeModePreferenceStore"))
    }

    @Test
    fun updaterUsesPackageInstallerAndVerifiedReleaseMetadata() {
        val updater = File("src/main/java/ai/zara/app/update/AndroidUpdateManager.kt").readText()
        val manifest = File("src/main/AndroidManifest.xml").readText()

        assertTrue(updater.contains("UpdateSecurity.verifySha256"))
        assertTrue(updater.contains("PackageInstaller.SessionParams"))
        assertTrue(updater.contains("session.commit"))
        assertFalse(updater.contains("ACTION_INSTALL_PACKAGE"))
        assertTrue(manifest.contains("REQUEST_INSTALL_PACKAGES"))
        assertTrue(manifest.contains("UpdateInstallReceiver"))
    }
}
