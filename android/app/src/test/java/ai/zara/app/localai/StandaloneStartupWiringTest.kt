package ai.zara.app.localai

import java.io.File
import org.junit.Assert.*
import org.junit.Test

class StandaloneStartupWiringTest {
    @Test
    fun localPreferencesAreLoadedBeforeRemoteRestoration() {
        val session = File("src/main/java/ai/zara/app/AndroidAppSession.kt").readText()
        assertTrue(session.indexOf("RuntimeModePreferenceStore(") < session.indexOf("init {"))
        assertTrue(session.contains("if (!RuntimeStartupPolicy(runtimeMode, executionPolicy).restoreRemote) return"))
        assertTrue(session.contains("RuntimeStartupPolicy(runtimeMode, executionPolicy).loadLocalModel"))
        val service = File("src/main/java/ai/zara/app/localai/LocalAiService.kt").readText()
        val startup = service.substringAfter("override fun onCreate()").substringBefore("override fun onBind")
        assertFalse(startup.contains("loadActiveModel()"))
        assertTrue(startup.contains("tts.initialize()"))
    }

    @Test
    fun menuExposesPolicyAndOllamaWithoutRequiringSlashCommands() {
        val ui = File("src/main/java/ai/zara/app/ui/ZaraApp.kt").readText()
        val activity = File("src/main/java/ai/zara/app/MainActivity.kt").readText()
        assertTrue(ui.contains("Pure symbolic · experts and Prolog only"))
        assertTrue(ui.contains("Ollama on this device"))
        assertTrue(ui.contains("onSelectLocalModel("))
        assertTrue(activity.contains("appSession.selectLocalModel(selection)"))
        assertTrue(activity.contains("onOpenLocalModelApp ="))
        assertTrue(ui.contains("Open Zara LLM Serve"))
        assertTrue(activity.contains("appSession.setExecutionPolicy(requestedPolicy)"))
        val manifest = File("src/main/AndroidManifest.xml").readText()
        assertTrue(manifest.contains("@xml/local_network_security"))
        assertTrue(manifest.contains("android.intent.action.TTS_SERVICE"))
        val security = File("src/main/res/xml/local_network_security.xml").readText()
        assertTrue(security.contains("<base-config cleartextTrafficPermitted=\"false\""))
        assertTrue(security.contains("<domain>127.0.0.1</domain>"))
    }
}
