package ai.zara.app.assistant

import java.io.File
import org.junit.Assert.assertFalse
import org.junit.Assert.assertTrue
import org.junit.Test

class AssistantServiceManifestContractTest {
    @Test
    fun `manifest exposes only the public voice interaction service contract`() {
        val manifest = File("src/main/AndroidManifest.xml").readText()

        assertTrue(manifest.contains("android.service.voice.VoiceInteractionService"))
        assertTrue(manifest.contains("android.permission.BIND_VOICE_INTERACTION"))
        assertTrue(manifest.contains("android:name=\"android.voice_interaction\""))
        assertTrue(manifest.contains("android:resource=\"@xml/voice_interaction_service\""))
        assertTrue(manifest.contains("android:name=\".assistant.ZaraVoiceInteractionSessionService\""))
        assertTrue(manifest.contains("android:process=\":voice\""))
    }

    @Test
    fun `voice interaction metadata binds Zara session service and assist support`() {
        val metadata = File("src/main/res/xml/voice_interaction_service.xml").readText()

        assertTrue(metadata.contains("android:sessionService=\"ai.zara.app.assistant.ZaraVoiceInteractionSessionService\""))
        assertTrue(metadata.contains("android:supportsAssist=\"true\""))
    }

    @Test
    fun `ordinary UI role onboarding uses explicit activity result and platform recheck`() {
        val activity = File("src/main/java/ai/zara/app/MainActivity.kt").readText()

        assertTrue(activity.contains("ActivityResultContracts.StartActivityForResult"))
        assertTrue(activity.contains("appSession.completeAssistantRoleRequest()"))
        assertTrue(activity.contains("appSession.assistantRoleRequestIntent()"))
        assertTrue(activity.contains("onRequestAssistantRole"))
    }

    @Test
    fun `ordinary UI rechecks assistant role whenever it resumes`() {
        val activity = File("src/main/java/ai/zara/app/MainActivity.kt").readText()

        assertTrue(activity.contains("override fun onResume()"))
        assertTrue(activity.contains("appSession.assessAssistantRole()"))
    }

    @Test
    fun `ui and voice services share one application scoped runtime process`() {
        val manifest = File("src/main/AndroidManifest.xml").readText()
        val activity = File("src/main/java/ai/zara/app/MainActivity.kt").readText()

        assertTrue(manifest.contains("android:name=\".ZaraApplication\""))
        assertTrue(manifest.contains("android:name=\".MainActivity\""))
        assertTrue(manifest.contains("android:process=\":voice\""))
        assertTrue(activity.contains("(application as ZaraApplication).appSession"))
        assertFalse(activity.contains("AndroidAppSession(this)"))
        assertFalse(activity.contains("appSession.close()"))
    }

    @Test
    fun `voice session reuses application runtime and cancels capture on hide`() {
        val session = File("src/main/java/ai/zara/app/assistant/ZaraVoiceInteractionSession.kt").readText()

        assertTrue(session.contains("(context.applicationContext as ZaraApplication).appSession"))
        assertTrue(session.contains("override fun onShow"))
        assertTrue(session.contains("appSession.startAssistantVoice"))
        assertTrue(session.contains("override fun onHide"))
        assertTrue(session.contains("appSession.cancelPushToTalk"))
    }

    @Test
    fun `voice interaction service rechecks role and stops capture on shutdown`() {
        val service = File("src/main/java/ai/zara/app/assistant/ZaraVoiceInteractionService.kt").readText()

        assertTrue(service.contains("(application as ZaraApplication).appSession"))
        assertTrue(service.contains("override fun onReady"))
        assertTrue(service.contains("appSession.assessAssistantRole()"))
        assertTrue(service.contains("override fun onShutdown"))
        assertTrue(service.contains("ManualVoiceState.Capturing"))
        assertTrue(service.contains("appSession.cancelPushToTalk()"))
    }
}
