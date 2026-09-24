package ai.zara.app.assistant

import java.io.File
import org.junit.Assert.assertFalse
import org.junit.Assert.assertTrue
import org.junit.Test

class AssistantServiceManifestContractTest {
    @Test
    fun `manifest exposes voice interaction and recognition service contracts`() {
        val manifest = File("src/main/AndroidManifest.xml").readText()

        assertTrue(manifest.contains("android.service.voice.VoiceInteractionService"))
        assertTrue(manifest.contains("android.permission.BIND_VOICE_INTERACTION"))
        assertTrue(manifest.contains("android:name=\"android.voice_interaction\""))
        assertTrue(manifest.contains("android:resource=\"@xml/voice_interaction_service\""))
        assertTrue(manifest.contains("android:name=\".assistant.ZaraVoiceInteractionSessionService\""))
        assertTrue(manifest.contains("android:name=\".assistant.ZaraRecognitionService\""))
        assertTrue(manifest.contains("android:permission=\"android.permission.BIND_SPEECH_RECOGNITION_SERVICE\""))
        assertTrue(manifest.contains("android.speech.RecognitionService"))
        assertTrue(manifest.contains("android:name=\"android.speech\""))
        assertTrue(manifest.contains("android:resource=\"@xml/recognition_service\""))
        assertTrue(manifest.contains("android:process=\":voice\""))
    }

    @Test
    fun `voice interaction metadata satisfies assistant role qualification`() {
        val metadata = File("src/main/res/xml/voice_interaction_service.xml").readText()

        assertTrue(metadata.contains("android:sessionService=\"ai.zara.app.assistant.ZaraVoiceInteractionSessionService\""))
        assertTrue(metadata.contains("android:recognitionService=\"ai.zara.app.assistant.ZaraRecognitionService\""))
        assertTrue(metadata.contains("android:supportsAssist=\"true\""))
    }

    @Test
    fun `assistant recognizer stays hidden as a manual default and delegates away from Zara`() {
        val metadata = File("src/main/res/xml/recognition_service.xml").readText()
        val service = File(
            "src/main/java/ai/zara/app/assistant/ZaraRecognitionService.kt"
        ).readText()

        assertTrue(metadata.contains("android:selectableAsDefault=\"false\""))
        assertTrue(service.contains("SpeechRecognizer.isOnDeviceRecognitionAvailable"))
        assertTrue(service.contains("SpeechRecognizer.createOnDeviceSpeechRecognizer"))
        assertTrue(service.contains("it.packageName != packageName"))
        assertTrue(service.contains("SpeechRecognizer.createSpeechRecognizer"))
    }

    @Test
    fun `ordinary UI role onboarding uses assistant settings and active service verification`() {
        val activity = File("src/main/java/ai/zara/app/MainActivity.kt").readText()
        val platform = File(
            "src/main/java/ai/zara/app/assistant/AndroidAssistantRolePlatform.kt"
        ).readText()

        assertTrue(activity.contains("ActivityResultContracts.StartActivityForResult"))
        assertTrue(activity.contains("appSession.completeAssistantRoleRequest()"))
        assertTrue(activity.contains("appSession.assistantRoleRequestIntent()"))
        assertTrue(activity.contains("onRequestAssistantRole"))
        assertTrue(platform.contains("roleManager.createRequestRoleIntent(RoleManager.ROLE_ASSISTANT)"))
        assertTrue(platform.contains("VoiceInteractionService.isActiveService"))
        assertFalse(platform.contains("Settings.ACTION_VOICE_INPUT_SETTINGS"))
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
    fun `voice session uses explicit overlay PTT instead of opening microphone on show`() {
        val session = File("src/main/java/ai/zara/app/assistant/ZaraVoiceInteractionSession.kt").readText()

        assertTrue(session.contains("(context.applicationContext as ZaraApplication).appSession"))
        assertTrue(session.contains("override fun onCreateContentView()"))
        assertTrue(session.contains("MotionEvent.ACTION_DOWN"))
        assertTrue(session.contains("MotionEvent.ACTION_UP"))
        assertTrue(session.contains("MotionEvent.ACTION_CANCEL"))
        assertTrue(session.contains("appSession.startAssistantVoice"))
        assertTrue(session.contains("appSession.releasePushToTalk"))
        assertTrue(session.contains("appSession.cancelPushToTalk"))
        val showBody = session.substringAfter("override fun onShow").substringBefore("override fun onHide")
        assertFalse(showBody.contains("appSession.startAssistantVoice"))
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
