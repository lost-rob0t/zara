package ai.zara.app.assistant

import java.io.File
import org.junit.Assert.assertFalse
import org.junit.Assert.assertTrue
import org.junit.Test

class LocalAssistantVoiceContractTest {
    @Test
    fun `strict local assistant uses the Android on-device recognizer only`() {
        val source = File(
            "src/main/java/ai/zara/app/assistant/LocalAssistantVoiceController.kt"
        ).readText()

        assertTrue(source.contains("SpeechRecognizer.createOnDeviceSpeechRecognizer"))
        assertTrue(source.contains("SpeechRecognizer.isOnDeviceRecognitionAvailable"))
        assertTrue(source.contains("RecognizerIntent.EXTRA_PREFER_OFFLINE"))
        assertFalse(source.contains("SpeechRecognizer.createSpeechRecognizer("))
    }

    @Test
    fun `local spoken replies refuse network-required TTS voices`() {
        val source = File(
            "src/main/java/ai/zara/app/assistant/LocalAssistantVoiceController.kt"
        ).readText()

        assertTrue(source.contains("!it.isNetworkConnectionRequired"))
    }

    @Test
    fun `assistant session routes local mode before remote voice startup`() {
        val source = File(
            "src/main/java/ai/zara/app/assistant/ZaraVoiceInteractionSession.kt"
        ).readText()

        assertTrue(source.contains("planAssistantCapture("))
        assertTrue(source.contains("AssistantCapturePlan.Local -> beginLocalPushToTalk"))
        assertTrue(source.contains("LocalAssistantVoiceController"))
        assertTrue(source.contains("appSession.startAssistantVoice"))
    }
}
