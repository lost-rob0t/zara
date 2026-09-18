package ai.zara.app.assistant

import java.io.File
import org.junit.Assert.assertEquals
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

    @Test
    fun `local capture remains pinned to local submission after recognition`() {
        val source = File(
            "src/main/java/ai/zara/app/assistant/LocalAssistantVoiceController.kt"
        ).readText()

        assertTrue(source.contains("appSession.submitLocalText(transcript)"))
        assertFalse(source.contains("appSession.submitText(transcript)"))
    }

    @Test
    fun `hiding assistant session fences pending local completion`() {
        val source = File(
            "src/main/java/ai/zara/app/assistant/ZaraVoiceInteractionSession.kt"
        ).readText()
        val onHide = source.substringAfter("override fun onHide() {").substringBefore("super.onHide()")

        assertTrue(onHide.contains("localVoice.cancel(notify = false)"))
    }

    @Test
    fun `assistant lifecycle invalidation notifies registered local cancellation listener`() {
        val fence = AssistantLifecycleFence()
        var invalidations = 0
        val registration = fence.onInvalidate { invalidations += 1 }

        fence.invalidate()
        registration.close()
        fence.invalidate()

        assertEquals(1, invalidations)
    }

    @Test
    fun `assistant lifecycle invalidation reaches every listener when one fails`() {
        val fence = AssistantLifecycleFence()
        val observed = mutableListOf<String>()
        fence.onInvalidate {
            observed += "first"
            error("first cancellation failed")
        }
        fence.onInvalidate { observed += "second" }

        var failure: Throwable? = null
        try {
            fence.invalidate()
        } catch (error: Throwable) {
            failure = error
        }

        assertEquals(listOf("first", "second"), observed)
        assertEquals("first cancellation failed", failure?.message)
    }

    @Test
    fun `assistant shutdown is wired to active local voice cancellation`() {
        val source = File(
            "src/main/java/ai/zara/app/assistant/ZaraVoiceInteractionSession.kt"
        ).readText()

        assertTrue(source.contains("lifecycleFence.onInvalidate"))
        assertTrue(source.contains("cancelLocalCaptureForLifecycleInvalidation"))
    }

    @Test
    fun `assistant shutdown fences released local transcription and model completion`() {
        val source = File(
            "src/main/java/ai/zara/app/assistant/ZaraVoiceInteractionSession.kt"
        ).readText()
        val cancellation = source
            .substringAfter("private fun cancelLocalCaptureForLifecycleInvalidation() {")
            .substringBefore("private fun updateStatus")

        assertTrue(cancellation.contains("localVoice.cancel(notify = false)"))
        assertFalse(cancellation.contains("return"))
    }
}
