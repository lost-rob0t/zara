package ai.zara.app.assistant

import java.io.File
import java.util.concurrent.CompletableFuture
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
    fun `assistant session auto starts on-device voice for local routes and keeps remote explicit`() {
        val source = File(
            "src/main/java/ai/zara/app/assistant/ZaraVoiceInteractionSession.kt"
        ).readText()
        val onShow = source.substringAfter("override fun onShow").substringBefore("override fun onHide")
        val automatic = source.substringAfter("private fun beginAutomaticCapture()")
            .substringBefore("override fun onHide")

        assertTrue(onShow.contains("beginAutomaticCapture()"))
        assertTrue(automatic.contains("planAssistantCapture("))
        assertTrue(automatic.contains("AssistantCapturePlan.Local ->"))
        assertTrue(automatic.contains("localVoice.start(permissionGranted)"))
        assertTrue(automatic.contains("AssistantCapturePlan.Remote -> updateStatus(\"Hold to talk to Zara\")"))
        assertFalse(automatic.contains("appSession.startAssistantVoice"))
        assertTrue(source.contains("appSession.startAssistantVoice"))
    }

    @Test
    fun `local capture remains pinned to local submission after recognition`() {
        val source = File(
            "src/main/java/ai/zara/app/assistant/LocalAssistantVoiceController.kt"
        ).readText()

        assertTrue(source.contains("appSession.submitText(transcript)"))
        assertFalse(source.contains("appSession.submitLocalText(transcript)"))
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
    fun `pending local assistant turn is actively cancelled`() {
        val pending = PendingLocalTurn()
        val turn = CompletableFuture<String>()

        pending.track(turn)
        assertTrue(pending.isCurrent(turn))
        pending.cancel()

        assertTrue(turn.isCancelled)
        assertFalse(pending.isCurrent(turn))
    }

    @Test
    fun `starting a replacement local turn cancels and fences the superseded future`() {
        val pending = PendingLocalTurn()
        val first = CompletableFuture<String>()
        val replacement = CompletableFuture<String>()

        pending.track(first)
        pending.track(replacement)

        assertTrue(first.isCancelled)
        assertFalse(pending.isCurrent(first))
        assertTrue(pending.isCurrent(replacement))
        assertFalse(replacement.isDone)
        pending.cancel()
        assertTrue(replacement.isCancelled)
    }

    @Test
    fun `clearing a completed turn removes callback authority without cancelling it`() {
        val pending = PendingLocalTurn()
        val turn = CompletableFuture.completedFuture("done")

        pending.track(turn)
        assertTrue(pending.isCurrent(turn))
        pending.clear(turn)
        pending.cancel()

        assertTrue(turn.isDone)
        assertFalse(turn.isCancelled)
        assertFalse(pending.isCurrent(turn))
    }

    @Test
    fun `local callback checks pending turn authority before publishing`() {
        val source = File(
            "src/main/java/ai/zara/app/assistant/LocalAssistantVoiceController.kt"
        ).readText()
        val callback = source
            .substringAfter("turn.whenComplete { result, error ->")
            .substringBefore("override fun onPartialResults")

        assertTrue(callback.contains("if (!pendingTurn.isCurrent(turn)) return@execute"))
        assertTrue(callback.contains("pendingTurn.clear(turn)"))
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
    fun `released local completion is fenced immediately by lifecycle generation`() {
        val controller = File(
            "src/main/java/ai/zara/app/assistant/LocalAssistantVoiceController.kt"
        ).readText()
        val session = File(
            "src/main/java/ai/zara/app/assistant/ZaraVoiceInteractionSession.kt"
        ).readText()
        val normalizedSession = session.replace(Regex("\\s+"), " ")

        assertTrue(controller.contains("private val lifecycleFence: AssistantLifecycleFence"))
        assertTrue(controller.contains("val lifecycleToken = lifecycleFence.beginStart()"))
        assertTrue(controller.contains("lifecycleFence.isCurrent(lifecycleToken)"))
        assertTrue(
            normalizedSession.contains(
                "LocalAssistantVoiceController( context, appSession, lifecycleFence,"
            )
        )
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
