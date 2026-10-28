package ai.zara.app.localai

import java.util.concurrent.CompletableFuture
import org.junit.Assert.*
import org.junit.Test

class OfflineSpeechSessionTest {
    @Test fun missingVoiceCanBeRetriedAfterInstallation() {
        val first = FakeEngine(emptyList())
        val second = FakeEngine(listOf(voice()))
        val engines = ArrayDeque(listOf(first, second))
        val session = OfflineSpeechSession { CompletableFuture.completedFuture(engines.removeFirst()) }
        assertEquals(LocalTtsPhase.UNAVAILABLE, session.initialize().join().phase)
        assertEquals(LocalTtsPhase.READY, session.initialize().join().phase)
        assertTrue(first.closed)
    }

    @Test fun networkAndNotInstalledVoicesCannotBeUsed() {
        val engine = FakeEngine(listOf(voice(network = true), voice(installed = false)))
        val session = OfflineSpeechSession { CompletableFuture.completedFuture(engine) }
        assertEquals(LocalTtsPhase.UNAVAILABLE, session.initialize().join().phase)
        assertTrue(session.speak("hello").isCompletedExceptionally)
        assertEquals(0, engine.utterances.size)
    }

    @Test fun stopDuringInitializationFencesQueuedSpeech() {
        val initialization = CompletableFuture<OfflineSpeechEngine>()
        val session = OfflineSpeechSession { initialization }
        val utterance = session.speak("hello")
        session.stop()
        val engine = FakeEngine(listOf(voice()))
        initialization.complete(engine)
        assertTrue(utterance.isCompletedExceptionally)
        assertEquals(0, engine.utterances.size)
        assertEquals(LocalTtsPhase.READY, session.state().phase)
    }

    @Test fun replacementAndLateCallbacksCannotCompleteNewSpeech() {
        val engine = FakeEngine(listOf(voice()))
        val session = OfflineSpeechSession { CompletableFuture.completedFuture(engine) }
        val first = session.speak("one")
        val oldId = engine.utterances.keys.single()
        val second = session.speak("two")
        val id = engine.utterances.keys.last()
        assertTrue(first.isCompletedExceptionally)
        engine.callback!!.done(oldId)
        assertFalse(second.isDone)
        assertEquals(LocalTtsPhase.SPEAKING, session.state().phase)
        engine.callback!!.done(id)
        assertTrue(second.isDone)
        assertEquals(LocalTtsPhase.READY, session.state().phase)
    }

    @Test fun closeWhileInitializingShutsDownLateEngine() {
        val initialization = CompletableFuture<OfflineSpeechEngine>()
        val session = OfflineSpeechSession { initialization }
        val ready = session.initialize()
        session.close()
        val engine = FakeEngine(listOf(voice()))
        initialization.complete(engine)
        assertTrue(ready.isCompletedExceptionally)
        assertTrue(engine.closed)
        assertEquals(LocalTtsPhase.STOPPED, session.state().phase)
    }

    @Test fun rejectionAndEngineErrorCompleteSpeechAndPermitRecovery() {
        val engine = FakeEngine(listOf(voice()))
        val session = OfflineSpeechSession { CompletableFuture.completedFuture(engine) }
        engine.accept = false
        assertTrue(session.speak("rejected").isCompletedExceptionally)
        engine.accept = true
        val speech = session.speak("accepted")
        engine.callback!!.failed(engine.utterances.keys.last())
        assertTrue(speech.isCompletedExceptionally)
        assertEquals(LocalTtsPhase.FAILED, session.state().phase)
    }

    @Test fun missingEngineIsActionableAndBlankOrOversizeSpeechIsRejected() {
        val session = OfflineSpeechSession {
            CompletableFuture.failedFuture(IllegalStateException("engine unavailable"))
        }
        assertEquals(LocalTtsPhase.UNAVAILABLE, session.initialize().join().phase)
        assertTrue(session.state().failure!!.contains("Install or enable"))
        assertThrows(IllegalArgumentException::class.java) { session.speak(" ") }
        assertThrows(IllegalArgumentException::class.java) { session.speak("x".repeat(16_385)) }
    }

    private fun voice(network: Boolean = false, installed: Boolean = true) =
        OfflineSpeechVoice("test", "en-US", network, installed, 400, 100)

    private class FakeEngine(override val voices: List<OfflineSpeechVoice>) : OfflineSpeechEngine {
        var callback: OfflineSpeechListener? = null
        var closed = false
        var accept = true
        val utterances = linkedMapOf<String, String>()
        override fun selectVoice(id: String) = true
        override fun setListener(value: OfflineSpeechListener) { callback = value }
        override fun speak(text: String, id: String): Boolean {
            utterances[id] = text
            return accept
        }
        override fun stop() = Unit
        override fun close() { closed = true }
    }
}
