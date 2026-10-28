package ai.zara.app.localai

import java.util.concurrent.CompletableFuture
import java.util.concurrent.CountDownLatch
import java.util.concurrent.TimeUnit
import org.junit.Assert.*
import org.junit.Test

class IsolatedSpeechBackendTest {
    @Test fun blockedEngineCannotBlockStateStopOrInitializationDeadline() {
        val entered = CountDownLatch(1)
        val release = CountDownLatch(1)
        val engine = FakeBackend {
            entered.countDown()
            check(release.await(3, TimeUnit.SECONDS))
            CompletableFuture.completedFuture(ready)
        }
        val backend = IsolatedSpeechBackend(engine, initializationTimeoutMs = 100)
        try {
            val initialization = backend.initialize()
            assertTrue(entered.await(1, TimeUnit.SECONDS))
            assertEquals(LocalTtsPhase.UNAVAILABLE, initialization.get(1, TimeUnit.SECONDS).phase)
            // These calls must return even while the worker remains inside an engine Binder call.
            assertEquals(LocalTtsPhase.UNAVAILABLE, backend.state().phase)
            backend.stop()
            assertEquals(LocalTtsPhase.UNAVAILABLE, backend.initialize().get(1, TimeUnit.SECONDS).phase)
        } finally { release.countDown(); backend.close() }
    }

    @Test fun stopDuringEngineStartupCannotSpeakAfterLateReadiness() {
        val startup = CompletableFuture<LocalTtsState>()
        val engine = FakeBackend { startup }
        val backend = IsolatedSpeechBackend(engine)
        try {
            val speech = backend.speak("must never play")
            backend.stop()
            startup.complete(ready)
            assertThrows(Exception::class.java) { speech.get(1, TimeUnit.SECONDS) }
            // A new initialization is a worker barrier after the cancelled request.
            backend.initialize().get(1, TimeUnit.SECONDS)
            assertEquals(0, engine.spoken)
        } finally { backend.close() }
    }

    @Test fun readyEngineSpeaksAndClosedBackendRejectsFurtherWork() {
        val engine = FakeBackend { CompletableFuture.completedFuture(ready) }
        val backend = IsolatedSpeechBackend(engine)
        backend.speak("hello").get(1, TimeUnit.SECONDS)
        assertEquals(1, engine.spoken)
        assertEquals(LocalTtsPhase.READY, backend.state().phase)
        backend.close()
        assertEquals(LocalTtsPhase.STOPPED, backend.state().phase)
        assertThrows(Exception::class.java) { backend.initialize().get(1, TimeUnit.SECONDS) }
    }

    @Test fun synchronousEngineFailureLeavesAnActionableStateAndAllowsRetry() {
        val engine = FakeBackend { CompletableFuture.completedFuture(ready) }
        engine.failSpeak = true
        val backend = IsolatedSpeechBackend(engine)
        try {
            assertThrows(Exception::class.java) { backend.speak("fail").get(1, TimeUnit.SECONDS) }
            assertEquals(LocalTtsPhase.FAILED, backend.state().phase)
            engine.failSpeak = false
            backend.speak("recover").get(1, TimeUnit.SECONDS)
            assertEquals(LocalTtsPhase.READY, backend.state().phase)
        } finally { backend.close() }
    }

    private class FakeBackend(val startup: () -> CompletableFuture<LocalTtsState>) : LocalTtsBackend {
        @Volatile var spoken = 0
        var failSpeak = false
        override fun state() = ready
        override fun initialize() = startup()
        override fun speak(text: String): CompletableFuture<Unit> { check(!failSpeak) { "fixture failure" }; spoken++; return CompletableFuture.completedFuture(Unit) }
        override fun stop() = Unit
        override fun close() = Unit
    }
    companion object { val ready = LocalTtsState(LocalTtsPhase.READY, "fixture-offline", "en-US") }
}
