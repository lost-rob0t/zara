package ai.zara.app.localai

import java.io.File
import java.util.concurrent.CountDownLatch
import java.util.concurrent.TimeUnit
import org.junit.Assert.assertEquals
import org.junit.Assert.assertFalse
import org.junit.Assert.assertThrows
import org.junit.Assert.assertTrue
import org.junit.Test

class LocalAiRuntimeTest {
    @Test
    fun modelSpecRequiresExplicitKnownQuantizationAndStrongIdentity() {
        val model = File.createTempFile("zara-test-model", ".litertlm")
        model.writeText("fixture")

        val spec = LocalModelSpec(
            id = "gemma3-1b-it",
            version = "1",
            quantization = LocalModelQuantization.requireKnown("dynamic-int4"),
            sha256 = "a".repeat(64),
            path = model.absolutePath,
            maxContextTokens = 4096,
            backend = LocalModelBackend.GPU,
        )

        assertEquals(LocalModelQuantization.DYNAMIC_INT4, spec.quantization)
        assertThrows(IllegalArgumentException::class.java) {
            LocalModelQuantization.requireKnown("unknown")
        }
        assertThrows(IllegalArgumentException::class.java) {
            spec.copy(sha256 = "nope")
        }
    }

    @Test
    fun actorSerializesLoadStreamingGenerationAndUnload() {
        val backend = FakeLlmBackend()
        val runtime = LocalAiRuntime(backend)
        val spec = modelSpec()

        assertEquals(LocalAiPhase.READY, runtime.load(spec).get(2, TimeUnit.SECONDS).phase)
        val chunks = mutableListOf<String>()
        val future = runtime.generate(
            LocalGenerationRequest(prompt = "hello", maxOutputTokens = 32),
            chunks::add,
        )

        assertTrue(backend.awaitGenerationStarted())
        backend.emit("hello ")
        backend.emit("world")
        backend.complete()

        val result = future.get(2, TimeUnit.SECONDS)
        assertEquals("hello world", result.text)
        assertEquals(listOf("hello ", "world"), chunks)
        assertEquals(LocalAiPhase.READY, runtime.state().phase)
        assertEquals(LocalAiPhase.STOPPED, runtime.unload().get(2, TimeUnit.SECONDS).phase)
        assertTrue(backend.unloaded)
        runtime.close()
    }

    @Test
    fun cancellationPoisonsOnlyTheActiveGenerationAndReturnsToReady() {
        val backend = FakeLlmBackend()
        val runtime = LocalAiRuntime(backend)
        runtime.load(modelSpec()).get(2, TimeUnit.SECONDS)
        val future = runtime.generate(LocalGenerationRequest("cancel me", 16))
        assertTrue(backend.awaitGenerationStarted())

        val state = runtime.cancel().get(2, TimeUnit.SECONDS)

        assertEquals(LocalAiPhase.READY, state.phase)
        assertTrue(backend.cancelled)
        assertTrue(future.isCompletedExceptionally)
        assertThrows(Exception::class.java) {
            future.get(2, TimeUnit.SECONDS)
        }
        runtime.close()
    }

    @Test
    fun secondGenerationIsRejectedWhileOneIsActive() {
        val backend = FakeLlmBackend()
        val runtime = LocalAiRuntime(backend)
        runtime.load(modelSpec()).get(2, TimeUnit.SECONDS)
        val first = runtime.generate(LocalGenerationRequest("one", 8))
        assertTrue(backend.awaitGenerationStarted())
        val second = runtime.generate(LocalGenerationRequest("two", 8))

        assertThrows(Exception::class.java) {
            second.get(2, TimeUnit.SECONDS)
        }
        assertFalse(first.isDone)
        runtime.cancel().get(2, TimeUnit.SECONDS)
        runtime.close()
    }

    @Test
    fun closeRejectsAllNewModelWorkBeforeBackendShutdownCompletes() {
        val backend = FakeLlmBackend().apply { blockClose = true }
        val runtime = LocalAiRuntime(backend)
        val spec = modelSpec()
        runtime.load(spec).get(2, TimeUnit.SECONDS)
        val closeFinished = CountDownLatch(1)
        val closer = Thread {
            try {
                runtime.close()
            } finally {
                closeFinished.countDown()
            }
        }
        closer.start()

        try {
            assertTrue("backend close must be in progress", backend.awaitCloseStarted())
            val lateGenerate = runtime.generate(LocalGenerationRequest("must reject", 8))
            val lateLoad = runtime.load(spec)
            val lateCancel = runtime.cancel()
            val lateUnload = runtime.unload()

            assertTrue(
                "new local generations must fail immediately once close begins",
                lateGenerate.isCompletedExceptionally,
            )
            assertTrue(
                "model loads must fail immediately once close begins",
                lateLoad.isCompletedExceptionally,
            )
            assertTrue(
                "cancellation requests must fail immediately once close begins",
                lateCancel.isCompletedExceptionally,
            )
            assertTrue(
                "model unloads must fail immediately once close begins",
                lateUnload.isCompletedExceptionally,
            )
        } finally {
            backend.releaseClose()
            assertTrue(closeFinished.await(2, TimeUnit.SECONDS))
            closer.join(2_000)
        }
    }

    @Test
    fun repeatedCloseCallsWaitForTheSameBackendTeardown() {
        val backend = FakeLlmBackend().apply { blockClose = true }
        val runtime = LocalAiRuntime(backend)
        runtime.load(modelSpec()).get(2, TimeUnit.SECONDS)
        val firstCloseFinished = CountDownLatch(1)
        val secondCloseFinished = CountDownLatch(1)
        val firstCloser = Thread {
            try {
                runtime.close()
            } finally {
                firstCloseFinished.countDown()
            }
        }
        val secondCloser = Thread {
            try {
                runtime.close()
            } finally {
                secondCloseFinished.countDown()
            }
        }

        firstCloser.start()
        assertTrue("backend close must start before the second close", backend.awaitCloseStarted())
        secondCloser.start()

        try {
            assertFalse(
                "a repeated close must not return before shared teardown completes",
                secondCloseFinished.await(100, TimeUnit.MILLISECONDS),
            )
        } finally {
            backend.releaseClose()
        }

        assertTrue(firstCloseFinished.await(2, TimeUnit.SECONDS))
        assertTrue(secondCloseFinished.await(2, TimeUnit.SECONDS))
        firstCloser.join(2_000)
        secondCloser.join(2_000)
        assertEquals(LocalAiPhase.STOPPED, runtime.state().phase)
    }

    @Test
    fun backendCallbacksAfterCloseAreDroppedWithoutRevivingRuntimeState() {
        val backend = FakeLlmBackend()
        val runtime = LocalAiRuntime(backend)
        runtime.load(modelSpec()).get(2, TimeUnit.SECONDS)
        val future = runtime.generate(LocalGenerationRequest("close me", 16))
        assertTrue(backend.awaitGenerationStarted())

        runtime.close()

        assertTrue(future.isCompletedExceptionally)
        assertEquals(LocalAiPhase.STOPPED, runtime.state().phase)
        backend.emit("late")
        backend.complete()
        backend.fail(IllegalStateException("late failure"))
        assertEquals(LocalAiPhase.STOPPED, runtime.state().phase)
    }

    @Test
    fun closeFromStateObserverDoesNotDeadlockActor() {
        val backend = FakeLlmBackend()
        val runtime = LocalAiRuntime(backend)
        val observerCloseReturned = CountDownLatch(1)
        runtime.setStateObserver { state ->
            if (state.phase == LocalAiPhase.READY) {
                runtime.close()
                observerCloseReturned.countDown()
            }
        }

        val loaded = runtime.load(modelSpec())

        assertTrue(
            "close invoked from the actor-owned observer must return",
            observerCloseReturned.await(2, TimeUnit.SECONDS),
        )
        assertEquals(LocalAiPhase.READY, loaded.get(2, TimeUnit.SECONDS).phase)
        runtime.close()
        assertEquals(LocalAiPhase.STOPPED, runtime.state().phase)
        assertTrue(backend.awaitCloseStarted())
    }

    @Test
    fun remoteCallerDeathCancelsCanonicalGenerationAndDropsLateBackendCallbacks() {
        val backend = FakeLlmBackend()
        val runtime = LocalAiRuntime(backend)
        runtime.load(modelSpec()).get(2, TimeUnit.SECONDS)
        val firstChunk = CountDownLatch(1)
        val chunks = mutableListOf<String>()
        var cancellation: java.util.concurrent.CompletableFuture<LocalAiState>? = null
        val lease = LocalAiRemoteGenerationLease(
            cancel = { cancellation = runtime.cancel() },
            unlink = {},
        )

        val future = checkNotNull(
            lease.runIfActive {
                runtime.generate(LocalGenerationRequest("remote caller", 16)) { chunk ->
                    chunks += chunk
                    firstChunk.countDown()
                }
            }
        )
        assertTrue(backend.awaitGenerationStarted())
        backend.emit("first")
        assertTrue(firstChunk.await(2, TimeUnit.SECONDS))

        assertTrue(lease.callerDied())
        val cancelledState = checkNotNull(cancellation).get(2, TimeUnit.SECONDS)
        assertEquals(LocalAiPhase.READY, cancelledState.phase)
        assertTrue(backend.cancelled)
        assertTrue(future.isCompletedExceptionally)

        backend.emit("late")
        backend.complete()
        Thread.sleep(50)
        assertEquals(listOf("first"), chunks)
        assertEquals(LocalAiPhase.READY, runtime.state().phase)
        runtime.close()
    }

    private fun modelSpec() = LocalModelSpec(
        id = "fixture",
        version = "1",
        quantization = LocalModelQuantization.INT4,
        sha256 = "b".repeat(64),
        path = "/data/user/0/ai.zara.app/files/zara/models/fixture.litertlm",
        maxContextTokens = 2048,
        backend = LocalModelBackend.CPU,
    )

    private class FakeLlmBackend : LocalLlmBackend {
        private val generationStarted = CountDownLatch(1)
        private val closeStarted = CountDownLatch(1)
        private val allowClose = CountDownLatch(1)
        private var listener: LocalGenerationListener? = null
        var cancelled = false
        var unloaded = false
        var blockClose = false

        override fun load(spec: LocalModelSpec) = Unit

        override fun generate(
            request: LocalGenerationRequest,
            listener: LocalGenerationListener,
        ): LocalGenerationSession {
            this.listener = listener
            generationStarted.countDown()
            return object : LocalGenerationSession {
                override fun cancel() {
                    cancelled = true
                }

                override fun close() = Unit
            }
        }

        fun awaitGenerationStarted(): Boolean = generationStarted.await(2, TimeUnit.SECONDS)

        fun awaitCloseStarted(): Boolean = closeStarted.await(2, TimeUnit.SECONDS)

        fun releaseClose() {
            allowClose.countDown()
        }

        fun emit(text: String) {
            listener?.onChunk(text)
        }

        fun complete() {
            listener?.onDone()
        }

        fun fail(error: Throwable) {
            listener?.onError(error)
        }

        override fun unload() {
            unloaded = true
        }

        override fun close() {
            closeStarted.countDown()
            if (blockClose) {
                check(allowClose.await(2, TimeUnit.SECONDS)) { "Timed out waiting to release fake backend close" }
            }
        }
    }
}
