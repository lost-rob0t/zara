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
        private var listener: LocalGenerationListener? = null
        var cancelled = false
        var unloaded = false

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

        fun emit(text: String) {
            listener?.onChunk(text)
        }

        fun complete() {
            listener?.onDone()
        }

        override fun unload() {
            unloaded = true
        }

        override fun close() = Unit
    }
}
