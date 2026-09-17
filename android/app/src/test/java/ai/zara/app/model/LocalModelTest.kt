package ai.zara.app.model

import java.io.File
import java.util.concurrent.TimeUnit
import org.junit.Assert.assertEquals
import org.junit.Assert.assertTrue
import org.junit.Rule
import org.junit.Test
import org.junit.rules.TemporaryFolder

class LocalModelTest {
    @get:Rule
    val temporary = TemporaryFolder()

    @Test
    fun configRejectsAnyNonLoopbackModelEndpoint() {
        assertTrue(LocalModelConfig(enabled = true).runCatchingValidated().isSuccess)
        assertTrue(
            LocalModelConfig(enabled = true, endpoint = "http://localhost:9000")
                .runCatchingValidated()
                .isSuccess
        )
        assertTrue(
            LocalModelConfig(enabled = true, endpoint = "http://[::1]:8080")
                .runCatchingValidated()
                .isSuccess
        )
        assertTrue(
            LocalModelConfig(enabled = true, endpoint = "https://example.com:443")
                .runCatchingValidated()
                .isFailure
        )
        assertTrue(
            LocalModelConfig(enabled = true, endpoint = "http://user@127.0.0.1:8080")
                .runCatchingValidated()
                .isFailure
        )
    }

    @Test
    fun coordinatorRunsGenerationOnOneDedicatedActor() {
        val backend = RecordingBackend("hello from local model")
        val store = enabledStore("actor")
        val coordinator = LocalModelCoordinator(store, backend)

        val first = coordinator.generate("one").get(2, TimeUnit.SECONDS)
        val second = coordinator.generate("two").get(2, TimeUnit.SECONDS)

        assertEquals("hello from local model", first.text)
        assertEquals(listOf("one", "two"), backend.prompts)
        assertEquals(1, backend.threadNames.distinct().size)
        assertTrue(backend.threadNames.first().contains("zara-local-model"))
        coordinator.close()
    }

    @Test
    fun activeGenerationCanBeCancelledWithoutChangingAuthority() {
        val backend = BlockingBackend()
        val coordinator = LocalModelCoordinator(enabledStore("cancel"), backend)
        val future = coordinator.generate("keep talking")
        backend.started.get(2, TimeUnit.SECONDS)

        coordinator.cancelActive()
        val failure = runCatching { future.get(2, TimeUnit.SECONDS) }.exceptionOrNull()

        assertTrue(failure != null)
        assertTrue(backend.cancelCalled)
        assertEquals(LocalModelFailureReason.CANCELLED, coordinator.state().lastFailure)
        coordinator.close()
    }

    private fun enabledStore(name: String): LocalModelConfigStore {
        val store = LocalModelConfigStore(File(temporary.newFolder(name), "model.properties"))
        store.save(
            LocalModelConfig(
                enabled = true,
                endpoint = "http://127.0.0.1:8080",
                model = "tiny-test-model",
                quantization = "Q4_K_M",
            )
        )
        return store
    }

    private fun LocalModelConfig.runCatchingValidated(): Result<LocalModelConfig> =
        runCatching { validated() }

    private class RecordingBackend(
        private val answer: String,
    ) : LocalModelBackend {
        val prompts = mutableListOf<String>()
        val threadNames = mutableListOf<String>()

        override fun generate(
            config: LocalModelConfig,
            request: LocalModelRequest,
            cancelled: () -> Boolean,
            onText: (String) -> Unit,
        ): LocalModelResult {
            prompts += request.prompt
            threadNames += Thread.currentThread().name
            onText(answer)
            return LocalModelResult(config.identity(), answer, 1)
        }
    }

    private class BlockingBackend : LocalModelBackend {
        val started = java.util.concurrent.CompletableFuture<Unit>()
        @Volatile var cancelCalled = false

        override fun generate(
            config: LocalModelConfig,
            request: LocalModelRequest,
            cancelled: () -> Boolean,
            onText: (String) -> Unit,
        ): LocalModelResult {
            started.complete(Unit)
            while (!cancelled()) Thread.sleep(5)
            throw LocalModelException(LocalModelFailureReason.CANCELLED, "cancelled")
        }

        override fun cancel() {
            cancelCalled = true
        }
    }
}
