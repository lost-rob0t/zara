package ai.zara.app.runtime

import ai.zara.app.model.LocalModelBackend
import ai.zara.app.model.LocalModelConfig
import ai.zara.app.model.LocalModelConfigStore
import ai.zara.app.model.LocalModelCoordinator
import ai.zara.app.model.LocalModelRequest
import ai.zara.app.model.LocalModelResult
import ai.zara.app.prolog.PrologWorkspace
import ai.zara.app.prolog.TreallaBridge
import java.io.File
import java.util.concurrent.TimeUnit
import org.junit.Assert.assertEquals
import org.junit.Assert.assertTrue
import org.junit.Rule
import org.junit.Test
import org.junit.rules.TemporaryFolder

class LocalZaraServerModelTest {
    @get:Rule
    val temporary = TemporaryFolder()

    @Test
    fun unresolvedNaturalLanguageFallsThroughToEnabledLocalModel() {
        val backend = RecordingModelBackend("local answer")
        val server = serverWithModel(backend)
        server.start().get(2, TimeUnit.SECONDS)

        val result = server.resolve("tell me something conversational").get(2, TimeUnit.SECONDS)

        assertEquals(listOf("local answer"), result.terms)
        assertEquals(listOf("tell me something conversational"), backend.prompts)
        server.close()
    }

    @Test
    fun explicitPrologQueriesNeverFallThroughToModel() {
        val backend = RecordingModelBackend("must not run")
        val server = serverWithModel(backend)
        server.start().get(2, TimeUnit.SECONDS)

        val result = server.query("known(Result)").get(2, TimeUnit.SECONDS)

        assertTrue(result.terms.isEmpty())
        assertTrue(backend.prompts.isEmpty())
        server.close()
    }

    @Test
    fun modelCommandsCanDisableFallbackWithoutTouchingPrologRuntime() {
        val backend = RecordingModelBackend("must not run")
        val server = serverWithModel(backend)
        server.start().get(2, TimeUnit.SECONDS)

        val command = server.resolve("/model off").get(2, TimeUnit.SECONDS)
        val unresolved = server.resolve("ordinary conversation").get(2, TimeUnit.SECONDS)

        assertTrue(command.terms.single().contains("disabled"))
        assertTrue(unresolved.terms.isEmpty())
        assertTrue(backend.prompts.isEmpty())
        assertEquals(LocalServerPhase.READY, server.state().phase)
        server.close()
    }

    private fun serverWithModel(backend: LocalModelBackend): LocalZaraServer {
        val store = LocalModelConfigStore(File(temporary.newFolder(), "model.properties"))
        store.save(
            LocalModelConfig(
                enabled = true,
                endpoint = "http://127.0.0.1:8080",
                model = "tiny-test-model",
                quantization = "Q4_K_M",
            )
        )
        return LocalZaraServer(
            bridge = EmptyTreallaBridge(),
            corePath = "/private/semantic_core.pl",
            workspace = PrologWorkspace(temporary.newFolder()),
            localModel = LocalModelCoordinator(store, backend),
        )
    }

    private class EmptyTreallaBridge : TreallaBridge {
        override fun initialize(coreAssetPath: String) = Unit
        override fun consult(sourcePath: String) = Unit
        override fun evaluate(query: String): List<String> = emptyList()
        override fun shutdown() = Unit
    }

    private class RecordingModelBackend(
        private val answer: String,
    ) : LocalModelBackend {
        val prompts = mutableListOf<String>()

        override fun generate(
            config: LocalModelConfig,
            request: LocalModelRequest,
            cancelled: () -> Boolean,
            onText: (String) -> Unit,
        ): LocalModelResult {
            prompts += request.prompt
            onText(answer)
            return LocalModelResult(config.identity(), answer, 1)
        }
    }
}
