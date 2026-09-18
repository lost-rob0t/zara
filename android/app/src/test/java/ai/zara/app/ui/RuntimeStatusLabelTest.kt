package ai.zara.app.ui

import ai.zara.app.localai.LocalAiPhase
import ai.zara.app.localai.LocalAiState
import ai.zara.app.localai.LocalModelBackend
import ai.zara.app.localai.LocalModelFormat
import ai.zara.app.localai.LocalModelQuantization
import ai.zara.app.localai.LocalModelSpec
import ai.zara.app.runtime.LocalServerPhase
import ai.zara.app.runtime.LocalServerState
import ai.zara.app.runtime.RuntimeMode
import org.junit.Assert.assertEquals
import org.junit.Test

class RuntimeStatusLabelTest {
    private val localReady = LocalServerState(
        phase = LocalServerPhase.READY,
        generation = 1,
        loadedSources = listOf("core", "workspace", "user"),
    )

    private val model = LocalModelSpec(
        id = "fixture-model",
        version = "1",
        quantization = LocalModelQuantization.Q4_K_M,
        sha256 = "a".repeat(64),
        path = "/tmp/fixture.gguf",
        maxContextTokens = 2_048,
        backend = LocalModelBackend.CPU,
        format = LocalModelFormat.GGUF,
    )

    @Test
    fun `failed local model is not advertised as active`() {
        val projection = RuntimeUiProjection(
            backendLabel = "local",
            chatReady = true,
            remoteInformational = true,
        )
        val failed = LocalAiState(
            phase = LocalAiPhase.FAILED,
            generation = 3,
            model = model,
            failure = "load failed",
        )

        assertEquals(
            "Offline · Symbolic",
            runtimeStatusLabel(projection, RuntimeMode.Local, localReady, failed),
        )
    }

    @Test
    fun `ready local model is advertised with canonical identity`() {
        val projection = RuntimeUiProjection(
            backendLabel = "local fallback",
            chatReady = true,
            remoteInformational = false,
        )
        val ready = LocalAiState(
            phase = LocalAiPhase.READY,
            generation = 4,
            model = model,
        )

        assertEquals(
            "Offline · Local model fixture-model",
            runtimeStatusLabel(projection, RuntimeMode.Auto, localReady, ready),
        )
    }
}
