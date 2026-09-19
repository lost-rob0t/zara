package ai.zara.app.runtime

import ai.zara.app.localai.LocalAiPhase
import ai.zara.app.localai.LocalAiState
import ai.zara.app.localai.LocalModelBackend
import ai.zara.app.localai.LocalModelQuantization
import ai.zara.app.localai.LocalModelSpec
import com.google.gson.JsonArray
import com.google.gson.JsonObject
import java.util.concurrent.TimeUnit
import org.junit.Assert.assertEquals
import org.junit.Assert.assertFalse
import org.junit.Assert.assertTrue
import org.junit.Test

class AssistantRuntimeDiscoveryHealthTest {
    @Test
    fun embeddedLocalDescriptorTracksCanonicalLocalAiLifecycle() {
        val model = localModelSpec()
        val cases = listOf(
            LocalAiState(phase = LocalAiPhase.STOPPED) to "stopped",
            LocalAiState(phase = LocalAiPhase.LOADING, model = model) to "starting",
            LocalAiState(phase = LocalAiPhase.READY, model = model) to "ready",
            LocalAiState(phase = LocalAiPhase.GENERATING, model = model) to "busy",
            LocalAiState(phase = LocalAiPhase.FAILED, model = model, failure = "backend died") to "failed",
        )

        cases.forEach { (state, expectedHealth) ->
            val descriptor = embeddedLocalRuntimeDescriptor(state)

            assertEquals(EMBEDDED_LOCAL_RUNTIME_ID, descriptor.id)
            assertEquals(expectedHealth, descriptor.health)
            assertEquals(state.model?.version ?: "unloaded", descriptor.runtimeVersion)
            assertEquals(expectedHealth in setOf("ready", "busy"), descriptor.selectable)
        }
    }

    @Test
    fun impossibleReadyWithoutModelFailsClosedAsDegraded() {
        val descriptor = embeddedLocalRuntimeDescriptor(
            LocalAiState(phase = LocalAiPhase.READY, model = null),
        )

        assertEquals("degraded", descriptor.health)
        assertEquals("unloaded", descriptor.runtimeVersion)
        assertFalse(descriptor.selectable)
    }

    @Test
    fun degradedPrologRuntimeRemainsDiscoveredButCannotBeSelected() {
        val client = PrologRlmSidecarClient(requestOverride = { _, path, _, _ ->
            check(path == "/zara-runtime/v1/discover")
            discoveryPayload(health = "degraded")
        })

        AssistantRuntimeRegistry(prologRlm = client).use { registry ->
            val runtimes = registry.discover().get(2, TimeUnit.SECONDS)
            val prolog = runtimes.single { it.id == PROLOG_RLM_RUNTIME_ID }

            assertEquals("degraded", prolog.health)
            assertFalse(prolog.selectable)
            assertEquals(EMBEDDED_LOCAL_RUNTIME_ID, registry.selectedRuntimeId())

            val rejected = runCatching { registry.select(PROLOG_RLM_RUNTIME_ID) }.isFailure
            assertTrue(rejected)
        }
    }

    private fun localModelSpec(): LocalModelSpec = LocalModelSpec(
        id = "fixture-model",
        version = "1.2.3",
        quantization = LocalModelQuantization.INT4,
        sha256 = "0".repeat(64),
        path = "/models/fixture-model.litertlm",
        maxContextTokens = 4_096,
        backend = LocalModelBackend.CPU,
    )

    private fun discoveryPayload(health: String): JsonObject = JsonObject().apply {
        add(
            "runtimes",
            JsonArray().apply {
                add(
                    JsonObject().apply {
                        addProperty("id", PROLOG_RLM_RUNTIME_ID)
                        addProperty("display_name", "Prolog-RLM")
                        addProperty("protocol", ZARA_RUNTIME_PROTOCOL)
                        addProperty("runtime_version", "0.1.0-dev")
                        addProperty("implementation_version", "0.1.0-dev")
                        addProperty("installed", true)
                        addProperty("available", true)
                        addProperty("health", health)
                        addProperty("locality", "local_sidecar")
                        addProperty("transport", "loopback_http")
                        add("capabilities", JsonArray())
                        add("profiles", JsonArray())
                        addProperty("provider_control", "runtime")
                        addProperty("model_control", "runtime")
                        addProperty("supports_streaming", false)
                        addProperty("supports_cancel", true)
                        addProperty("supports_context_handles", false)
                        addProperty("supports_host_tools", false)
                    },
                )
            },
        )
    }
}
