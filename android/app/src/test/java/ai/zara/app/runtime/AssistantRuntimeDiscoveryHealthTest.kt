package ai.zara.app.runtime

import com.google.gson.JsonArray
import com.google.gson.JsonObject
import java.util.concurrent.TimeUnit
import org.junit.Assert.assertEquals
import org.junit.Assert.assertFalse
import org.junit.Assert.assertTrue
import org.junit.Test

class AssistantRuntimeDiscoveryHealthTest {
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
