package ai.zara.app.runtime

import com.google.gson.JsonArray
import com.google.gson.JsonObject
import java.util.concurrent.TimeUnit
import org.junit.Assert.assertEquals
import org.junit.Assert.assertFalse
import org.junit.Assert.assertTrue
import org.junit.Test

class AssistantRuntimeTest {
    @Test
    fun compatiblePrologRuntimeIsDiscoveredAndProviderAuthorityStaysInRuntime() {
        val client = PrologRlmSidecarClient(requestOverride = { method, path, _, timeout ->
            assertEquals("GET", method)
            assertEquals("/zara-runtime/v1/discover", path)
            assertEquals(350, timeout)
            discoveryPayload()
        })

        val runtime = client.discover()

        assertEquals(PROLOG_RLM_RUNTIME_ID, runtime.id)
        assertEquals("local_sidecar", runtime.locality)
        assertTrue(runtime.supportsCancel)
        assertFalse(runtime.supportsStreaming)
    }

    @Test
    fun generatedTurnContainsNoProviderOrCredentialFields() {
        var requestBody: JsonObject? = null
        val client = PrologRlmSidecarClient(requestOverride = { method, path, body, _ ->
            when (path) {
                "/zara-runtime/v1/discover" -> discoveryPayload()
                "/zara-runtime/v1/generate" -> {
                    assertEquals("POST", method)
                    requestBody = body
                    JsonObject().apply {
                        addProperty("protocol", ZARA_RUNTIME_PROTOCOL)
                        addProperty("runtime_id", PROLOG_RLM_RUNTIME_ID)
                        addProperty("request_id", "turn-1")
                        addProperty("status", "completed")
                        addProperty("text", "from Prolog")
                    }
                }
                else -> error("unexpected path $path")
            }
        })

        val result = client.generate(
            text = "question",
            requestId = "turn-1",
            conversationId = "conversation-1",
        )

        assertEquals("from Prolog", result.text)
        val serialized = requestBody.toString()
        assertFalse(serialized.contains("api_key"))
        assertFalse(serialized.contains("credential"))
        assertFalse(serialized.contains("\"provider\""))
        assertFalse(serialized.contains("\"model\""))
    }

    @Test
    fun registryListsOnlyEmbeddedRuntimeWhenOptionalSidecarIsAbsent() {
        val client = PrologRlmSidecarClient(requestOverride = { _, _, _, _ ->
            throw AssistantRuntimeException("offline")
        })
        AssistantRuntimeRegistry(prologRlm = client).use { registry ->
            val runtimes = registry.discover().get(2, TimeUnit.SECONDS)

            assertEquals(listOf(EMBEDDED_LOCAL_RUNTIME_ID), runtimes.map { it.id })
            assertEquals(EMBEDDED_LOCAL_RUNTIME_ID, registry.selectedRuntimeId())
        }
    }

    @Test
    fun persistedSelectionCanOnlyBeAppliedAfterLiveDiscovery() {
        val client = PrologRlmSidecarClient(requestOverride = { _, path, _, _ ->
            if (path == "/zara-runtime/v1/discover") discoveryPayload() else error(path)
        })
        AssistantRuntimeRegistry(prologRlm = client).use { registry ->
            var rejectedBeforeDiscovery = false
            try {
                registry.select(PROLOG_RLM_RUNTIME_ID)
            } catch (_: IllegalArgumentException) {
                rejectedBeforeDiscovery = true
            }
            assertTrue(rejectedBeforeDiscovery)

            registry.discover().get(2, TimeUnit.SECONDS)
            registry.select(PROLOG_RLM_RUNTIME_ID)

            assertEquals(PROLOG_RLM_RUNTIME_ID, registry.selectedRuntimeId())
        }
    }

    private fun discoveryPayload(): JsonObject = JsonObject().apply {
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
                        addProperty("health", "ready")
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
