package ai.zara.app.runtime

import com.google.gson.JsonArray
import com.google.gson.JsonObject
import java.util.concurrent.ExecutionException
import java.util.concurrent.TimeUnit
import org.junit.Assert.assertEquals
import org.junit.Assert.assertTrue
import org.junit.Test

class AssistantRuntimeFailureFencingTest {
    @Test
    fun terminalCancellationIsTypedAndKeepsLiveRuntimeSelected() {
        val client = PrologRlmSidecarClient(requestOverride = { _, path, _, _ ->
            when (path) {
                "/zara-runtime/v1/discover" -> discoveryPayload()
                "/zara-runtime/v1/generate" -> cancelledPayload("cancelled-turn")
                else -> error("unexpected path $path")
            }
        })

        AssistantRuntimeRegistry(prologRlm = client).use { registry ->
            registry.discover().get(2, TimeUnit.SECONDS)
            registry.select(PROLOG_RLM_RUNTIME_ID)

            val failure = generationFailure(
                registry.generatePrologRlm(
                    text = "cancel me",
                    requestId = "cancelled-turn",
                    conversationId = "conversation-1",
                ),
            )

            assertTrue(failure is AssistantRuntimeCancelledException)
            assertEquals(PROLOG_RLM_RUNTIME_ID, registry.selectedRuntimeId())
            assertTrue(registry.discovered().any { it.id == PROLOG_RLM_RUNTIME_ID })
        }
    }

    @Test
    fun transportDeathFencesRuntimeBeforeFailureIsPublished() {
        val client = PrologRlmSidecarClient(requestOverride = { _, path, _, _ ->
            when (path) {
                "/zara-runtime/v1/discover" -> discoveryPayload()
                "/zara-runtime/v1/generate" -> throw IllegalStateException("connection reset")
                else -> error("unexpected path $path")
            }
        })

        AssistantRuntimeRegistry(prologRlm = client).use { registry ->
            registry.discover().get(2, TimeUnit.SECONDS)
            registry.select(PROLOG_RLM_RUNTIME_ID)

            val failure = generationFailure(
                registry.generatePrologRlm(
                    text = "runtime dies",
                    requestId = "dead-turn",
                    conversationId = "conversation-1",
                ),
            )

            assertTrue(failure is AssistantRuntimeUnavailableException)
            assertEquals(EMBEDDED_LOCAL_RUNTIME_ID, registry.selectedRuntimeId())
            assertEquals(
                listOf(EMBEDDED_LOCAL_RUNTIME_ID),
                registry.discovered().map { it.id },
            )
        }
    }

    @Test
    fun terminalTurnFailureDoesNotImpersonateRuntimeDeath() {
        val client = PrologRlmSidecarClient(requestOverride = { _, path, _, _ ->
            when (path) {
                "/zara-runtime/v1/discover" -> discoveryPayload()
                "/zara-runtime/v1/generate" -> failedPayload("failed-turn")
                else -> error("unexpected path $path")
            }
        })

        AssistantRuntimeRegistry(prologRlm = client).use { registry ->
            registry.discover().get(2, TimeUnit.SECONDS)
            registry.select(PROLOG_RLM_RUNTIME_ID)

            val failure = generationFailure(
                registry.generatePrologRlm(
                    text = "provider rejected turn",
                    requestId = "failed-turn",
                    conversationId = "conversation-1",
                ),
            )

            assertTrue(failure is AssistantRuntimeTurnFailedException)
            assertEquals(PROLOG_RLM_RUNTIME_ID, registry.selectedRuntimeId())
            assertTrue(registry.discovered().any { it.id == PROLOG_RLM_RUNTIME_ID })
        }
    }

    private fun generationFailure(
        future: java.util.concurrent.CompletableFuture<AssistantRuntimeTurn>,
    ): Throwable? = try {
        future.get(2, TimeUnit.SECONDS)
        null
    } catch (error: ExecutionException) {
        error.cause
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

    private fun cancelledPayload(requestId: String): JsonObject = JsonObject().apply {
        addProperty("protocol", ZARA_RUNTIME_PROTOCOL)
        addProperty("runtime_id", PROLOG_RLM_RUNTIME_ID)
        addProperty("request_id", requestId)
        addProperty("status", "cancelled")
    }

    private fun failedPayload(requestId: String): JsonObject = JsonObject().apply {
        addProperty("protocol", ZARA_RUNTIME_PROTOCOL)
        addProperty("runtime_id", PROLOG_RLM_RUNTIME_ID)
        addProperty("request_id", requestId)
        addProperty("status", "failed")
        add(
            "error",
            JsonObject().apply {
                addProperty("kind", "provider_error")
                addProperty("message", "provider rejected turn")
            },
        )
    }
}
