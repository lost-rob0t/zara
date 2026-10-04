package ai.zara.app.localai

import com.sun.net.httpserver.HttpServer
import java.net.InetSocketAddress
import java.util.concurrent.TimeUnit
import org.junit.Assert.*
import org.junit.Test

class OllamaLocalAiProviderTest {
    @Test
    fun loopbackServiceDiscoversSelectsAndStreamsInstalledModel() {
        fixture { endpoint, requests ->
            OllamaLocalAiProvider(endpoint).use { provider ->
                assertEquals(listOf("gemma3:1b"), provider.modelNames().get(3, TimeUnit.SECONDS))
                val ready = provider.selectModel("gemma3:1b", "").get(3, TimeUnit.SECONDS)
                assertEquals(LocalAiPhase.READY, ready.phase)
                assertEquals("ollama", ready.providerId)
                assertEquals("gemma3:1b", ready.modelName)
                assertNull(ready.model)
                val chunks = mutableListOf<String>()
                val result = provider.generate(LocalGenerationRequest("hello", 12), chunks::add)
                    .get(3, TimeUnit.SECONDS)
                assertEquals("Hello there", result.text)
                assertEquals(listOf("Hello", " there"), chunks)
                assertEquals(LocalModelQuantization.Q4_K_M, result.quantization)
                assertEquals(LocalAiPhase.READY, provider.state().get().phase)
                val body = requests.single { it.first == "/api/generate" }.second
                assertTrue(body.contains("\"num_predict\":12"))
                assertFalse(body.contains("tools"))
                assertFalse(body.contains("http://"))
            }
        }
    }

    @Test
    fun rejectsRemoteEndpointsCredentialsRedirectTargetsAndNonHttpSchemes() {
        listOf("https://example.com", "http://localhost:11434", "http://127.0.0.1.evil:11434",
            "http://user:password@127.0.0.1:11434", "file:///tmp/ollama", "http://127.0.0.1:11434/path")
            .forEach { endpoint ->
                assertThrows(IllegalArgumentException::class.java) { OllamaLocalAiProvider(endpoint) }
            }
    }

    @Test
    fun missingModelAndDigestMismatchFailWithoutGeneration() {
        fixture { endpoint, requests ->
            OllamaLocalAiProvider(endpoint).use { provider ->
                assertThrows(Exception::class.java) { provider.selectModel("missing", "").get() }
                assertEquals(LocalAiPhase.FAILED, provider.state().get().phase)
                assertThrows(Exception::class.java) { provider.selectModel("gemma3:1b", "wrong").get() }
                assertFalse(requests.any { it.first == "/api/generate" })
                provider.selectModel("gemma3:1b", "").get()
                assertEquals(LocalAiPhase.READY, provider.state().get().phase)
            }
        }
    }

    @Test
    fun zaraLlmServeMetadataWorksWithoutUpstreamShowEndpoint() {
        fixture(inlineQuantization = true) { endpoint, requests ->
            OllamaLocalAiProvider(endpoint).use { provider ->
                provider.selectModel("gemma3:1b", "").get()
                assertEquals("Hello there", provider.generate(LocalGenerationRequest("hello")).get().text)
                assertFalse(requests.any { it.first == "/api/show" })
            }
        }
    }

    @Test
    fun malformedAndTruncatedStreamsFailAndCanRecover() {
        listOf("not-json\n", "{\"response\":\"partial\",\"done\":false}\n",
            "{\"error\":\"model unavailable\"}\n").forEach { stream ->
            fixture(stream) { endpoint, _ ->
                OllamaLocalAiProvider(endpoint).use { provider ->
                    provider.selectModel("gemma3:1b", "").get()
                    assertThrows(Exception::class.java) { provider.generate(LocalGenerationRequest("hello")).get() }
                    assertEquals(LocalAiPhase.FAILED, provider.state().get().phase)
                    provider.selectModel("gemma3:1b", "").get()
                    assertEquals(LocalAiPhase.READY, provider.state().get().phase)
                }
            }
        }
    }

    @Test
    fun redirectsAreRejectedWithoutContactingDestination() {
        val server = HttpServer.create(InetSocketAddress("127.0.0.1", 0), 0)
        server.createContext("/api/tags") { exchange ->
            exchange.responseHeaders.add("Location", "https://example.com")
            exchange.sendResponseHeaders(302, -1)
            exchange.close()
        }
        server.start()
        try {
            OllamaLocalAiProvider("http://127.0.0.1:${server.address.port}").use { provider ->
                assertThrows(Exception::class.java) { provider.modelNames().get() }
            }
        } finally { server.stop(0) }
    }

    @Test
    fun oversizedOutputIsRejectedAndFailedSelectionCannotReuseOldModel() {
        fixture("{\"response\":\"${"x".repeat(32_769)}\",\"done\":true}\n") { endpoint, requests ->
            OllamaLocalAiProvider(endpoint).use { provider ->
                provider.selectModel("gemma3:1b", "").get()
                assertThrows(Exception::class.java) { provider.generate(LocalGenerationRequest("hello")).get() }
                provider.selectModel("gemma3:1b", "").get()
                assertThrows(Exception::class.java) { provider.selectModel("missing", "").get() }
                val calls = requests.count { it.first == "/api/generate" }
                assertThrows(Exception::class.java) { provider.generate(LocalGenerationRequest("hello")).get() }
                assertEquals(calls, requests.count { it.first == "/api/generate" })
            }
        }
    }

    @Test
    fun futureCancellationStopsStreamCallbacksAndAllowsFreshSelection() {
        val firstChunk = java.util.concurrent.CountDownLatch(1)
        val finish = java.util.concurrent.CountDownLatch(1)
        val server = HttpServer.create(InetSocketAddress("127.0.0.1", 0), 0)
        server.executor = java.util.concurrent.Executors.newCachedThreadPool()
        server.createContext("/api/") { exchange ->
            exchange.requestBody.close()
            val body = when (exchange.requestURI.path) {
                "/api/tags" -> "{\"models\":[{\"name\":\"gemma3:1b\",\"digest\":\"${"a".repeat(64)}\"}]}"
                "/api/show" -> "{\"details\":{\"quantization_level\":\"Q4_K_M\"}}"
                else -> null
            }
            if (body != null) {
                val bytes = body.toByteArray()
                exchange.sendResponseHeaders(200, bytes.size.toLong())
                exchange.responseBody.use { it.write(bytes) }
            } else {
                exchange.sendResponseHeaders(200, 0)
                runCatching {
                    exchange.responseBody.use { output ->
                        output.write("{\"response\":\"first\",\"done\":false}\n".toByteArray())
                        output.flush()
                        finish.await(3, TimeUnit.SECONDS)
                        output.write("{\"response\":\"stale\",\"done\":true}\n".toByteArray())
                    }
                }
            }
        }
        server.start()
        try {
            OllamaLocalAiProvider("http://127.0.0.1:${server.address.port}").use { provider ->
                provider.selectModel("gemma3:1b", "").get()
                val chunks = java.util.Collections.synchronizedList(mutableListOf<String>())
                val generation = provider.generate(LocalGenerationRequest("hello")) { chunk ->
                    chunks.add(chunk)
                    firstChunk.countDown()
                }
                assertTrue(firstChunk.await(3, TimeUnit.SECONDS))
                generation.cancel(true)
                assertEquals(LocalAiPhase.READY, provider.state().get().phase)
                finish.countDown()
                provider.selectModel("gemma3:1b", "").get(3, TimeUnit.SECONDS)
                assertEquals(listOf("first"), chunks)
                assertEquals(LocalAiPhase.READY, provider.state().get().phase)
            }
        } finally {
            finish.countDown()
            server.stop(0)
            (server.executor as java.util.concurrent.ExecutorService).shutdownNow()
        }
    }

    private fun fixture(
        stream: String = "{\"response\":\"Hello\",\"done\":false}\n{\"response\":\" there\",\"done\":true}\n",
        inlineQuantization: Boolean = false,
        test: (String, MutableList<Pair<String, String>>) -> Unit,
    ) {
        val requests = java.util.Collections.synchronizedList(mutableListOf<Pair<String, String>>())
        val server = HttpServer.create(InetSocketAddress("127.0.0.1", 0), 0)
        server.createContext("/api/") { exchange ->
            val path = exchange.requestURI.path
            requests.add(path to exchange.requestBody.bufferedReader().readText())
            val body = when (path) {
                "/api/tags" -> "{\"models\":[{\"name\":\"gemma3:1b\",\"digest\":\"${"a".repeat(64)}\"" +
                    (if (inlineQuantization) ",\"details\":{\"quantization_level\":\"dynamic-int4\"}" else "") + "}]}"
                "/api/show" -> "{\"details\":{\"quantization_level\":\"Q4_K_M\"}}"
                else -> stream
            }.toByteArray()
            exchange.sendResponseHeaders(200, body.size.toLong())
            exchange.responseBody.use { it.write(body) }
        }
        server.start()
        try { test("http://127.0.0.1:${server.address.port}", requests) }
        finally { server.stop(0) }
    }
}
