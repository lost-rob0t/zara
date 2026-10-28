package ai.zara.app.model

import com.sun.net.httpserver.HttpServer
import java.net.HttpURLConnection
import java.net.InetSocketAddress
import java.net.URL
import java.util.concurrent.atomic.AtomicBoolean
import org.junit.Assert.*
import org.junit.Test

class CloudModelConnectionTest {
    @Test fun localHttpFixtureChecksCredentialsListsModelsAndStreamsChat() {
        val server = HttpServer.create(InetSocketAddress("127.0.0.1", 0), 0)
        val authenticated = AtomicBoolean(false)
        server.createContext("/v1/models") { exchange ->
            authenticated.set(exchange.requestHeaders.getFirst("Authorization") == "Bearer fixture-key")
            val body = """{"data":[{"id":"fixture-model"},{"id":"second-model"}]}""".toByteArray()
            exchange.sendResponseHeaders(200, body.size.toLong())
            exchange.responseBody.use { it.write(body) }
        }
        server.createContext("/v1/chat/completions") { exchange ->
            exchange.requestBody.use { it.readBytes() }
            exchange.responseHeaders.add("Content-Type", "text/event-stream")
            exchange.sendResponseHeaders(200, 0)
            exchange.responseBody.use { output ->
                for (chunk in listOf("Hello ", "Android")) {
                    output.write("data: {\"choices\":[{\"delta\":{\"content\":\"$chunk\"}}]}\n\n".toByteArray())
                    output.flush()
                }
                output.write("data: [DONE]\n\n".toByteArray())
            }
        }
        server.start()
        val backend = OpenAiCompatibleCloudModelBackend { url ->
            URL("http://127.0.0.1:${server.address.port}${url.path}").openConnection() as HttpURLConnection
        }
        try {
            val config = CloudModelConfig(enabled = true, endpoint = "https://fixture.invalid/v1", model = "fixture-model")
            assertEquals(listOf("fixture-model", "second-model"), backend.models(config, "fixture-key", { false }))
            assertTrue(authenticated.get())
            val chunks = mutableListOf<String>()
            val result = backend.generate(config, "fixture-key",
                CloudModelRequest("hi", CloudModelPurpose.GENERAL, 128, 5000, "Zara"), { false }, chunks::add)
            assertEquals(listOf("Hello ", "Android"), chunks)
            assertEquals("Hello Android", result.text)
        } finally { backend.close(); server.stop(0) }
    }

    @Test fun stalledCatalogHasDeadlineAndActiveCancellation() {
        for (cancel in listOf(false, true)) {
            val started = java.util.concurrent.CountDownLatch(1)
            val release = java.util.concurrent.CountDownLatch(1)
            val server = HttpServer.create(InetSocketAddress("127.0.0.1", 0), 0)
            server.createContext("/models") { e ->
                e.sendResponseHeaders(200, 0)
                e.responseBody.use { out ->
                    out.write("{".toByteArray()); out.flush(); started.countDown()
                    release.await(3, java.util.concurrent.TimeUnit.SECONDS)
                }
            }
            server.start()
            val cancelled = AtomicBoolean(false)
            val backend = OpenAiCompatibleCloudModelBackend {
                URL("http://127.0.0.1:${server.address.port}/models").openConnection() as HttpURLConnection
            }
            try {
                val result = java.util.concurrent.CompletableFuture.supplyAsync {
                    runCatching { backend.models(CloudModelConfig(enabled = true,
                        endpoint = "https://fixture.invalid", model = "x", deadlineMs = 1000), "fixture-key", cancelled::get) }
                        .exceptionOrNull() as CloudModelException
                }
                assertTrue(started.await(2, java.util.concurrent.TimeUnit.SECONDS))
                if (cancel) { cancelled.set(true); backend.cancel() }
                assertEquals(if (cancel) CloudModelFailureReason.CANCELLED else CloudModelFailureReason.TIMEOUT,
                    result.get(3, java.util.concurrent.TimeUnit.SECONDS).reason)
            } finally { release.countDown(); backend.close(); server.stop(0) }
        }
    }

    @Test fun authRedirectMalformedCatalogAndCancellationHaveTypedFailuresWithoutBodyLeak() {
        for ((status, body, reason) in listOf(
            Triple(401, "fixture-key SECRET", CloudModelFailureReason.AUTHENTICATION),
            Triple(302, "https://wrong.invalid/", CloudModelFailureReason.UNAVAILABLE),
            Triple(200, "not-json", CloudModelFailureReason.INVALID_RESPONSE),
        )) {
            val server = HttpServer.create(InetSocketAddress("127.0.0.1", 0), 0)
            server.createContext("/models") { e ->
                e.sendResponseHeaders(status, body.length.toLong())
                e.responseBody.use { it.write(body.toByteArray()) }
            }
            server.start()
            val backend = OpenAiCompatibleCloudModelBackend {
                URL("http://127.0.0.1:${server.address.port}/models").openConnection() as HttpURLConnection
            }
            try {
                val config = CloudModelConfig(enabled = true, endpoint = "https://fixture.invalid", model = "x")
                val error = assertThrows(CloudModelException::class.java) { backend.models(config, "fixture-key", { false }) }
                assertEquals(reason, error.reason)
                assertFalse(error.message.orEmpty().contains("fixture-key"))
                assertEquals(CloudModelFailureReason.CANCELLED,
                    assertThrows(CloudModelException::class.java) { backend.models(config, "fixture-key", { true }) }.reason)
            } finally { backend.close(); server.stop(0) }
        }
    }
}
