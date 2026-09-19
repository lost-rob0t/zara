package ai.zara.app.runtime

import com.google.gson.JsonArray
import com.google.gson.JsonObject
import com.google.gson.JsonParser
import java.io.ByteArrayOutputStream
import java.net.HttpURLConnection
import java.net.URL
import java.nio.charset.StandardCharsets
import java.util.concurrent.CompletableFuture
import java.util.concurrent.ExecutionException
import java.util.concurrent.ExecutorService
import java.util.concurrent.Executors
import java.util.concurrent.FutureTask
import java.util.concurrent.TimeUnit
import java.util.concurrent.TimeoutException
import java.util.concurrent.atomic.AtomicReference

const val ZARA_RUNTIME_PROTOCOL: String = "ZARA-RUNTIME/1"
const val EMBEDDED_LOCAL_RUNTIME_ID: String = "embedded-local"
const val PROLOG_RLM_RUNTIME_ID: String = "prolog-rlm"

data class AssistantRuntimeDescriptor(
    val id: String,
    val displayName: String,
    val runtimeVersion: String,
    val health: String,
    val locality: String,
    val transport: String,
    val profiles: List<String>,
    val supportsStreaming: Boolean,
    val supportsCancel: Boolean,
) {
    val selectable: Boolean
        get() = health == "ready" || health == "busy"
}

data class AssistantRuntimeTurn(
    val requestId: String,
    val text: String,
)

open class AssistantRuntimeException(message: String) : IllegalStateException(message)

class AssistantRuntimeCancelledException(message: String) : AssistantRuntimeException(message)

class AssistantRuntimeStaleGenerationException(message: String) : AssistantRuntimeException(message)

class AssistantRuntimeTurnFailedException(message: String) : AssistantRuntimeException(message)

class AssistantRuntimeUnavailableException(message: String) : AssistantRuntimeException(message)

fun embeddedLocalRuntimeDescriptor(): AssistantRuntimeDescriptor =
    AssistantRuntimeDescriptor(
        id = EMBEDDED_LOCAL_RUNTIME_ID,
        displayName = "Embedded Local",
        runtimeVersion = "builtin",
        health = "ready",
        locality = "embedded",
        transport = "in_process",
        profiles = emptyList(),
        supportsStreaming = false,
        supportsCancel = false,
    )

class PrologRlmSidecarClient(
    private val endpoint: String = "http://127.0.0.1:18765",
    private val discoveryTimeoutMs: Int = 350,
    private val requestTimeoutMs: Int = 30_000,
    private val requestOverride:
        ((method: String, path: String, body: JsonObject?, timeoutMs: Int) -> JsonObject)? = null,
) {
    init {
        require(endpoint == "http://127.0.0.1:18765") {
            "Android Prolog-RLM runtime is restricted to the fixed loopback sidecar"
        }
        require(discoveryTimeoutMs in 1..5_000) { "Discovery timeout is out of bounds" }
        require(requestTimeoutMs in 1..300_000) { "Request timeout is out of bounds" }
    }

    fun discover(): AssistantRuntimeDescriptor {
        val root = request(
            method = "GET",
            path = "/zara-runtime/v1/discover",
            body = null,
            timeoutMs = discoveryTimeoutMs,
        )
        val runtimes = root.getAsJsonArray("runtimes")
            ?: throw AssistantRuntimeException("Prolog-RLM discovery omitted runtimes")
        require(runtimes.size() <= MAX_RUNTIMES) { "Runtime discovery exceeds bound" }
        val matching = runtimes
            .mapNotNull { it.takeIf { value -> value.isJsonObject }?.asJsonObject }
            .map(::parseDescriptor)
            .filter { it.id == PROLOG_RLM_RUNTIME_ID }
        if (matching.size != 1) {
            throw AssistantRuntimeException("Prolog-RLM discovery identity is missing or duplicated")
        }
        return matching.single()
    }

    fun generate(
        text: String,
        requestId: String,
        conversationId: String,
        inlineContext: String? = null,
    ): AssistantRuntimeTurn {
        requireBoundedText(text, MAX_TEXT_CHARS, "Runtime turn")
        requireBoundedId(requestId, "requestId")
        requireBoundedId(conversationId, "conversationId")
        inlineContext?.let { requireBoundedText(it, MAX_CONTEXT_CHARS, "Runtime context") }

        val request = JsonObject().apply {
            addProperty("protocol", ZARA_RUNTIME_PROTOCOL)
            addProperty("request_id", requestId)
            addProperty("mode", "rlm")
            add(
                "messages",
                JsonArray().apply {
                    add(
                        JsonObject().apply {
                            addProperty("role", "user")
                            addProperty("content", text)
                        },
                    )
                },
            )
            add(
                "metadata",
                JsonObject().apply {
                    addProperty("conversation_id", conversationId)
                },
            )
            add(
                "budgets",
                JsonObject().apply {
                    addProperty("max_tokens", 512)
                    addProperty("wall_time_ms", requestTimeoutMs)
                },
            )
            inlineContext?.let { addProperty("inline_context", it) }
        }
        val reply = request(
            method = "POST",
            path = "/zara-runtime/v1/generate",
            body = request,
            timeoutMs = requestTimeoutMs,
        )
        requireWireIdentity(reply)
        when (val status = requiredString(reply, "status", 32)) {
            "completed" -> {
                val responseText = requiredString(reply, "text", MAX_TEXT_CHARS)
                return AssistantRuntimeTurn(requestId = requestId, text = responseText)
            }
            "cancelled" -> throw AssistantRuntimeCancelledException(
                "Prolog-RLM request was cancelled",
            )
            "failed" -> {
                val error = reply.getAsJsonObject("error")
                val kind = error?.let { optionalString(it, "kind", 64) } ?: "runtime_error"
                val message = error?.let { optionalString(it, "message", 512) }
                    ?: "Prolog-RLM runtime request failed"
                throw AssistantRuntimeTurnFailedException("$kind: $message")
            }
            else -> throw AssistantRuntimeUnavailableException(
                "Unsupported Prolog-RLM terminal status: $status",
            )
        }
    }

    fun cancel(requestId: String) {
        requireBoundedId(requestId, "requestId")
        val reply = request(
            method = "POST",
            path = "/zara-runtime/v1/cancel",
            body = JsonObject().apply { addProperty("request_id", requestId) },
            timeoutMs = discoveryTimeoutMs,
        )
        requireWireIdentity(reply)
        val status = requiredString(reply, "status", 32)
        if (status !in setOf("cancel_requested", "not_found")) {
            throw AssistantRuntimeException("Invalid Prolog-RLM cancellation response")
        }
    }

    private fun parseDescriptor(value: JsonObject): AssistantRuntimeDescriptor {
        if (requiredString(value, "protocol", 32) != ZARA_RUNTIME_PROTOCOL) {
            throw AssistantRuntimeException("Prolog-RLM protocol is incompatible")
        }
        val runtimeId = requiredRuntimeId(value, "id")
        val profiles = value.getAsJsonArray("profiles")?.map {
            if (!it.isJsonPrimitive || !it.asJsonPrimitive.isString) {
                throw AssistantRuntimeException("Runtime profile is invalid")
            }
            it.asString.also { profile ->
                requireBoundedText(profile, 96, "Runtime profile")
            }
        } ?: emptyList()
        if (profiles.distinct().size != profiles.size) {
            throw AssistantRuntimeException("Runtime profiles contain duplicates")
        }
        if (requiredString(value, "provider_control", 16) != "runtime") {
            throw AssistantRuntimeException("Prolog-RLM must own provider selection")
        }
        if (requiredString(value, "model_control", 16) != "runtime") {
            throw AssistantRuntimeException("Prolog-RLM must own model selection")
        }
        if (!requiredBoolean(value, "installed") || !requiredBoolean(value, "available")) {
            throw AssistantRuntimeException("Prolog-RLM is not installed and available")
        }
        return AssistantRuntimeDescriptor(
            id = runtimeId,
            displayName = requiredString(value, "display_name", 96),
            runtimeVersion = requiredString(value, "runtime_version", 64),
            health = requiredChoice(
                value,
                "health",
                setOf("starting", "ready", "busy", "degraded", "failed", "stopped"),
            ),
            locality = requiredChoice(
                value,
                "locality",
                setOf("embedded", "local_process", "local_sidecar"),
            ),
            transport = requiredChoice(
                value,
                "transport",
                setOf("in_process", "stdio", "loopback_http", "binder"),
            ),
            profiles = profiles,
            supportsStreaming = requiredBoolean(value, "supports_streaming"),
            supportsCancel = requiredBoolean(value, "supports_cancel"),
        )
    }

    private fun requireWireIdentity(value: JsonObject) {
        if (requiredString(value, "protocol", 32) != ZARA_RUNTIME_PROTOCOL) {
            throw AssistantRuntimeUnavailableException("Prolog-RLM response protocol changed")
        }
        if (requiredRuntimeId(value, "runtime_id") != PROLOG_RLM_RUNTIME_ID) {
            throw AssistantRuntimeUnavailableException("Prolog-RLM response identity changed")
        }
    }

    private fun request(
        method: String,
        path: String,
        body: JsonObject?,
        timeoutMs: Int,
    ): JsonObject {
        val connectionRef = AtomicReference<HttpURLConnection?>(null)
        val task = FutureTask<JsonObject> {
            requestOverride?.invoke(method, path, body, timeoutMs)
                ?: requestHttp(method, path, body, timeoutMs, connectionRef)
        }
        Thread(task, "zara-prolog-rlm-transport").apply { isDaemon = true }.start()
        return try {
            task.get(timeoutMs.toLong(), TimeUnit.MILLISECONDS)
        } catch (_: TimeoutException) {
            connectionRef.getAndSet(null)?.disconnect()
            task.cancel(true)
            throw AssistantRuntimeUnavailableException(
                "Prolog-RLM sidecar request exceeded total deadline",
            )
        } catch (_: InterruptedException) {
            connectionRef.getAndSet(null)?.disconnect()
            task.cancel(true)
            Thread.currentThread().interrupt()
            throw AssistantRuntimeUnavailableException("Prolog-RLM sidecar request was interrupted")
        } catch (error: ExecutionException) {
            when (val cause = error.cause) {
                is AssistantRuntimeException -> throw cause
                is Error -> throw cause
                else -> throw AssistantRuntimeUnavailableException(
                    "Prolog-RLM sidecar transport failed",
                )
            }
        }
    }

    private fun requestHttp(
        method: String,
        path: String,
        body: JsonObject?,
        timeoutMs: Int,
        connectionRef: AtomicReference<HttpURLConnection?>,
    ): JsonObject {
        val connection = URL("$endpoint$path").openConnection() as HttpURLConnection
        connectionRef.set(connection)
        try {
            connection.requestMethod = method
            connection.instanceFollowRedirects = false
            connection.useCaches = false
            connection.connectTimeout = minOf(timeoutMs, MAX_CONNECT_TIMEOUT_MS)
            connection.readTimeout = timeoutMs
            connection.setRequestProperty("Accept", "application/json")
            if (body != null) {
                val encoded = body.toString().toByteArray(StandardCharsets.UTF_8)
                if (encoded.size > MAX_REQUEST_BYTES) {
                    throw AssistantRuntimeException("Runtime request exceeds transport bound")
                }
                connection.doOutput = true
                connection.setRequestProperty("Content-Type", "application/json")
                connection.connect()
                connection.outputStream.use { output -> output.write(encoded) }
            } else {
                connection.connect()
            }
            val status = connection.responseCode
            if (status !in 200..299) {
                throw AssistantRuntimeUnavailableException(
                    "Prolog-RLM sidecar returned HTTP $status",
                )
            }
            val bytes = connection.inputStream.use(::readBounded)
            val parsed = runCatching {
                JsonParser.parseString(String(bytes, StandardCharsets.UTF_8))
            }.getOrElse {
                throw AssistantRuntimeUnavailableException("Prolog-RLM returned invalid JSON")
            }
            if (!parsed.isJsonObject) {
                throw AssistantRuntimeUnavailableException(
                    "Prolog-RLM response must be a JSON object",
                )
            }
            return parsed.asJsonObject
        } finally {
            connectionRef.compareAndSet(connection, null)
            connection.disconnect()
        }
    }

    private fun readBounded(input: java.io.InputStream): ByteArray {
        val output = ByteArrayOutputStream()
        val buffer = ByteArray(4096)
        var total = 0
        while (true) {
            val count = input.read(buffer)
            if (count < 0) break
            total += count
            if (total > MAX_RESPONSE_BYTES) {
                throw AssistantRuntimeException("Runtime response exceeds transport bound")
            }
            output.write(buffer, 0, count)
        }
        return output.toByteArray()
    }

    private companion object {
        const val MAX_RUNTIMES = 16
        const val MAX_REQUEST_BYTES = 512 * 1024
        const val MAX_RESPONSE_BYTES = 512 * 1024
        const val MAX_TEXT_CHARS = 131_072
        const val MAX_CONTEXT_CHARS = 262_144
        const val MAX_CONNECT_TIMEOUT_MS = 2_000
    }
}

class AssistantRuntimeRegistry(
    private val prologRlm: PrologRlmSidecarClient = PrologRlmSidecarClient(),
    private val executor: ExecutorService = Executors.newSingleThreadExecutor { runnable ->
        Thread(runnable, "zara-assistant-runtime").apply { isDaemon = true }
    },
    private val controlExecutor: ExecutorService = Executors.newSingleThreadExecutor { runnable ->
        Thread(runnable, "zara-assistant-runtime-control").apply { isDaemon = true }
    },
) : AutoCloseable {
    private val stateLock = Any()
    private val activePrologRequests = mutableSetOf<String>()
    private val cancelledPrologRequests = mutableSetOf<String>()
    private var generation: Long = 0L
    private var closed: Boolean = false

    @Volatile
    private var discovered: List<AssistantRuntimeDescriptor> =
        listOf(embeddedLocalRuntimeDescriptor())

    @Volatile
    private var selectedId: String = EMBEDDED_LOCAL_RUNTIME_ID

    fun discover(): CompletableFuture<List<AssistantRuntimeDescriptor>> {
        synchronized(stateLock) {
            check(!closed) { "Assistant runtime registry is closed" }
        }
        return CompletableFuture.supplyAsync(
            {
                val optional = runCatching { prologRlm.discover() }.getOrNull()
                val next = buildList {
                    add(embeddedLocalRuntimeDescriptor())
                    if (optional != null) add(optional)
                }
                synchronized(stateLock) {
                    check(!closed) { "Assistant runtime registry is closed" }
                    discovered = next
                    if (next.none { it.id == selectedId && it.selectable }) {
                        selectedId = EMBEDDED_LOCAL_RUNTIME_ID
                        generation += 1L
                    }
                    next.toList()
                }
            },
            executor,
        )
    }

    fun discovered(): List<AssistantRuntimeDescriptor> = discovered.toList()

    fun selectedRuntimeId(): String = selectedId

    fun select(runtimeId: String) {
        synchronized(stateLock) {
            check(!closed) { "Assistant runtime registry is closed" }
            require(discovered.any { it.id == runtimeId && it.selectable }) {
                "Assistant runtime is not currently installed and selectable"
            }
            if (selectedId != runtimeId) {
                selectedId = runtimeId
                generation += 1L
            }
        }
    }

    fun generatePrologRlm(
        text: String,
        requestId: String,
        conversationId: String,
        inlineContext: String? = null,
    ): CompletableFuture<AssistantRuntimeTurn> {
        val requestGeneration = synchronized(stateLock) {
            check(!closed) { "Assistant runtime registry is closed" }
            check(selectedId == PROLOG_RLM_RUNTIME_ID) {
                "Prolog-RLM is not the selected assistant runtime"
            }
            check(discovered.any { it.id == PROLOG_RLM_RUNTIME_ID && it.selectable }) {
                "Prolog-RLM is no longer an available installed runtime"
            }
            check(activePrologRequests.add(requestId)) {
                "Prolog-RLM request id is already active"
            }
            cancelledPrologRequests.remove(requestId)
            generation
        }
        return CompletableFuture.supplyAsync(
            {
                try {
                    synchronized(stateLock) {
                        if (requestId in cancelledPrologRequests) {
                            throw AssistantRuntimeCancelledException(
                                "Cancelled Prolog-RLM request cannot start generation",
                            )
                        }
                        if (
                            closed ||
                            generation != requestGeneration ||
                            selectedId != PROLOG_RLM_RUNTIME_ID ||
                            discovered.none { it.id == PROLOG_RLM_RUNTIME_ID && it.selectable }
                        ) {
                            throw AssistantRuntimeStaleGenerationException(
                                "Stale Prolog-RLM runtime generation cannot start generation",
                            )
                        }
                    }
                    val turn = prologRlm.generate(text, requestId, conversationId, inlineContext)
                    synchronized(stateLock) {
                        if (requestId in cancelledPrologRequests) {
                            throw AssistantRuntimeCancelledException(
                                "Cancelled Prolog-RLM request cannot publish a result",
                            )
                        }
                        if (
                            closed ||
                            generation != requestGeneration ||
                            selectedId != PROLOG_RLM_RUNTIME_ID ||
                            discovered.none { it.id == PROLOG_RLM_RUNTIME_ID && it.selectable }
                        ) {
                            throw AssistantRuntimeStaleGenerationException(
                                "Stale Prolog-RLM runtime generation cannot publish a result",
                            )
                        }
                    }
                    turn
                } catch (error: AssistantRuntimeCancelledException) {
                    throw error
                } catch (error: AssistantRuntimeTurnFailedException) {
                    throw error
                } catch (error: AssistantRuntimeStaleGenerationException) {
                    throw error
                } catch (error: AssistantRuntimeException) {
                    fenceUnavailablePrologRlm(requestGeneration)
                    throw error
                } finally {
                    synchronized(stateLock) {
                        activePrologRequests.remove(requestId)
                        cancelledPrologRequests.remove(requestId)
                    }
                }
            },
            executor,
        )
    }

    private fun fenceUnavailablePrologRlm(requestGeneration: Long) {
        synchronized(stateLock) {
            if (
                closed ||
                generation != requestGeneration ||
                selectedId != PROLOG_RLM_RUNTIME_ID
            ) {
                return
            }
            discovered = discovered.filterNot { it.id == PROLOG_RLM_RUNTIME_ID }
            selectedId = EMBEDDED_LOCAL_RUNTIME_ID
            generation += 1L
        }
    }

    fun cancel(requestId: String): CompletableFuture<Unit> {
        val active = synchronized(stateLock) {
            if (closed || requestId !in activePrologRequests) {
                false
            } else {
                cancelledPrologRequests.add(requestId)
                true
            }
        }
        if (!active) return CompletableFuture.completedFuture(Unit)
        return CompletableFuture.supplyAsync(
            {
                prologRlm.cancel(requestId)
            },
            controlExecutor,
        )
    }

    override fun close() {
        synchronized(stateLock) {
            if (closed) return
            closed = true
            cancelledPrologRequests.addAll(activePrologRequests)
            generation += 1L
            selectedId = EMBEDDED_LOCAL_RUNTIME_ID
            discovered = listOf(embeddedLocalRuntimeDescriptor())
        }
        controlExecutor.shutdownNow()
        executor.shutdownNow()
    }
}

private fun requiredString(value: JsonObject, key: String, maximum: Int): String {
    val element = value.get(key)
    if (element == null || !element.isJsonPrimitive || !element.asJsonPrimitive.isString) {
        throw AssistantRuntimeException("Runtime field $key is invalid")
    }
    return element.asString.also { requireBoundedText(it, maximum, "Runtime field $key") }
}

private fun optionalString(value: JsonObject, key: String, maximum: Int): String? {
    val element = value.get(key) ?: return null
    if (!element.isJsonPrimitive || !element.asJsonPrimitive.isString) return null
    return element.asString.take(maximum)
}

private fun requiredRuntimeId(value: JsonObject, key: String): String =
    requiredString(value, key, 64).also {
        if (!it.matches(Regex("[a-z0-9][a-z0-9._-]{0,63}"))) {
            throw AssistantRuntimeException("Runtime id is invalid")
        }
    }

private fun requiredBoolean(value: JsonObject, key: String): Boolean {
    val element = value.get(key)
    if (element == null || !element.isJsonPrimitive || !element.asJsonPrimitive.isBoolean) {
        throw AssistantRuntimeException("Runtime field $key must be boolean")
    }
    return element.asBoolean
}

private fun requiredChoice(
    value: JsonObject,
    key: String,
    choices: Set<String>,
): String = requiredString(value, key, 32).also {
    if (it !in choices) throw AssistantRuntimeException("Runtime field $key is unsupported")
}

private fun requireBoundedText(value: String, maximum: Int, label: String) {
    require(value.length <= maximum) { "$label exceeds bound" }
    require(value.none { it.code < 0x20 && it !in setOf('\n', '\r', '\t') }) {
        "$label contains invalid control characters"
    }
}

private fun requireBoundedId(value: String, label: String) {
    require(value.isNotBlank() && value.length <= 128) { "$label is invalid" }
    require(value.none { it.code < 0x20 }) { "$label contains control characters" }
}
