package ai.zara.llmserve

import ai.zara.app.localai.LocalModelSpec
import org.json.JSONArray
import org.json.JSONObject
import java.io.BufferedInputStream
import java.io.BufferedOutputStream
import java.io.ByteArrayOutputStream
import java.net.InetAddress
import java.net.ServerSocket
import java.net.Socket
import java.nio.charset.StandardCharsets
import java.time.Instant
import java.util.Locale
import java.util.concurrent.ExecutorService
import java.util.concurrent.Executors
import java.util.concurrent.atomic.AtomicBoolean

class OllamaLoopbackServer(
    private val engine: LlmServeEngine,
    private val port: Int = DEFAULT_PORT,
) : AutoCloseable {
    private val running = AtomicBoolean(false)
    private val acceptor: ExecutorService = Executors.newSingleThreadExecutor { runnable ->
        Thread(runnable, "zara-llm-serve-accept").apply { isDaemon = true }
    }
    private val workers: ExecutorService = Executors.newFixedThreadPool(MAX_WORKERS) { runnable ->
        Thread(runnable, "zara-llm-serve-worker").apply { isDaemon = true }
    }

    @Volatile
    private var serverSocket: ServerSocket? = null

    fun start() {
        if (!running.compareAndSet(false, true)) return
        try {
            serverSocket = ServerSocket(
                port,
                BACKLOG,
                InetAddress.getByName(LOOPBACK_HOST),
            ).apply {
                reuseAddress = true
            }
        } catch (error: Throwable) {
            running.set(false)
            throw error
        }
        acceptor.execute(::acceptLoop)
    }

    fun isRunning(): Boolean = running.get()

    private fun acceptLoop() {
        try {
            while (running.get()) {
                val socket = serverSocket?.accept() ?: break
                workers.execute {
                    socket.use(::handle)
                }
            }
        } catch (_: Throwable) {
            if (running.get()) running.set(false)
        }
    }

    private fun handle(socket: Socket) {
        socket.soTimeout = SOCKET_TIMEOUT_MS
        val input = BufferedInputStream(socket.getInputStream())
        val output = BufferedOutputStream(socket.getOutputStream())
        try {
            route(readRequest(input), output)
        } catch (error: Throwable) {
            runCatching {
                writeJson(
                    output,
                    400,
                    JSONObject().put("error", boundedMessage(error)),
                )
            }
        } finally {
            runCatching { output.flush() }
        }
    }

    private fun route(request: HttpRequest, output: BufferedOutputStream) {
        when {
            request.method == "GET" && request.path == "/api/version" ->
                writeJson(
                    output,
                    200,
                    JSONObject()
                        .put("version", "0.3.0-alpha")
                        .put("server", "zara-llm-serve")
                        .put("compatibility", "ollama"),
                )

            request.method == "GET" && request.path == "/api/tags" -> {
                val models = JSONArray()
                engine.models().forEach { spec -> models.put(modelJson(spec)) }
                writeJson(output, 200, JSONObject().put("models", models))
            }

            request.method == "GET" && request.path == "/api/ps" -> {
                val models = JSONArray()
                engine.activeModel()?.let { spec -> models.put(modelJson(spec)) }
                writeJson(output, 200, JSONObject().put("models", models))
            }

            request.method == "POST" && request.path == "/api/chat" ->
                handleChat(request, output)

            request.method == "POST" && request.path == "/api/generate" ->
                handleGenerate(request, output)

            else -> writeJson(output, 404, JSONObject().put("error", "not found"))
        }
    }

    private fun handleChat(request: HttpRequest, output: BufferedOutputStream) {
        val payload = JSONObject(request.body)
        val requestedModel = payload.optString("model").takeIf(String::isNotBlank)
        val stream = payload.optBoolean("stream", true)
        val maxTokens = boundedMaxTokens(
            payload.optJSONObject("options")?.optInt("num_predict", 256) ?: 256
        )
        val messages = payload.optJSONArray("messages")
            ?: throw IllegalArgumentException("messages must be an array")
        require(messages.length() in 1..MAX_MESSAGES) {
            "messages count is outside the supported range"
        }

        val prompt = buildString {
            for (index in 0 until messages.length()) {
                val message = messages.getJSONObject(index)
                val role = message.optString("role").lowercase(Locale.ROOT)
                require(role == "system" || role == "user" || role == "assistant") {
                    "unsupported message role"
                }
                val content = message.optString("content")
                require(content.length <= MAX_MESSAGE_CHARS) { "message is too large" }
                append(role).append(": ").append(content).append('\n')
            }
            append("assistant:")
        }

        if (!stream) {
            val result = engine.generate(requestedModel, prompt, maxTokens)
            writeJson(
                output,
                200,
                JSONObject()
                    .put("model", result.modelId + ":" + result.modelVersion)
                    .put("created_at", Instant.now().toString())
                    .put(
                        "message",
                        JSONObject()
                            .put("role", "assistant")
                            .put("content", result.text),
                    )
                    .put("done", true),
            )
            return
        }

        writeStreamingHeaders(output)
        val result = engine.generate(requestedModel, prompt, maxTokens) { chunk ->
            synchronized(output) {
                val event = JSONObject()
                    .put("model", requestedModel.orEmpty())
                    .put("created_at", Instant.now().toString())
                    .put(
                        "message",
                        JSONObject()
                            .put("role", "assistant")
                            .put("content", chunk),
                    )
                    .put("done", false)
                output.write((event.toString() + "\n").toByteArray(StandardCharsets.UTF_8))
                output.flush()
            }
        }
        synchronized(output) {
            val done = JSONObject()
                .put("model", result.modelId + ":" + result.modelVersion)
                .put("created_at", Instant.now().toString())
                .put(
                    "message",
                    JSONObject()
                        .put("role", "assistant")
                        .put("content", ""),
                )
                .put("done", true)
            output.write((done.toString() + "\n").toByteArray(StandardCharsets.UTF_8))
        }
    }

    private fun handleGenerate(request: HttpRequest, output: BufferedOutputStream) {
        val payload = JSONObject(request.body)
        val requestedModel = payload.optString("model").takeIf(String::isNotBlank)
        val prompt = payload.optString("prompt")
        require(prompt.isNotBlank()) { "prompt is required" }
        require(prompt.length <= MAX_PROMPT_CHARS) { "prompt is too large" }
        val stream = payload.optBoolean("stream", true)
        val maxTokens = boundedMaxTokens(
            payload.optJSONObject("options")?.optInt("num_predict", 256) ?: 256
        )

        if (!stream) {
            val result = engine.generate(requestedModel, prompt, maxTokens)
            writeJson(
                output,
                200,
                JSONObject()
                    .put("model", result.modelId + ":" + result.modelVersion)
                    .put("created_at", Instant.now().toString())
                    .put("response", result.text)
                    .put("done", true),
            )
            return
        }

        writeStreamingHeaders(output)
        val result = engine.generate(requestedModel, prompt, maxTokens) { chunk ->
            synchronized(output) {
                val event = JSONObject()
                    .put("model", requestedModel.orEmpty())
                    .put("created_at", Instant.now().toString())
                    .put("response", chunk)
                    .put("done", false)
                output.write((event.toString() + "\n").toByteArray(StandardCharsets.UTF_8))
                output.flush()
            }
        }
        synchronized(output) {
            val done = JSONObject()
                .put("model", result.modelId + ":" + result.modelVersion)
                .put("created_at", Instant.now().toString())
                .put("response", "")
                .put("done", true)
            output.write((done.toString() + "\n").toByteArray(StandardCharsets.UTF_8))
        }
    }

    private fun modelJson(spec: LocalModelSpec): JSONObject =
        JSONObject()
            .put("name", spec.id + ":" + spec.version)
            .put("model", spec.id + ":" + spec.version)
            .put("digest", "sha256:" + spec.sha256)
            .put(
                "details",
                JSONObject()
                    .put("format", spec.format.wireName)
                    .put("quantization_level", spec.quantization.wireName),
            )

    private fun readRequest(input: BufferedInputStream): HttpRequest {
        val requestLine = readAsciiLine(input, MAX_REQUEST_LINE_BYTES)
            ?: throw IllegalArgumentException("missing request line")
        val parts = requestLine.split(' ')
        require(parts.size == 3) { "malformed request line" }
        val method = parts[0]
        require(method == "GET" || method == "POST") { "unsupported method" }
        val path = parts[1].substringBefore('?')
        require(path.startsWith("/") && path.length <= MAX_PATH_CHARS) { "invalid path" }
        require(parts[2] == "HTTP/1.1" || parts[2] == "HTTP/1.0") {
            "unsupported HTTP version"
        }

        val headers = linkedMapOf<String, String>()
        var headerBytes = 0
        repeat(MAX_HEADERS) {
            val line = readAsciiLine(input, MAX_HEADER_LINE_BYTES)
                ?: throw IllegalArgumentException("unexpected EOF in headers")
            if (line.isEmpty()) {
                val length = headers["content-length"]?.toIntOrNull() ?: 0
                require(length in 0..MAX_BODY_BYTES) { "request body is too large" }
                val body = ByteArray(length)
                var offset = 0
                while (offset < length) {
                    val read = input.read(body, offset, length - offset)
                    if (read < 0) throw IllegalArgumentException("truncated request body")
                    offset += read
                }
                return HttpRequest(
                    method,
                    path,
                    headers,
                    String(body, StandardCharsets.UTF_8),
                )
            }
            headerBytes += line.length
            require(headerBytes <= MAX_HEADER_BYTES) { "headers are too large" }
            val split = line.indexOf(':')
            require(split > 0) { "malformed header" }
            val name = line.substring(0, split).trim().lowercase(Locale.ROOT)
            val value = line.substring(split + 1).trim()
            require(name.isNotEmpty() && value.length <= MAX_HEADER_VALUE_CHARS) {
                "invalid header"
            }
            headers[name] = value
        }
        throw IllegalArgumentException("too many headers")
    }

    private fun readAsciiLine(input: BufferedInputStream, maxBytes: Int): String? {
        val buffer = ByteArrayOutputStream()
        while (buffer.size() <= maxBytes) {
            val value = input.read()
            if (value < 0) {
                return if (buffer.size() == 0) {
                    null
                } else {
                    buffer.toString(StandardCharsets.US_ASCII.name())
                }
            }
            if (value == '\n'.code) {
                val bytes = buffer.toByteArray()
                val end = if (
                    bytes.isNotEmpty() &&
                    bytes.last() == '\r'.code.toByte()
                ) {
                    bytes.size - 1
                } else {
                    bytes.size
                }
                return String(bytes, 0, end, StandardCharsets.US_ASCII)
            }
            buffer.write(value)
        }
        throw IllegalArgumentException("HTTP line is too long")
    }

    private fun writeJson(
        output: BufferedOutputStream,
        status: Int,
        body: JSONObject,
    ) {
        val bytes = body.toString().toByteArray(StandardCharsets.UTF_8)
        val reason = when (status) {
            200 -> "OK"
            400 -> "Bad Request"
            404 -> "Not Found"
            else -> "Error"
        }
        val headers = buildString {
            append("HTTP/1.1 ").append(status).append(' ').append(reason).append("\r\n")
            append("Content-Type: application/json\r\n")
            append("Content-Length: ").append(bytes.size).append("\r\n")
            append("Connection: close\r\n\r\n")
        }
        output.write(headers.toByteArray(StandardCharsets.US_ASCII))
        output.write(bytes)
    }

    private fun writeStreamingHeaders(output: BufferedOutputStream) {
        val headers = buildString {
            append("HTTP/1.1 200 OK\r\n")
            append("Content-Type: application/x-ndjson\r\n")
            append("Connection: close\r\n\r\n")
        }
        output.write(headers.toByteArray(StandardCharsets.US_ASCII))
        output.flush()
    }

    private fun boundedMaxTokens(value: Int): Int = value.coerceIn(1, 4096)

    private fun boundedMessage(error: Throwable): String =
        (error.message ?: error::class.java.simpleName).take(256)

    override fun close() {
        if (!running.compareAndSet(true, false)) return
        runCatching { serverSocket?.close() }
        serverSocket = null
        acceptor.shutdownNow()
        workers.shutdownNow()
    }

    data class HttpRequest(
        val method: String,
        val path: String,
        val headers: Map<String, String>,
        val body: String,
    )

    companion object {
        const val LOOPBACK_HOST = "127.0.0.1"
        const val DEFAULT_PORT = 11434

        private const val BACKLOG = 8
        private const val MAX_WORKERS = 2
        private const val SOCKET_TIMEOUT_MS = 10_000
        private const val MAX_REQUEST_LINE_BYTES = 4_096
        private const val MAX_HEADER_LINE_BYTES = 8_192
        private const val MAX_HEADER_BYTES = 32_768
        private const val MAX_HEADER_VALUE_CHARS = 4_096
        private const val MAX_HEADERS = 64
        private const val MAX_BODY_BYTES = 1024 * 1024
        private const val MAX_PATH_CHARS = 512
        private const val MAX_MESSAGES = 128
        private const val MAX_MESSAGE_CHARS = 32_768
        private const val MAX_PROMPT_CHARS = 65_536
    }
}
