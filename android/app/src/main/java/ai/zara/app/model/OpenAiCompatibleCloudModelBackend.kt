package ai.zara.app.model

import java.io.BufferedReader
import java.io.IOException
import java.net.ConnectException
import java.net.HttpURLConnection
import java.net.SocketTimeoutException
import java.net.URL
import org.json.JSONArray
import org.json.JSONObject

class OpenAiCompatibleCloudModelBackend : CloudModelBackend {
    @Volatile
    private var activeConnection: HttpURLConnection? = null

    override fun generate(
        config: CloudModelConfig,
        apiKey: String?,
        request: CloudModelRequest,
        cancelled: () -> Boolean,
        onText: (String) -> Unit,
    ): CloudModelResult {
        val safe = config.validated()
        if (!safe.enabled) {
            throw CloudModelException(CloudModelFailureReason.DISABLED, "Model server is disabled")
        }
        if (safe.provider.codingOnly && request.purpose != CloudModelPurpose.CODING) {
            throw CloudModelException(
                CloudModelFailureReason.UNSUPPORTED,
                "${safe.provider.wireName} is restricted to coding requests",
            )
        }
        if (safe.provider.requiresApiKey && apiKey.isNullOrBlank()) {
            throw CloudModelException(
                CloudModelFailureReason.AUTHENTICATION,
                "Model-server API key is required",
            )
        }

        val started = System.nanoTime()
        val deadlineNanos = started + request.deadlineMs * 1_000_000L
        val endpoint = when (safe.provider) {
            CloudModelProvider.OLLAMA -> "${safe.endpoint}/api/chat"
            else -> "${safe.endpoint}/chat/completions"
        }
        val connection = openConnection(endpoint, safe, apiKey, request.deadlineMs, request.appName)
        activeConnection = connection

        return try {
            checkActive(cancelled, deadlineNanos)
            val payload = when (safe.provider) {
                CloudModelProvider.OLLAMA -> ollamaPayload(safe, request)
                else -> openAiPayload(safe, request)
            }
            connection.outputStream.use { it.write(payload.toString().encodeToByteArray()) }
            val status = connection.responseCode
            if (status !in 200..299) {
                throw classifyHttpFailure(status, readError(connection))
            }
            val text = when (safe.provider) {
                CloudModelProvider.OLLAMA -> readOllamaStream(
                    connection.inputStream.bufferedReader(),
                    cancelled,
                    deadlineNanos,
                    onText,
                )
                else -> if (connection.contentType.orEmpty().contains("text/event-stream")) {
                    readOpenAiEventStream(
                        connection.inputStream.bufferedReader(),
                        cancelled,
                        deadlineNanos,
                        onText,
                    )
                } else {
                    readOpenAiJson(
                        connection.inputStream.bufferedReader(),
                        cancelled,
                        deadlineNanos,
                        onText,
                    )
                }
            }
            if (text.isBlank()) {
                throw CloudModelException(
                    CloudModelFailureReason.INVALID_RESPONSE,
                    "Model server returned no text",
                )
            }
            CloudModelResult(
                identity = CloudModelIdentity(
                    provider = safe.provider,
                    endpoint = safe.endpoint,
                    model = safe.model,
                    appName = request.appName,
                ),
                text = text,
                elapsedMs = (System.nanoTime() - started) / 1_000_000L,
            )
        } catch (error: CloudModelException) {
            throw error
        } catch (error: SocketTimeoutException) {
            throw CloudModelException(CloudModelFailureReason.TIMEOUT, "Model server timed out", error)
        } catch (error: ConnectException) {
            throw CloudModelException(CloudModelFailureReason.UNAVAILABLE, "Model server is unavailable", error)
        } catch (error: IOException) {
            if (cancelled()) {
                throw CloudModelException(CloudModelFailureReason.CANCELLED, "Model request was cancelled", error)
            }
            throw CloudModelException(CloudModelFailureReason.UNAVAILABLE, "Model-server connection failed", error)
        } finally {
            if (activeConnection === connection) activeConnection = null
            connection.disconnect()
        }
    }

    override fun models(
        config: CloudModelConfig,
        apiKey: String?,
        cancelled: () -> Boolean,
    ): List<String> {
        val safe = config.validated()
        if (safe.provider.requiresApiKey && apiKey.isNullOrBlank()) {
            throw CloudModelException(
                CloudModelFailureReason.AUTHENTICATION,
                "Model-server API key is required",
            )
        }
        val endpoint = when (safe.provider) {
            CloudModelProvider.OLLAMA -> "${safe.endpoint}/api/tags"
            else -> "${safe.endpoint}/models"
        }
        val connection = openConnection(
            endpoint = endpoint,
            config = safe,
            apiKey = apiKey,
            deadlineMs = safe.deadlineMs,
            appName = safe.appName,
            method = "GET",
        )
        activeConnection = connection
        return try {
            if (cancelled()) {
                throw CloudModelException(CloudModelFailureReason.CANCELLED, "Model discovery was cancelled")
            }
            val status = connection.responseCode
            if (status !in 200..299) {
                throw classifyHttpFailure(status, readError(connection))
            }
            val raw = readBounded(connection.inputStream.bufferedReader(), cancelled)
            val json = JSONObject(raw)
            val names = when (safe.provider) {
                CloudModelProvider.OLLAMA -> {
                    val models = json.optJSONArray("models") ?: JSONArray()
                    buildList {
                        for (index in 0 until models.length()) {
                            val item = models.optJSONObject(index) ?: continue
                            val name = item.optString("name", item.optString("model", "")).trim()
                            if (name.isNotEmpty()) add(name)
                        }
                    }
                }
                else -> {
                    val data = json.optJSONArray("data") ?: JSONArray()
                    buildList {
                        for (index in 0 until data.length()) {
                            val id = data.optJSONObject(index)?.optString("id", "")?.trim().orEmpty()
                            if (id.isNotEmpty()) add(id)
                        }
                    }
                }
            }
            names.distinct().take(MAX_MODELS)
        } finally {
            if (activeConnection === connection) activeConnection = null
            connection.disconnect()
        }
    }

    override fun cancel() {
        activeConnection?.disconnect()
    }

    override fun close() = cancel()

    private fun openConnection(
        endpoint: String,
        config: CloudModelConfig,
        apiKey: String?,
        deadlineMs: Long,
        appName: String,
        method: String = "POST",
    ): HttpURLConnection = (URL(endpoint).openConnection() as HttpURLConnection).apply {
        requestMethod = method
        doOutput = method != "GET"
        instanceFollowRedirects = false
        connectTimeout = minOf(deadlineMs, 15_000L).toInt()
        readTimeout = deadlineMs.toInt()
        setRequestProperty("Accept", "text/event-stream, application/x-ndjson, application/json")
        setRequestProperty("User-Agent", "zara-android-model-server/1")
        if (method != "GET") setRequestProperty("Content-Type", "application/json")
        if (!apiKey.isNullOrBlank()) setRequestProperty("Authorization", "Bearer $apiKey")
        if (config.provider == CloudModelProvider.OPENROUTER) {
            setRequestProperty("X-Title", appName)
        }
    }

    private fun openAiPayload(config: CloudModelConfig, request: CloudModelRequest): JSONObject =
        JSONObject()
            .put("model", config.model)
            .put("stream", true)
            .put("max_tokens", request.maxOutputTokens)
            .put(
                "messages",
                JSONArray()
                    .put(JSONObject().put("role", "system").put("content", request.systemPrompt()))
                    .put(JSONObject().put("role", "user").put("content", request.prompt)),
            )
            .also { payload ->
                if (config.provider == CloudModelProvider.OPENROUTER) {
                    payload.put("provider", JSONObject(config.openRouterPolicy.toWireMap()))
                }
            }

    private fun ollamaPayload(config: CloudModelConfig, request: CloudModelRequest): JSONObject =
        JSONObject()
            .put("model", config.model)
            .put("stream", true)
            .put(
                "messages",
                JSONArray()
                    .put(JSONObject().put("role", "system").put("content", request.systemPrompt()))
                    .put(JSONObject().put("role", "user").put("content", request.prompt)),
            )
            .put("options", JSONObject().put("num_predict", request.maxOutputTokens))

    private fun readOpenAiEventStream(
        reader: BufferedReader,
        cancelled: () -> Boolean,
        deadlineNanos: Long,
        onText: (String) -> Unit,
    ): String {
        val output = StringBuilder()
        reader.useLines { lines ->
            for (line in lines) {
                checkActive(cancelled, deadlineNanos)
                if (!line.startsWith("data:")) continue
                val data = line.removePrefix("data:").trim()
                if (data == "[DONE]") break
                if (data.isEmpty()) continue
                val text = extractOpenAiText(parseJson(data))
                append(output, text, onText)
            }
        }
        return output.toString()
    }

    private fun readOllamaStream(
        reader: BufferedReader,
        cancelled: () -> Boolean,
        deadlineNanos: Long,
        onText: (String) -> Unit,
    ): String {
        val output = StringBuilder()
        reader.useLines { lines ->
            for (line in lines) {
                checkActive(cancelled, deadlineNanos)
                if (line.isBlank()) continue
                val json = parseJson(line)
                val text = json.optJSONObject("message")?.optString("content", "").orEmpty()
                append(output, text, onText)
                if (json.optBoolean("done", false)) break
            }
        }
        return output.toString()
    }

    private fun readOpenAiJson(
        reader: BufferedReader,
        cancelled: () -> Boolean,
        deadlineNanos: Long,
        onText: (String) -> Unit,
    ): String {
        checkActive(cancelled, deadlineNanos)
        val text = extractOpenAiText(parseJson(readBounded(reader, cancelled)))
        if (text.isNotEmpty()) onText(text)
        return text
    }

    private fun extractOpenAiText(json: JSONObject): String {
        val choices = json.optJSONArray("choices") ?: return ""
        val choice = choices.optJSONObject(0) ?: return ""
        val content = choice.optJSONObject("delta")?.opt("content")
            ?: choice.optJSONObject("message")?.opt("content")
            ?: choice.opt("text")
        return when (content) {
            is String -> content
            is JSONArray -> buildString {
                for (index in 0 until content.length()) {
                    append(content.optJSONObject(index)?.optString("text", "").orEmpty())
                }
            }
            else -> ""
        }
    }

    private fun readBounded(reader: BufferedReader, cancelled: () -> Boolean): String =
        reader.use { input ->
            buildString {
                val buffer = CharArray(DEFAULT_BUFFER_SIZE)
                while (true) {
                    if (cancelled()) {
                        throw CloudModelException(
                            CloudModelFailureReason.CANCELLED,
                            "Model request was cancelled",
                        )
                    }
                    val read = input.read(buffer)
                    if (read < 0) break
                    if (length + read > MAX_RESPONSE_CHARS) {
                        throw CloudModelException(
                            CloudModelFailureReason.INVALID_RESPONSE,
                            "Model-server response exceeded the safe limit",
                        )
                    }
                    append(buffer, 0, read)
                }
            }
        }

    private fun append(output: StringBuilder, text: String, onText: (String) -> Unit) {
        if (text.isEmpty()) return
        if (output.length + text.length > MAX_RESPONSE_CHARS) {
            throw CloudModelException(
                CloudModelFailureReason.INVALID_RESPONSE,
                "Model-server response exceeded the safe limit",
            )
        }
        output.append(text)
        onText(text)
    }

    private fun parseJson(raw: String): JSONObject = runCatching { JSONObject(raw) }.getOrElse {
        throw CloudModelException(
            CloudModelFailureReason.INVALID_RESPONSE,
            "Model server returned malformed JSON",
            it,
        )
    }

    private fun readError(connection: HttpURLConnection): String {
        val stream = connection.errorStream ?: return ""
        return stream.bufferedReader().use { it.readText().take(MAX_ERROR_CHARS) }
    }

    private fun classifyHttpFailure(status: Int, body: String): CloudModelException {
        val reason = when (status) {
            HttpURLConnection.HTTP_UNAUTHORIZED, HttpURLConnection.HTTP_FORBIDDEN ->
                CloudModelFailureReason.AUTHENTICATION
            429 -> CloudModelFailureReason.RATE_LIMIT
            HttpURLConnection.HTTP_NOT_FOUND -> CloudModelFailureReason.UNSUPPORTED
            in 400..499 -> CloudModelFailureReason.UNSUPPORTED
            else -> CloudModelFailureReason.UNAVAILABLE
        }
        val detail = body.trim().take(MAX_ERROR_DETAIL_CHARS).ifBlank { "HTTP $status" }
        return CloudModelException(reason, "Model server rejected the request: $detail")
    }

    private fun checkActive(cancelled: () -> Boolean, deadlineNanos: Long) {
        if (cancelled()) {
            throw CloudModelException(CloudModelFailureReason.CANCELLED, "Model request was cancelled")
        }
        if (System.nanoTime() >= deadlineNanos) {
            throw CloudModelException(CloudModelFailureReason.TIMEOUT, "Model request exceeded its deadline")
        }
    }

    companion object {
        private const val MAX_MODELS = 512
        private const val MAX_RESPONSE_CHARS = 2_097_152
        private const val MAX_ERROR_CHARS = 32_768
        private const val MAX_ERROR_DETAIL_CHARS = 512
    }
}
