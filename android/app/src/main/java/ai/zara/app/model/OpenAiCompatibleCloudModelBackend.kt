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
        apiKey: String,
        request: CloudModelRequest,
        cancelled: () -> Boolean,
        onText: (String) -> Unit,
    ): CloudModelResult {
        val safe = config.validated()
        if (!safe.enabled) {
            throw CloudModelException(CloudModelFailureReason.DISABLED, "Cloud model is disabled")
        }
        if (safe.provider.codingOnly && request.purpose != CloudModelPurpose.CODING) {
            throw CloudModelException(
                CloudModelFailureReason.UNSUPPORTED,
                "${safe.provider.wireName} is restricted to explicit coding requests",
            )
        }
        require(apiKey.isNotBlank()) { "Cloud model API key is required" }
        val started = System.nanoTime()
        val deadlineNanos = started + request.deadlineMs * 1_000_000L
        val endpoint = "${safe.endpoint}/chat/completions"
        val connection = (URL(endpoint).openConnection() as HttpURLConnection).apply {
            requestMethod = "POST"
            doOutput = true
            instanceFollowRedirects = false
            connectTimeout = minOf(request.deadlineMs, 15_000L).toInt()
            readTimeout = request.deadlineMs.toInt()
            setRequestProperty("Authorization", "Bearer $apiKey")
            setRequestProperty("Content-Type", "application/json")
            setRequestProperty("Accept", "text/event-stream, application/json")
            setRequestProperty("User-Agent", "zara-android-cloud-model/1")
            if (safe.provider == CloudModelProvider.OPENROUTER) {
                setRequestProperty("X-Title", request.appName)
            }
        }
        activeConnection = connection
        return try {
            checkActive(cancelled, deadlineNanos)
            val payload = JSONObject()
                .put("model", safe.model)
                .put("stream", true)
                .put("max_tokens", request.maxOutputTokens)
                .put(
                    "messages",
                    JSONArray()
                        .put(JSONObject().put("role", "system").put("content", request.systemPrompt()))
                        .put(JSONObject().put("role", "user").put("content", request.prompt)),
                )
            if (safe.provider == CloudModelProvider.OPENROUTER) {
                payload.put("provider", JSONObject(safe.openRouterPolicy.toWireMap()))
            }
            connection.outputStream.use { output ->
                output.write(payload.toString().encodeToByteArray())
            }
            val status = connection.responseCode
            if (status !in 200..299) {
                throw classifyHttpFailure(status, readError(connection))
            }
            val text = if (connection.contentType.orEmpty().contains("text/event-stream")) {
                readEventStream(
                    connection.inputStream.bufferedReader(),
                    cancelled,
                    deadlineNanos,
                    onText,
                )
            } else {
                readJsonResponse(
                    connection.inputStream.bufferedReader(),
                    cancelled,
                    deadlineNanos,
                    onText,
                )
            }
            if (text.isBlank()) {
                throw CloudModelException(
                    CloudModelFailureReason.INVALID_RESPONSE,
                    "Cloud model returned no text",
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
            throw CloudModelException(CloudModelFailureReason.TIMEOUT, "Cloud model timed out", error)
        } catch (error: ConnectException) {
            throw CloudModelException(CloudModelFailureReason.UNAVAILABLE, "Cloud model endpoint is unavailable", error)
        } catch (error: IOException) {
            if (cancelled()) {
                throw CloudModelException(CloudModelFailureReason.CANCELLED, "Cloud model request was cancelled", error)
            }
            throw CloudModelException(CloudModelFailureReason.UNAVAILABLE, "Cloud model connection failed", error)
        } finally {
            if (activeConnection === connection) activeConnection = null
            connection.disconnect()
        }
    }

    override fun cancel() {
        activeConnection?.disconnect()
    }

    override fun close() {
        cancel()
    }

    private fun readEventStream(
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
                val chunk = runCatching { JSONObject(data) }.getOrElse {
                    throw CloudModelException(
                        CloudModelFailureReason.INVALID_RESPONSE,
                        "Cloud model stream returned malformed JSON",
                        it,
                    )
                }
                val text = extractText(chunk)
                if (text.isNotEmpty()) {
                    appendBounded(output, text)
                    onText(text)
                }
            }
        }
        return output.toString()
    }

    private fun readJsonResponse(
        reader: BufferedReader,
        cancelled: () -> Boolean,
        deadlineNanos: Long,
        onText: (String) -> Unit,
    ): String {
        val raw = StringBuilder()
        reader.use { input ->
            val buffer = CharArray(DEFAULT_BUFFER_SIZE)
            while (true) {
                checkActive(cancelled, deadlineNanos)
                val read = input.read(buffer)
                if (read < 0) break
                if (raw.length + read > MAX_RESPONSE_CHARS) {
                    throw CloudModelException(
                        CloudModelFailureReason.INVALID_RESPONSE,
                        "Cloud model response exceeded the safe limit",
                    )
                }
                raw.append(buffer, 0, read)
            }
        }
        val text = runCatching { extractText(JSONObject(raw.toString())) }.getOrElse {
            throw CloudModelException(
                CloudModelFailureReason.INVALID_RESPONSE,
                "Cloud model returned malformed JSON",
                it,
            )
        }
        if (text.isNotEmpty()) onText(text)
        return text
    }

    private fun extractText(json: JSONObject): String {
        val choices = json.optJSONArray("choices") ?: return ""
        if (choices.length() == 0) return ""
        val choice = choices.optJSONObject(0) ?: return ""
        val delta = choice.optJSONObject("delta")
        val message = choice.optJSONObject("message")
        val content = when {
            delta != null -> delta.opt("content")
            message != null -> message.opt("content")
            else -> choice.opt("text")
        }
        return when (content) {
            is String -> content
            is JSONArray -> buildString {
                for (index in 0 until content.length()) {
                    val item = content.optJSONObject(index) ?: continue
                    append(item.optString("text", ""))
                }
            }
            else -> ""
        }
    }

    private fun appendBounded(output: StringBuilder, text: String) {
        if (output.length + text.length > MAX_RESPONSE_CHARS) {
            throw CloudModelException(
                CloudModelFailureReason.INVALID_RESPONSE,
                "Cloud model response exceeded the safe limit",
            )
        }
        output.append(text)
    }

    private fun readError(connection: HttpURLConnection): String {
        val stream = connection.errorStream ?: return ""
        return stream.bufferedReader().use { reader ->
            buildString {
                val buffer = CharArray(DEFAULT_BUFFER_SIZE)
                while (length < MAX_ERROR_CHARS) {
                    val read = reader.read(buffer, 0, minOf(buffer.size, MAX_ERROR_CHARS - length))
                    if (read < 0) break
                    append(buffer, 0, read)
                }
            }
        }
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
        return CloudModelException(reason, "Cloud model server rejected the request: $detail")
    }

    private fun checkActive(cancelled: () -> Boolean, deadlineNanos: Long) {
        if (cancelled()) {
            throw CloudModelException(CloudModelFailureReason.CANCELLED, "Cloud model request was cancelled")
        }
        if (System.nanoTime() >= deadlineNanos) {
            throw CloudModelException(CloudModelFailureReason.TIMEOUT, "Cloud model request exceeded its deadline")
        }
    }

    companion object {
        private const val MAX_RESPONSE_CHARS = 2_097_152
        private const val MAX_ERROR_CHARS = 32_768
        private const val MAX_ERROR_DETAIL_CHARS = 512
    }
}
